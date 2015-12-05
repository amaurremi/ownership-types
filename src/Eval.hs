module Eval (eval) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AstTypes
import CollectionFuncs
import State

-- object identifier
data O = O { oRef :: Int, oType :: OwnershipType }
    deriving (Eq, Ord, Show)

-- an object identifier or null
data Value = Val O
           | ValNull
    deriving (Eq, Show)

-- object (map from field names to values)
-- when an object is created its ``stickiness'' is set to 0;
-- the stickiness of an object is an overapproximation of
-- the number of references to it.
-- an object can be freed if its stickiness is <= 1, and if
-- the only reference to the object is a variable that is popped
-- from the stack.
-- if the object is stickier we can only free it if the object's
-- owner context is `rep` and the object's owner was freed.
data F = F { fieldMap :: Map.Map Name Value,
             sticky :: Int }
    deriving (Eq, Show)

-- a stack frame
data StackFrame = StackFrame { thisVal  :: O,
                               stackVal :: Map.Map Name Value }
    deriving (Eq, Show)

-- a stack
type Δ = [StackFrame]

-- a store
type S = Map.Map O F

type RedState    = State (S, Δ)

type Environment = RedState Value

newO :: S -> OwnershipType -> O
newO store = case map oRef $ Set.toList $ dom store of
    []   -> O 1
    refs -> O (1 + maximum refs)

putStore :: S -> RedState ()
putStore s = do
    (_, δ) <- get
    put (s, δ)

getStore :: RedState S
getStore = do
    (s, _) <- get
    return s

putStack :: Δ -> RedState ()
putStack δ = do
    (s, _) <- get
    put (s, δ)

getStack :: RedState Δ
getStack = do
    (_, δ) <- get
    return δ

getValFromStack :: VarName -> Δ -> Value
getValFromStack _ []      = error "empty stack"
getValFromStack v (δ : _) = case v of
    This      -> Val $ thisVal δ
    VarName n -> fromMaybe ("identifier " ++ n ++ " not on stack") $ getVal n $ stackVal δ

pushOnStack :: VarName -> Value -> Δ -> Δ
pushOnStack _ _ []       = error "no stack frame"
pushOnStack v o ((StackFrame t s) : δs) = case v of
    This      -> error "assignment to this"
    VarName n -> StackFrame t (Map.insert n o s) : δs

popStackFrame :: RedState ()
popStackFrame = do
    δ <- getStack
    case δ of
        []     -> error "attempt to pop stack frame from empty stack"
        _ : fs -> putStack fs

pushStackFrame :: StackFrame -> RedState ()
pushStackFrame f = do
    δ <- getStack
    putStack $ f : δ

-- increase the stickiness of an object
makeSticky :: Value -> RedState ()
makeSticky ValNull = return ()
makeSticky (Val o) = do
    s <- getStore
    let (F f sticky) = fromMaybe ("object " ++ show o ++ " is not in the store") $ getVal o s
    putStore $ Map.insert o (F f $ sticky + 1) s

---------------------
-- Reduction rules --
---------------------

-- reduces the program to an expression;
-- returns the resulting object (map from fields to values) and the states of the stack and store
eval :: Prog -> S
eval p = fst $ snd $ runState (evalProg p) (Map.empty, [])

evalProg :: Prog -> Environment
evalProg prog = do
    let locals = newMap $ map (\(VarDec t n) -> (VarName n, t)) $ progVarDecs prog
    let frame  = createStackFrame (O 0 NullType) [] [] $ locals
    pushStackFrame frame
    result <- evalExpr prog $ progExpr prog
    popStackFrame
    return result

evalExpr :: Prog -> Expr -> Environment
evalExpr prog expr = case expr of
    New t            -> evalNew prog t
    Null             -> evalNull
    End              -> evalEnd
    Seq es           -> evalSeq prog es
    VarExpr v        -> evalVarExpr v
    Asgn l r         -> evalAsgn prog l r
    FieldRead o n    -> evalFieldRead prog o n
    FieldWrite o n e -> evalFieldWrite prog o n e
    Invoc o n args   -> evalInvoc prog o n args

evalNew :: Prog -> OwnershipType -> Environment
evalNew prog t = do
    s <- getStore
    let o = newO s t
    let fields = dom $ fieldDict (getClass prog (tName t))
    let fieldMap = newMap [(f, ValNull) | f <- Set.toList fields]
    putStore $ Map.union s $ Map.singleton o $ F fieldMap 0
    return $ Val o

evalNull :: Environment
evalNull = return ValNull

evalEnd :: Environment
evalEnd = return ValNull

evalSeq :: Prog -> [Expr] -> Environment
evalSeq prog es = do
     es' <- mapM (evalExpr prog) es
     return $ last es'

evalVarExpr :: VarName -> Environment
evalVarExpr v = do
    δ <- getStack
    return $ getValFromStack v δ

evalAsgn :: Prog -> VarName -> Expr -> Environment
evalAsgn prog lhs rhs = do
    o <- evalExpr prog rhs
    δ <- getStack
    putStack $ pushOnStack lhs o δ
    makeSticky o
    return o

evalFieldRead :: Prog -> Expr ->  Name -> Environment
evalFieldRead prog obj name = do
    o <- evalExpr prog obj
    case o of
        ValNull -> error $ "attempt to read field " ++ name ++ " on null receiver"
        Val o'  -> do
            s <- getStore
            let (F f _) = fromMaybe ("store does not contain object " ++ show o') $ getVal o' s
            let v = fromMaybe ("object " ++ show o' ++ " does not contain field " ++ name) $ getVal name f
            return v

evalFieldWrite :: Prog -> Expr -> Name -> Expr -> Environment
evalFieldWrite prog obj name expr = do
        o <- evalExpr prog obj
        case o of
            ValNull -> error $ "field write on null object; field name " ++ name
            Val o' -> do
                v <- evalExpr prog expr
                s <- getStore
                let (F f sticky) = fromMaybe (error $ "object " ++ show o' ++ " not in the store") $ getVal o' s
                putStore $ Map.insert o' (F (Map.insert name v f) $ sticky + 1) s
                return v

evalInvoc :: Prog -> Expr -> Name -> [Expr] -> Environment
evalInvoc prog e md es = do
    o <- evalExpr prog e
    case o of
        ValNull -> error $ "invocation of " ++ md ++ " method on null object"
        Val o'  -> do
            vs       <- mapM (evalExpr prog) es
            let className                 = tName $ oType o'
                mDict                     = methodDict $ getClass prog className
                noMethodMsg               = "class " ++ className ++ " does not contain method " ++ md
                (MDV _ params body vDict) = fromMaybe noMethodMsg $ getVal md mDict
                newStackFrame             = createStackFrame o' params vs vDict
                in do
                    pushStackFrame newStackFrame
                    v' <- evalExpr prog body
                    popStackFrame
                    return v'

createStackFrame this params vs vDict =
    let paramNames = map vName $ filter (/= This) params
        argsToVals = newMap $ paramNames `zip` vs
        locMap     = Map.mapKeys vName $ Map.filterWithKey (\k _ -> k /= This) vDict
        locsToNull = Map.map (\_ -> ValNull) locMap
    in StackFrame this $ Map.union argsToVals locsToNull