module Eval (eval) where

import Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Set as Set

import AstTypes
import CollectionFuncs
import State

doCollect = True

-- object identifier
data O = O { oRef :: Int, oType :: OwnershipType }
    deriving (Eq, Ord)

instance Show O where
    show (O oref otype) = "ref address: " ++ show oref ++ ", type: " ++ show otype

-- an object identifier or null
data Value = Val O
           | ValNull
    deriving (Eq)

instance Show Value where
    show ValNull = "null"
    show (Val o) = show o

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
             sticky :: Sticky }
    deriving (Eq)

instance Show F where
    show (F f s) = "field map: " ++ showMapField f ++ ", " ++ show s
        where showMapField m = foldl (\s (name, ref) -> "\n   " ++ name ++ " -> " ++ show ref) "" $ Map.toList m

data Sticky = NewObject | NotStickyVar | NotStickyField | Sticky
    deriving (Eq, Ord)

instance Show Sticky where
    show s = case s of
        NewObject      -> "stickiness 0"
        NotStickyVar   -> "not sticky variable (1)"
        NotStickyField -> "not sticky field (1)"
        Sticky         -> "sticky (>1)"

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
        f : fs -> do
            freeObjects f
            putStack fs

freeObjects :: StackFrame -> RedState ()
freeObjects (StackFrame _ stackVal) = when doCollect $ mapM_ (freeObject False) $ Map.elems stackVal
    where freeObject _ ValNull = return ()
          freeObject isChild (Val o) = do
            s <- getStore
            let (F fs sticky) = getFromStore o s
            when (sticky <= NotStickyVar || isChild && isRep o) $ putStore $ Map.delete o s
            mapM_ (freeObject True) $ vals fs
          isRep (O _ (OwnershipType _ c cs)) = Rep `elem` c : cs

pushStackFrame :: StackFrame -> RedState ()
pushStackFrame f = do
    δ <- getStack
    putStack $ f : δ

getFromStore :: O -> S -> F
getFromStore o = fromMaybe ("object " ++ show o ++ " is not in the store") . getVal o

-- increase the stickiness of an object
makeSticky :: Value -> Bool -> RedState ()
makeSticky ValNull _           = return ()
makeSticky (Val o) assignToVar = do
    s <- getStore
    let (F f sticky) = getFromStore o s
        newSticky    = case sticky of
            NewObject -> if assignToVar then NotStickyVar else NotStickyField
            _         -> Sticky
    putStore $ Map.insert o (F f newSticky) s

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
        frame  = createStackFrame (O 0 NullType) [] [] $ locals
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
        fields = dom $ fieldDict (getClass prog (tName t))
        fieldMap = newMap [(f, ValNull) | f <- Set.toList fields]
    putStore $ Map.union s $ Map.singleton o $ F fieldMap NewObject
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
    makeSticky o True
    return o

evalFieldRead :: Prog -> Expr ->  Name -> Environment
evalFieldRead prog obj name = do
    o <- evalExpr prog obj
    case o of
        ValNull -> error $ "attempt to read field " ++ name ++ " on null receiver"
        Val o'  -> do
            s <- getStore
            let (F f _) = getFromStore o' s
                v = fromMaybe ("object " ++ show o' ++ " does not contain field " ++ name) $ getVal name f
            return v

evalFieldWrite :: Prog -> Expr -> Name -> Expr -> Environment
evalFieldWrite prog obj name expr = do
        o <- evalExpr prog obj
        case o of
            ValNull -> error $ "field write on null object; field name " ++ name
            Val o' -> do
                v <- evalExpr prog expr
                s <- getStore
                let (F f sticky) = getFromStore o' s
                putStore $ Map.insert o' (F (Map.insert name v f) $ sticky) s
                makeSticky v False
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
