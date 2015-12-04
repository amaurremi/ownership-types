module Eval (eval) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AstTypes
import CollectionFuncs
import State

-- object identifier
type O = Int
-- object (map from field names to values)
type F = Map.Map Name Expr

-- a stack frame
data StackFrame = StackFrame { thisVal  :: O,
                               stackVal :: Map.Map Name O }

-- a stack
type Δ = [StackFrame]

-- a store
type S = Map.Map O F

type RedState    = State (S, Δ)
type Environment = RedState O

newO :: S -> O
newO store = maximum (dom store) + 1

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

getValFromStack :: VarName -> Δ -> O
getValFromStack _ []      = error "empty stack"
getValFromStack v (δ : _) = case v of
    This      -> thisVal δ
    VarName n -> fromMaybe ("identifier " ++ n ++ " not on stack") $ getVal n $ stackVal δ

pushOnStack :: VarName -> O -> Δ -> Δ
pushOnStack _ _ []       = error "no stack frame"
pushOnStack v o ((StackFrame t s) : δs) = case v of
    This      -> StackFrame o s : δs
    VarName n -> StackFrame t (Map.insert n o s) : δs

---------------------
-- Reduction rules --
---------------------

eval :: Prog -> F
eval p = let (o, (s, _)) = runState (evalExpr p $ progExpr p) (Map.empty, [])
         in fromMaybe ("object " ++ show o ++ " is not in the store") $ getVal o s

evalExpr :: Prog -> Expr -> Environment
evalExpr prog expr = case expr of
    New t            -> evalNew prog t
    Null             -> evalNull
    End              -> evalEnd
    Seq es           -> evalSeq es
    VarExpr v        -> evalVarExpr v
    Asgn l r         -> evalAsgn prog l r
    FieldRead o n    -> evalFieldRead o n
    FieldWrite o n e -> evalFieldWrite o n e
    Invoc o n args   -> evalInvoc o n args

evalNew :: Prog -> OwnershipType -> Environment
evalNew prog t = do
    s <- getStore
    let o = newO s
    let fields = dom $ fieldDict (getClass prog (tName t))
    putStore $ Map.union s $ Map.singleton o $ newMap [(f, Null) | f <- Set.toList fields]
    return o

evalNull :: Environment
evalNull = do
    (s, δ) <- get
    return $ error ""

evalEnd :: Environment
evalEnd = do
     (s, δ) <- get
     return $ error ""

evalSeq :: [Expr] -> Environment
evalSeq es = do
     (s, δ) <- get
     return $ error ""

evalVarExpr :: VarName -> Environment
evalVarExpr v = do
    δ <- getStack
    return $ getValFromStack v δ

evalAsgn :: Prog -> VarName -> Expr -> Environment
evalAsgn prog lhs rhs = do
    e <- evalExpr prog rhs
    δ <- getStack
    putStack $ pushOnStack lhs e δ
    return e

evalFieldRead :: Expr ->  Name -> Environment
evalFieldRead obj name = do
     (s, δ) <- get
     return $ error ""

evalFieldWrite :: Expr -> Name -> Expr -> Environment
evalFieldWrite obj name expr = do
    (s, δ) <- get
    return $ error ""

evalInvoc :: Expr -> Name -> [Expr] -> Environment
evalInvoc obj method args = do
    (s, δ) <- get
    return $ error ""
