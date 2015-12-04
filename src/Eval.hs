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

putStack :: Δ -> RedState ()
putStack δ = do
    (s, _) <- get
    put (s, δ)

---------------------
-- Reduction rules --
---------------------

eval :: Prog -> F
eval e = let (o, (s, _)) = runState (eval' e) (Map.empty, [])
         in case getVal o s of
            Just f  -> f
            Nothing -> error $ "object " ++ show o ++ " is not in the store"

eval' :: Prog -> Environment
eval' prog = case progExpr prog of
    New t            -> evalNew prog t
    Null             -> evalNull
    End              -> evalEnd
    Seq es           -> evalSeq es
    VarExpr v        -> evalVarExpr v
    Asgn l r         -> evalAsgn l r
    FieldRead o n    -> evalFieldRead o n
    FieldWrite o n e -> evalFieldWrite o n e
    Invoc o n args   -> evalInvoc o n args

evalNew :: Prog -> OwnershipType -> Environment
evalNew prog t = do
    (s, δ) <- get
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
    (s, δ) <- get
    return $ error ""

evalAsgn :: VarName -> Expr -> Environment
evalAsgn lhs rhs = do
    (s, δ) <- get
    return $ error ""

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
