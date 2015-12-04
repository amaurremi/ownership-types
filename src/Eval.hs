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

type Environment = State (S, Δ) Expr

newO :: S -> O
newO store = maximum (dom store) + 1

eval :: Expr -> Expr
eval e = fst $ runState (eval' e) (Map.empty, [])

eval' :: Expr -> Environment
eval' expr = case expr of
    New t            -> evalNew t
    Null             -> evalNull
    End              -> evalEnd
    Seq es           -> evalSeq es
    VarExpr v        -> evalVarExpr v
    Asgn l r         -> evalAsgn l r
    FieldRead o n    -> evalFieldRead o n
    FieldWrite o n e -> evalFieldWrite o n e
    Invoc o n args   -> evalInvoc o n args

evalNew :: OwnershipType -> Environment
evalNew = error ""

evalNull :: Environment
evalNull = error ""

evalEnd :: Environment
evalEnd = error ""

evalSeq :: [Expr] -> Environment
evalSeq es = error ""

evalVarExpr :: VarName -> Environment
evalVarExpr v = error ""

evalAsgn :: VarName -> Expr -> Environment
evalAsgn lhs rhs = error ""

evalFieldRead :: Expr ->  Name -> Environment
evalFieldRead obj name = error ""

evalFieldWrite :: Expr -> Name -> Expr -> Environment
evalFieldWrite obj name expr = error ""

evalInvoc :: Expr -> Name -> [Expr] -> Environment
evalInvoc obj method args = error ""
