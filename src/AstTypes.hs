module AstTypes where

data Defn = Defn Name [Context] [Field] [Method]
    deriving (Eq, Show)

data Field = Field OwnershipType Name
    deriving (Eq, Show)

data Method = Method OwnershipType Name [VarDec] [VarDec] Expr
    deriving (Eq, Show)

data VarName = VarName Name
             | This
    deriving (Eq, Show)

data VarDec = VarDec OwnershipType Name
    deriving (Eq, Show)

type Name = String

data Expr = New OwnershipType
          | Null
          | Seq [Expr]
          | VarExpr VarName
          | Asgn VarName Expr
          | FieldRead Expr Name
          | FieldWrite Expr Name Expr
          | Invoc Expr Name [Expr]
    deriving (Eq, Show)

data Prog = Prog [Defn] [VarDec] Expr
    deriving (Show)

data OwnershipType = OwnershipType Name Context [Context]
    deriving (Eq, Show)

data Context = Context Name
             | Rep
             | NoRep
             | Owner
    deriving (Eq, Show)