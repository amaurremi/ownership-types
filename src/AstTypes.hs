module AstTypes where

data Defn = Defn {
    className :: Name,
    classCtxs :: [Context],
    fields    :: [Field],
    methods   :: [Method]
} deriving (Eq, Show)

data Field = Field {
    fdType :: OwnershipType,
    fdName :: Name
} deriving (Eq, Show)

data Method = Method {
    mType   :: OwnershipType,
    mName   :: Name,
    mArgs   :: [VarDec],
    mLocals :: [VarDec],
    mExpr   :: Expr
} deriving (Eq, Show)

data VarName = VarName { vName :: Name }
             | This
    deriving (Eq, Ord, Show)

data VarDec = VarDec {
    vDecType :: OwnershipType,
    vDecName :: Name
} deriving (Eq, Show)

type Name = String

data Expr = New        { newType   :: OwnershipType }
          | Null
          | End
          | Seq        { seqExprs  :: [Expr] }
          | VarExpr    { vExprName :: VarName }
          | Asgn       { lhs       :: VarName,
                         rhs       :: Expr }
          | FieldRead  { fdRObj    :: Expr,
                         fdRName   :: Name }
          | FieldWrite { fdWObj    :: Expr,
                         fdWName   :: Name,
                         fdWExpr   :: Expr }
          | Invoc      { invocObj  :: Expr,
                         invocName :: Name,
                         invocArgs :: [Expr] }
    deriving (Eq, Show)

data Prog = Prog {
    defns       :: [Defn],
    progVarDecs :: [VarDec],
    progExpr    :: Expr
} deriving (Show)

data OwnershipType =
                     OwnershipType {
                         tName  :: Name,
                         tOwner :: Context,
                         tCtxs  :: [Context]
                     }
                   | NullType
                   | UnitType
    deriving (Eq, Ord, Show)

data Context = Context { ctx :: Name }
             | Rep
             | NoRep
             | Owner
    deriving (Eq, Ord, Show)