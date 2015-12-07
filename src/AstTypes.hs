module AstTypes where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (find, intercalate)

import CollectionFuncs

---------------
-- AST types --
---------------

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

data OwnershipType = OwnershipType {
                         tName  :: Name,
                         tOwner :: Context,
                         tCtxs  :: [Context] }
                   | NullType
                   | UnitType
    deriving (Eq, Ord)

instance Show OwnershipType where
    show NullType = "null-type"
    show UnitType = "Unit"
    show (OwnershipType name c cs) = name ++ " <" ++ show c ++ showCs cs ++ ">"
        where showCs :: [Context] -> String
              showCs [] = ""
              showCs cs = " | " ++ (intercalate ", " $ map show cs)


data Context = Context { ctx :: Name }
             | Rep
             | NoRep
             | Owner
    deriving (Eq, Ord)

instance Show Context where
    show Rep         = "rep"
    show NoRep       = "norep"
    show Owner       = "owner"
    show (Context n) = n

-- method signature type: return type + parameter types
data Sig = Sig {
    sigArgTypes :: [OwnershipType],
    sigRetType  :: OwnershipType
}

-------------------
-- AST traversal --
-------------------

type VarDict = Map.Map VarName OwnershipType

type FieldDict = Map.Map Name OwnershipType

data MethodDictVal = MDV {
    mdvSig     :: Sig,
    mdvArgs    :: [VarName],
    mdvExpr    :: Expr,
    mdvVarDict :: VarDict
}

type MethodDict = Map.Map Name MethodDictVal

fieldDict :: Defn -> FieldDict
fieldDict = newMap . map (\(Field t n) -> (n, t)) . fields

methodDict :: Defn -> MethodDict
methodDict = newMap .
    map (\(Method t n args vars e) ->
        let (argNames, argTypes) = unzip $ varDictList args
        in (n, MDV (Sig argTypes t) argNames e $ varDict args)) . methods

varDictList :: [VarDec] -> [(VarName, OwnershipType)]
varDictList = map (\(VarDec vt vn) -> (VarName vn, vt))

varDict :: [VarDec] -> VarDict
varDict = newMap . varDictList

getClass :: Prog -> Name -> Defn
getClass prog name =
    let defs = defns prog
    in case find (\def -> className def == name) defs of
        Just defn -> defn
        Nothing   -> error $ "no class of type " ++ name ++ "\nAvailable classes: " ++ unwords (map className defs)
