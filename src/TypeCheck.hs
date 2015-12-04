module TypeCheck where

import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Set as Set

import AstTypes
import CollectionHelpers

-- method signature type: return type + parameter types
data Sig = Sig {
    sigArgTypes :: [OwnershipType],
    sigRetType  :: OwnershipType
}

type VarDict = Map.Map VarName OwnershipType
type FieldDict = Map.Map Name OwnershipType
data MethodDictVal = MDV {
    mdvSig :: Sig,
    mdvArgs :: [VarName],
    mdvExpr :: Expr,
    mdvVarDict :: VarDict
}
type MethodDict = Map.Map Name MethodDictVal

type P = Prog
type Σ = Set.Set Context
type Γ = Map.Map VarName OwnershipType

----------------------
-- Helper functions --
----------------------

-- static visibility
sv :: Expr -> OwnershipType -> Bool
sv (VarExpr This) t         = True
sv e (OwnershipType _ c cs) = not $ Rep `elem` c : cs
sv _ UnitType               = True
sv _ NullType               = True

-- given a concrete type, looks up its ownership scheme
-- and creates a substitution function
ψ :: Prog -> OwnershipType -> Map.Map Context Context
ψ prog (OwnershipType name c cs) =
    newMap $ zip (Owner : (classCtxs $ getClass prog name)) (c : cs)

-- given a substitution function, takes a polymorphic ownership type
-- and replaces it with a concrete one
σ :: Map.Map Context Context -> OwnershipType -> OwnershipType
σ m o@(OwnershipType name c cs) =
    let contexts = do
        ctx  <- getVal c m
        ctxs <- mapM (\c -> getVal c m) cs
        return $ ctx : ctxs
    in case contexts of
        Just (c' : cs') -> OwnershipType name c' cs'
        Nothing         -> o
σ _ UnitType = UnitType
σ _ NullType = NullType

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

-- type checking for assignment
-- typesMatch lhsType rhsType should succeed if rhsType is of type Null
typesMatch :: OwnershipType -> OwnershipType -> Bool
typesMatch _ NullType = True
typesMatch t t'       = t == t'


-------------------
-- Type checking --
-------------------

typeCheck :: Prog -> Either String OwnershipType
typeCheck prog@(Prog defns varDecs expr) = do
    let (varNames, varTypes) = unzip $ varDictList varDecs
    mapM_ (checkClass prog) defns
    mapM_ (checkType prog Set.empty) varTypes
    checkExpr prog Set.empty (varDict varDecs) expr

checkClass :: P -> Defn -> Either String ()
checkClass prog defn@(Defn n cs fs ms) =
    let fDict = fieldDict defn
        sigma = Set.insert Owner $ newSet cs
        gamma = Map.singleton This $ OwnershipType n Owner cs
    in do
        mapM_ (checkType prog sigma) $ vals fDict
        mapM_ (checkMethod prog sigma gamma) ms

checkType :: P -> Σ -> OwnershipType -> Either String OwnershipType
checkType prog sigma o@(OwnershipType _ t ts) =
    let newSigma = newSet [Rep, NoRep] `Set.union` sigma
    in if (t `Set.member` newSigma) && (newSet ts `Set.isSubsetOf` newSigma)
       then return o
       else Left $ "checkType error: " ++ show o
checkType _ _ UnitType                        =
    return UnitType
checkType _ _ NullType                        =
    return NullType

checkAsgn prog sigma gamma x e =
    case getVal x gamma of
        Just t -> do
            t' <- checkExpr prog sigma gamma e
            if typesMatch t t'
                then return t
                else Left $ "assignment type mismatch: " ++ show t ++ " and " ++ show t'
        _      ->
            Left $ "variable " ++ show x ++ " not in scope"

checkFieldRead prog sigma gamma e fd = do
    t <- checkExpr prog sigma gamma e
    let c = getClass prog $ tName t
    case getVal fd (fieldDict c) of
        Just t' ->
            if sv e t'
            then return $ σ (ψ prog t) t'
            else Left $ "static visibility check failed for expression "
                 ++ show e ++ " and type " ++ show t'
        Nothing -> Left $ "field " ++ show fd ++ " not defined in class " ++ show t

checkVarExpr gamma v =
    case getVal v gamma of
        Just t  -> return t
        Nothing -> Left $ show v ++ " not in scope"

checkSeq prog sigma gamma es = do
    e' : _ <- mapM (checkExpr prog sigma gamma) $ reverse es
    return e'

checkFieldWrite prog sigma gamma r n e = do
    t  <- checkFieldRead prog sigma gamma r n
    t' <- checkExpr prog sigma gamma e
    if typesMatch t t'
        then return t
        else Left $ "type mismatch of field write " ++ show r ++ "."
            ++ show n ++ " = " ++ show e

checkInvoc prog sigma gamma obj name args = do
    objT  <- checkExpr prog sigma gamma obj
    argTs <- mapM (checkExpr prog sigma gamma) args
    let subst                  = ψ prog objT
    let mDict                  = methodDict $ getClass prog $ tName objT
    let Sig mArgTypes mRetType = case getVal name mDict of
            Just m  -> mdvSig m
            Nothing -> error $ "method with name " ++ name ++ " not in dictionary for class " ++ tName objT
    let expArgTs               = map (σ subst) mArgTypes
    if (all (sv obj) (mRetType : mArgTypes)) && (foldl (&&) True $ zipWith typesMatch expArgTs argTs)
        then return $ σ subst mRetType
        else Left $ "invoc type error: " ++ show obj ++ "." ++ name

checkExpr :: P -> Σ -> Γ -> Expr -> Either String OwnershipType
checkExpr prog sigma gamma e = case e of
    New t             ->
        checkType prog sigma t
    Null              ->
        return NullType
    Seq es            ->
        checkSeq prog sigma gamma es
    VarExpr v         ->
        checkVarExpr gamma v
    Asgn This _       ->
        Left "assignment to `this`"
    Asgn x e'         ->
        checkAsgn prog sigma gamma x e'
    FieldRead e' fd   ->
        checkFieldRead prog sigma gamma e' fd
    FieldWrite r n e' ->
        checkFieldWrite prog sigma gamma r n e'
    Invoc r n args    ->
        checkInvoc prog sigma gamma r n args
    End               ->
        return UnitType

checkMethod :: P -> Σ -> Γ -> Method -> Either String OwnershipType
checkMethod prog sigma gamma (Method t n args vars e) =
    let types    = snd . unzip . varDictList
        argTypes = types args
        varTypes = types vars
        newGamma = gamma `Map.union` (varDict args) `Map.union` (varDict vars)
    in do
        mapM_ (checkType prog sigma) $ t : argTypes ++ varTypes
        checkExpr prog sigma newGamma e
