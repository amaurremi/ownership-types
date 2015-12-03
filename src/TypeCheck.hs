module TypeCheck where

import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set

import AstTypes

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
sv (VarExpr This) t                   = True
sv e def = not $ Rep `elem` tOwner def : tCtxs def

-- given a concrete type, looks up its ownership scheme
-- and creates a substitution function
ψ :: Prog -> OwnershipType -> Map.Map Context Context
ψ prog (OwnershipType name c cs) =
    case getClass prog name of
        Just def ->
            Map.fromList $ zip (Owner : classCtxs def) (c : cs)
        Nothing              ->
            error $ "no class for type " ++ name

-- given a substitution function, takes a polymorphic ownership type
-- and replaces it with a concrete one
σ :: Map.Map Context Context -> OwnershipType -> OwnershipType
σ m o@(OwnershipType name c cs) =
    let contexts = do
        ctx  <- Map.lookup c m
        ctxs <- mapM (\c -> Map.lookup c m) cs
        return $ ctx : ctxs
    in case contexts of
        Just (c' : cs') -> OwnershipType name c' cs'
        Nothing         -> o

fieldDict :: Defn -> FieldDict
fieldDict = Map.fromList . map (\(Field t n) -> (n, t)) . fields

methodDict :: Defn -> MethodDict
methodDict = Map.fromList .
    map (\(Method t n args vars e) ->
        let (argNames, argTypes) = unzip $ varDictList args
        in (n, MDV (Sig argTypes t) argNames e $ varDict args)) . methods

varDictList :: [VarDec] -> [(VarName, OwnershipType)]
varDictList = map (\(VarDec vt vn) -> (VarName vn, vt))

varDict :: [VarDec] -> VarDict
varDict = Map.fromList . varDictList

getClass :: Prog -> Name -> Maybe Defn
getClass prog name =
    find (\def -> className def == name) $ defns prog

-------------------
-- Type checking --
-------------------

typeCheck :: Prog -> Either String OwnershipType
typeCheck prog@(Prog defns varDecs expr) =
    let (varNames, varTypes) = unzip $ varDictList varDecs
    in do
        mapM_ (checkClass prog) defns
        mapM_ (checkType prog Set.empty) varTypes
        checkExpr prog Set.empty (varDict varDecs) expr

checkClass :: P -> Defn -> Either String ()
checkClass prog defn@(Defn n cs fs ms) =
    let fDict = fieldDict defn
        sigma = Set.insert Owner $ Set.fromList cs
        gamma = Map.singleton This $ OwnershipType n Owner cs
    in do
        mapM_ (checkType prog sigma) $ Map.elems fDict
        mapM_ (checkMethod prog sigma gamma) ms

checkType :: P -> Σ -> OwnershipType -> Either String OwnershipType
checkType prog sigma o@(OwnershipType n t ts) =
    let newSigma = Set.fromList [Rep, NoRep] `Set.union` sigma
    in if (t `Set.member` newSigma) && (Set.fromList ts `Set.isSubsetOf` newSigma)
       then return o
       else Left $ "checkType error: " ++ show o

checkAsgn prog sigma gamma x e =
    case Map.lookup x gamma of
        Just t -> do
            t' <- checkExpr prog sigma gamma e
            if t == t'
                then return t
                else Left $ "assignment type mismatch: " ++ show t ++ " and " ++ show t'
        _      ->
            Left $ "variable " ++ show x ++ " not in scope"

checkFieldRead prog sigma gamma e fd = do
    t <- checkExpr prog sigma gamma e
    c <- case getClass prog (tName t) of
        Just cl -> return cl
        Nothing -> Left $ "no class of type " ++ show t
    case Map.lookup fd (fieldDict c) of
        Just t' ->
            if sv e t'
            then return $ σ (ψ prog t) t'
            else Left $ "static visibility check failed for expression "
                 ++ show e ++ " and type " ++ show t'
        Nothing -> Left $ "field " ++ show fd ++ " not defined in class " ++ show t

checkVarExpr gamma v =
    case Map.lookup v gamma of
        Just t  -> return t
        Nothing -> Left $ show v ++ " not in scope"

checkSeq prog sigma gamma es = do
    e' : _ <- mapM (checkExpr prog sigma gamma) $ reverse es
    return e'

checkFieldWrite prog sigma gamma r n e = do
    t  <- checkFieldRead prog sigma gamma r n
    t' <- checkExpr prog sigma gamma e
    if t == t'
        then return t
        else Left $ "type mismatch of field write " ++ show r ++ "."
            ++ show n ++ " = " ++ show e

checkInvoc prog sigma gamma obj name args = do
    objT  <- checkExpr prog sigma gamma obj
    argTs <- mapM (checkExpr prog sigma gamma) args
    let subst                  = ψ prog objT
        mDict                  = methodDict $ fromJust $ getClass prog $ tName objT
        Sig mArgTypes mRetType = case Map.lookup name mDict of
            Just m  -> mdvSig m
            Nothing -> error $ "method with name " ++ name ++ " not in dictionary for class " ++ tName objT
        expArgTs               = map (σ subst) mArgTypes
        in if (all (sv obj) (mRetType : mArgTypes)) &&
              (expArgTs == argTs)
           then return $ σ subst mRetType
           else Left $ "invoc type error: " ++ show obj ++ "." ++ name

checkExpr :: P -> Σ -> Γ -> Expr -> Either String OwnershipType
checkExpr prog sigma gamma e = case e of
    New t             ->
        checkType prog sigma t
    Null              ->
        error "???" -- todo don't do type checking for null
    Seq es            ->
        checkSeq prog sigma gamma es
    VarExpr v         ->
        checkVarExpr gamma v
    Asgn This _       ->
        Left "assignment to `this`"
    Asgn x e'         ->
        checkAsgn prog sigma gamma x e'
    FieldRead e' fd    ->
        checkFieldRead prog sigma gamma e' fd
    FieldWrite r n e' ->
        checkFieldWrite prog sigma gamma r n e'
    Invoc r n args      -> checkInvoc prog sigma gamma r n args

checkMethod :: P -> Σ -> Γ -> Method -> Either String OwnershipType
checkMethod prog sigma gamma (Method t n args vars e) =
    let types    = snd . unzip . varDictList
        argTypes = types args
        varTypes = types vars
        newGamma = gamma `Map.union` (varDict args) `Map.union` (varDict vars)
    in do
        mapM_ (checkType prog sigma) $ t : argTypes ++ varTypes
        checkExpr prog sigma newGamma e
