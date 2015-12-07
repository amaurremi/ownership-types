module TypeCheck where

import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Set as Set

import AstTypes
import CollectionFuncs

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

-- type checking for assignment
-- typesMatch lhsType rhsType should succeed if rhsType is of type Null
typesMatch :: OwnershipType -> OwnershipType -> Bool
typesMatch _ NullType = True
typesMatch t t'       = t == t'


-------------------
-- Type checking --
-------------------

typeCheck :: Prog -> Either String Prog
typeCheck prog@(Prog defns varDecs expr) = do
    let (varNames, varTypes) = unzip $ varDictList varDecs
    mapM_ (checkClass prog) defns
    mapM_ (checkType prog Set.empty) varTypes
    checkExpr prog Set.empty (varDict varDecs) expr
    return prog

checkClass :: P -> Defn -> Either String ()
checkClass prog defn@(Defn n cs fs ms) = do
    let fDict = fieldDict defn
        sigma = Set.insert Owner $ newSet cs
        gamma = Map.singleton This $ OwnershipType n Owner cs
    mapM_ (checkType prog sigma) $ vals fDict
    mapM_ (checkMethod prog sigma gamma) ms

checkType :: P -> Σ -> OwnershipType -> Either String OwnershipType
checkType prog sigma o@(OwnershipType _ t ts) =
    let newSigma = newSet [Rep, NoRep] ∪ sigma
    in if (t ∈ newSigma) && (newSet ts ⊆ newSigma)
       then return o
       else Left $ "checkType error: " ++ show o
checkType _ _ UnitType                        = return UnitType
checkType _ _ NullType                        = return NullType

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
        mDict                  = methodDict $ getClass prog $ tName objT
        Sig mArgTypes mRetType = case getVal name mDict of
            Just m  -> mdvSig m
            Nothing -> error $ "method with name " ++ name ++ " not in dictionary for class " ++ tName objT
        expArgTs               = map (σ subst) mArgTypes
        staticVisibility       = all (sv obj) (mRetType : mArgTypes)
        matchingTypes          = foldl (&&) True $ zipWith typesMatch expArgTs argTs
        invocErrorInfo         = "\nreceiver expression: " ++ show obj ++ "\ncallee name: " ++ name
    if not staticVisibility
        then Left $ "static visibility check failed in invocation:" ++ invocErrorInfo
        else if not matchingTypes
            then Left $ "argument types do not match parameter types in invocation:" ++ invocErrorInfo
            else return $ σ subst mRetType

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
checkMethod prog sigma gamma (Method t n args vars e) = do
    let types    = snd . unzip . varDictList
        argTypes = types args
        varTypes = types vars
        newGamma = gamma `Map.union` (varDict args) `Map.union` (varDict vars)
    mapM_ (checkType prog sigma) $ t : argTypes ++ varTypes
    checkExpr prog sigma newGamma e
