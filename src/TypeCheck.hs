module TypeCheck where

import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Set as Set

import AstTypes

-- method signature type: return type + parameter types
type Sig = ([OwnershipType], OwnershipType)

type VarDict = Map.Map VarName OwnershipType
type FieldDict = Map.Map Name OwnershipType
type MethodDict = Map.Map Name (Sig, [VarName], Expr, VarDict)

type P = Prog
type Σ = Set.Set Context
type Γ = Map.Map VarName OwnershipType

-- static visibility
sv :: Expr -> OwnershipType -> Bool
sv (VarExpr This) t                   = True
sv e (OwnershipType _ c cs) = not $ Rep `elem` c : cs

-- substitution
--σ =
--ψ =

fieldDict :: Defn -> FieldDict
fieldDict (Defn _ _ fields _) = Map.fromList $ map (\(Field t n) -> (n, t)) fields

methodDict :: Defn -> MethodDict
methodDict (Defn _ _ _ methods) = Map.fromList $
    map (\(Method t n args vars e) ->
        let (argNames, argTypes) = unzip $ varDictList args
        in (n, ((argTypes, t), argNames, e, varDict args))
    ) methods

varDictList :: [VarDec] -> [(VarName, OwnershipType)]
varDictList = map (\(VarDec vt vn) -> (VarName vn, vt))

varDict :: [VarDec] -> VarDict
varDict = Map.fromList . varDictList

getClass :: Prog -> OwnershipType -> Maybe Defn
getClass (Prog defs _ _) (OwnershipType name _ _) =
    find (\(Defn n _ _ _) -> n == name) defs

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
       then Right o
       else Left $ "checkType error: " ++ show o

checkAsgn prog sigma gamma x e =
    case Map.lookup x gamma of
        Just t -> do
            t' <- checkExpr prog sigma gamma e
            if t == t'
                then Right t
                else Left $ "assignment type mismatch: " ++ show t ++ " and " ++ show t'
        _                 ->
            Left $ "variable " ++ show x ++ " not in scope"

checkFieldRead prog sigma gamma e fd = do
    t <- checkExpr prog sigma gamma e
    c <- case getClass prog t of
        Just cl -> Right cl
        Nothing -> Left $ "no class of type " ++ show t
    case Map.lookup fd (fieldDict c) of
        Just t' ->
            if sv e t'
            then error "???"
            else Left $ "static visibility check failed for expression "
                 ++ show e ++ " and type " ++ show t'
        Nothing -> Left $ "field " ++ show fd ++ " not defined in class " ++ show t

checkVarExpr gamma v =
    case Map.lookup v gamma of
        Just t  -> Right t
        Nothing -> Left $ show v ++ " not in scope"

checkSeq prog sigma gamma es = do
    (e' : _) <- mapM (checkExpr prog sigma gamma) $ reverse es
    return e'

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
    FieldWrite r n e' -> error ""
    Invoc r n es      -> error ""

checkMethod :: P -> Σ -> Γ -> Method -> Either String OwnershipType
checkMethod prog sigma gamma (Method t n args vars e) =
    let types    = snd . unzip . varDictList
        argTypes = types args
        varTypes = types vars
        newGamma = gamma `Map.union` (varDict args) `Map.union` (varDict vars)
    in do
        mapM_ (checkType prog sigma) $ t : argTypes ++ varTypes
        checkExpr prog sigma newGamma e
