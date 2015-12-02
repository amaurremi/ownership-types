module TypeCheck where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AstTypes

-- method signature type: return type + parameter types
type Sig = ([OwnershipType], OwnershipType)

type VarDict = Map.Map Name OwnershipType
type FieldDict = Map.Map Name OwnershipType
type MethodDict = Map.Map Name (Sig, [Name], Expr, VarDict)

type P = Prog
type Σ = Set.Set Context
type Γ = Map.Map VarName OwnershipType



fieldDict :: Defn -> FieldDict
fieldDict (Defn _ _ fields _) = Map.fromList $ map (\(Field t n) -> (n, t)) fields

methodDict :: Defn -> MethodDict
methodDict (Defn _ _ _ methods) = Map.fromList $
    map (\(Method t n args vars e) ->
        let (argNames, argTypes) = unzip $ varDictList args
        in (n, ((argTypes, t), argNames, e, varDict args))
    ) methods

varDictList :: [VarDec] -> [(Name, OwnershipType)]
varDictList = map (\(VarDec vt vn) -> (vn, vt))

varDict :: [VarDec] -> VarDict
varDict = Map.fromList . varDictList

typeCheck :: Prog -> Bool
typeCheck prog@(Prog defns varDecs expr) =
    let varsAndTypes         = map (\(VarDec t n) -> (n, t)) varDecs
        (varNames, varTypes) = unzip varsAndTypes
    in (all (checkClass prog) defns) &&
       (all (checkType prog Set.empty) varTypes) &&
       (checkExpr prog Set.empty (Map.fromList $ map (\(v, t) -> (VarName v, t)) varsAndTypes) expr)

checkClass :: P -> Defn -> Bool
checkClass prog defn@(Defn n cs fs ms) =
    let fDict = fieldDict defn
        sigma = Set.insert Owner $ Set.fromList cs
        gamma = Map.singleton This $ OwnershipType n Owner cs
    in (all (checkType prog sigma) $ Map.elems fDict) &&
       (all (checkMethod prog sigma gamma) ms)

checkType :: P -> Σ -> OwnershipType -> Bool
checkType prog sigma (OwnershipType n t ts) =
    let newSigma = Set.fromList [Rep, NoRep] `Set.union` sigma
    in (t `Set.member` newSigma) && (Set.fromList ts `Set.isSubsetOf` newSigma)

checkExpr :: P -> Σ -> Γ -> Expr -> Bool
checkExpr prog sigma gamma e = error ""

checkMethod :: P -> Σ -> Γ -> Method -> Bool
checkMethod prog sigma gamma (Method t n args vars e) = error ""
