--------------------------------------------
-- Valid Execution Environment (Figure 6) --
--------------------------------------------

module ExecEnvironment where

import qualified Data.Map as Map
import qualified Data.Set as Set

import AstTypes
import CollectionFuncs

type O = Context
type E = Map.Map O OwnershipType

data Value = ValObj O
           | ValNull
    deriving (Eq, Ord, Show)

-- stack frame
data Δ = Δ { thisVal  :: Value,
             stackVal :: Map.Map Name Value }

type Stack = [Δ]

-- stack frame typing
data D = D { thisType  :: OwnershipType,
             stackType :: Map.Map Name OwnershipType }

getStackVal :: VarName -> Δ -> Maybe Value
getStackVal This δ     = Just $ thisVal δ
getStack (VarName n) δ = getVal $ stackVal δ

getStackType :: VarName -> D -> Maybe OwnershipType
getStackType This d        = Just $ thisType d
getStackType (VarName n) d = getVal n $ stackType d

-----------
-- Rules --
-----------

typeType :: Set.Set O -> OwnershipType -> Bool
typeType os (OwnershipType _ c cs) =
    let cSet = c +++ Set.fromList cs
    in cSet ⊆ os
typeType _ _                       = True

storeType :: E -> Bool
storeType e = all (typeType (dom e)) $ vals e

objectType :: E -> O -> Bool
objectType e _ = storeType e

nullType :: E -> OwnershipType -> Bool
nullType e t = (storeType e) && (typeType (dom e) t)

stackFrame :: E -> Δ -> D -> Bool
stackFrame e δ d =
    case do
            o <- getStackVal This δ
            t <- getStackType This d
            return (o, t) of
        Just (o, t) -> error "" --(objectType E o)
        Nothing     -> error ""