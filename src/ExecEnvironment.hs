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
data Δ = Δ { thisVal  :: O,
             stackVal :: Map.Map Name O }

type Stack = [Δ]

-- stack frame typing
data D = D { thisType  :: OwnershipType,
             stackType :: Map.Map Name OwnershipType }

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
stackFrame e (Δ this δ) (D thisT d) =
    (objectType e this) && all ()