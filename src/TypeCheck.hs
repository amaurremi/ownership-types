module TypeCheck where

import Data.Map hiding (map)

import AstTypes

-- method signature type: return type + parameter types
type Sig = ([OwnershipType], OwnershipType)

type VarDict = Map Name OwnershipType

type FieldDict = Map Name OwnershipType

type MethodDict = Map Name (Sig, [Name], Expr, VarDict)

fieldDict :: Defn -> FieldDict
fieldDict (Defn _ _ fields _) = fromList $ map (\(Field t n) -> (n, t)) fields

methodDict :: Defn -> MethodDict
methodDict (Defn _ _ _ methods) = fromList $
    map (\(Method t n args vars e) ->
        let vDict                = map (\(VarDec vt vn) -> (vn, vt))
            (argNames, argTypes) = unzip $ vDict args
            varDict              = fromList $ vDict vars
        in (n, ((argTypes, t), argNames, e, varDict))
    ) methods

