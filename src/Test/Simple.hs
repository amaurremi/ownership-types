module Test.Simple where

import AstTypes

simple :: Prog
simple = Prog defs locals expr

defs = [engineClass]

engineClass = Defn "Engine" [] [] [startM, stopM]
    where startM = Method engineType "start" [] [] $ New engineType
          stopM  = Method engineType "stop" [] [] $ New engineType

engineType = OwnershipType "Engine" NoRep []

locals = [VarDec (OwnershipType "Engine" NoRep []) "engine"]

expr = Invoc (VarExpr $ VarName "engine") "start" []