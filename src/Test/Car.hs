module Car where

import AstTypes

car :: Prog
car = Prog defs [] expr

defs = [engineClass, driverClass, mainClass]

engineClass = Defn "Engine" [] [] [startM, stopM]
    where startM = Method void "start" [] [] Null
          stopM  = Method void "stop" [] [] Null

driverClass = Defn "Driver" [] [] []

void = OwnershipType "void" NoRep []

mainClass = Defn "Main" [] [] [main]
    where bob     = VarDec (OwnershipType "Driver" NoRep []) "bob"
          car     = VarDec (OwnershipType "Car" NoRep []) "car"
          e       = VarDec (OwnershipType "Engine" Rep []) "e"
          carExpr = VarExpr $ VarName "car"
            Asgn (VarName "bob") (New $ OwnershipType "Driver" NoRep []),
            Asgn (VarName "car") (New $ OwnershipType "Car" NoRep []),
            FieldWrite carExpr "driver" (VarExpr $ VarName "bob"),
            Invoc carExpr "go" [],
--            Invoc (FieldRead carExpr "engine") "stop" [],
--            Invoc (Invoc carExpr "getEngine" []) "stop" [],
            Asgn (VarName "e") $ New (OwnershipType "Engine" Rep [])
--            Invoc carExpr "setEngine" [VarExpr $ VarName "e"]
            ]

expr = Invoc (New $ OwnershipType "Main" NoRep []) "main" []