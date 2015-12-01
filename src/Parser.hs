module Parser where

import Text.Parsec

import AstTypes

parsed :: String -> Either ParseError Prog
parsed = parse prog "source"

prog = do
    defns   <- many defn
    spaces
    locDefs <- many locDef
    spaces
    e       <- expr
    spaces
    eof
    return $ Prog defns locDefs e

defn = do
    string "class"
    spaces
    n <- name
    spaces
    cs <- option [] context
    spaces
    char '{'
    fs <- many field
    spaces
    ms <- many method
    spaces
    char '}'
    return $ Defn n cs fs ms

context = do
    char '<'
    spaces
    cs <- many1 ctx
    spaces
    char '>'
    return cs

name = do
    l <- letter
    a <- many alphaNum
    return $ l : a

ctx = do
    n <- name
    return $ Context n

varName = (do {string "this"; return This})
          <|> (do {n <- name ; return $ VarName n})

locDef = do
    o <- ownType
    spaces
    n <- varName
    return $ VarDec o n

expr = error ""
ownType = error ""
field = error ""
method = error ""

--
--
--LocalR    : OwnTypeR VarNameR ';'                       { VarDec $1 $2 }
--
--VarNameR  : STR                                         { VarName $1 }
--          | this                                        { This }
--
--ExprR     : new OwnTypeR                                { New $1 }
--          | null                                        { Null }
--          | ExprR ';' ExprR                             { Seq $1 $2 }
--          | VarNameR                                    { VarExpr $1 }
--          | VarNameR '=' ExprR                          { Asgn $1 $2 }
--          | ExprR '.' VarNameR                          { FieldRead $1 $2 }
--          | ExprR '.' VarNameR '=' ExprR                { FieldWrite $1 $2 $3 }
--          | ExprR '.' STR '(' ExprsR ')'                { Invoc $1 $2 $3 }
--
--ExprsR    : {- empty -}                                 { [] }
--        | ExprR ',' ExprsR                            { $1 : $2 }
--
--NamesR    : STR                                         { [$1] }
--        | STR ',' NamesR                              { $1 : $2 }