module Parser where

import Text.Parsec
import Text.Parsec.Char

import AstTypes

type Parser t u = Parsec String u t

parsed :: String -> Either ParseError Prog
parsed = parse prog "source"

prog :: Parser Prog u
prog = do
    defns   <- sepBy defn spaces
    spaces
    locDefs <- locDefStmts
    spaces
    e       <- expr
    spaces
    eof
    return $ Prog defns locDefs e

locDefStmts = endBy varDef $ do { spaces; char ';'; spaces }

defn :: Parser Defn u
defn = do
    string "class"
    spaces
    n  <- name
    spaces
    cs <- option [] contextDefs
    spaces
    char '{'
    fs <- sepBy field spaces
    spaces
    ms <- sepBy method spaces
    spaces
    char '}'
    return $ Defn n cs fs ms

contextDefs :: Parser [Context] u
contextDefs = do
    char '<'
    spaces
    cs <- ctxs
    spaces
    char '>'
    return cs
    <?> "context definitions"

name :: Parser String u
name = do
    l <- letter
    a <- many alphaNum
    return $ l : a

ctx :: Parser Context u
ctx = choice [
        do { n <- name ; return $ Context n },
        do { string "rep"; return Rep },
        do { string "norep"; return NoRep },
        do { string "owner"; return Owner }
    ]

ctxs :: Parser [Context] u
ctxs = sepBy ctx $ do
    spaces
    char ','
    spaces

varName :: Parser VarName u
varName = (do {string "this"; return This})
          <|> (do {n <- name ; return $ VarName n})

varDef :: Parser VarDec u
varDef = do
    o <- ownType
    spaces
    n <- varName
    return $ VarDec o n

expr :: Parser Expr u
expr = choice [newExpr, null, varNameExpr, fieldRead, fieldWrite, invocation, assignment, seq] <?> "expr"
    where newExpr = do
            string "new"
            space
            spaces
            o <- ownType
            return $ New o
          null = do
            string "null"
            return Null
          varNameExpr = do
            n <- varName
            return $ VarExpr  n
          seq = do
            e1 <- expr
            spaces
            char ';'
            spaces
            e2 <- expr
            return $ Seq e1 e2
          assignment = do
            n <- varName
            spaces
            char '='
            spaces
            e <- expr
            return $ Asgn n e
          fieldRead = do
            e <- expr
            spaces
            char '.'
            spaces
            VarName n <- varName
            return $ FieldRead e n
          fieldWrite = do
            FieldRead e1 n <- fieldRead
            spaces
            char '='
            spaces
            e2 <- expr
            return $ FieldWrite e1 n e2
          invocation = do
            e1 <- expr
            spaces
            char '.'
            spaces
            n <- name
            spaces
            char '('
            spaces
            e2 <- exprs
            spaces
            char ')'
            return $ Invoc e1 n e2

exprs :: Parser [Expr] u
exprs = sepBy expr (do { spaces; char ','; spaces })

ownType :: Parser OwnershipType u
ownType = do
    n  <- name
    spaces
    char '<'
    spaces
    c  <- ctx
    spaces
    cs <- option [] $ do { spaces; char '|'; spaces; cs <- ctxs; spaces; return cs }
    spaces
    char '>'
    return $ OwnershipType n c cs
    <?> "ownership type"

field :: Parser Field u
field = do
    o <- ownType
    spaces
    n <- name
    return $ Field o n

method :: Parser Method u
method = do
    o <- ownType
    spaces
    n <- name
    spaces
    char '('
    spaces
    as <- args
    spaces
    char ')'
    spaces
    char '{'
    spaces
    ls <- locDefStmts
    spaces
    e <- expr
    spaces
    char '}'
    return $ Method o n as ls e
        where args = sepBy varDef $ do { spaces; char ','; spaces }

