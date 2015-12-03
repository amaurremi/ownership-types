module Parser where

import Data.Functor.Identity (Identity)

import Lexer
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Pos

import AstTypes

tokToStr :: Token -> Maybe String
tokToStr (TokClass _) = Just "class"
tokToStr (TokNew _) = Just "new"
tokToStr (TokRep _) = Just "rep"
tokToStr (TokNoRep _) = Just "norep"
tokToStr (TokOwner _) = Just "owner"
tokToStr (TokNull _) = Just "null"
tokToStr (TokVoid _) = Just "void"
tokToStr (TokThis _) = Just "this"
tokToStr (TokName _ _) = Nothing
tokToStr (TokOp _) = Just "("
tokToStr (TokCl _) = Just ")"
tokToStr (TokAsgn _) = Just "="
tokToStr (TokSeq _) = Just "seq"
tokToStr (TokInvoc _) = Just "invoc"

parsed :: [Token] -> Either ParseError Prog
parsed = parse prog "source"

type Parser a u = ParsecT [Token] u Identity a

prog :: Parser Prog u
prog = do
    defns   <- pars $ many defn
    locDefs <- pars $ many locDef
    e       <- expr
    eof
    return $ Prog defns locDefs e

pars = between (sym "(") (sym ")")

pos :: Token -> SourcePos
pos = alexToParsecPos . tokenPos
    where alexToParsecPos (AlexPn _ l c) = newPos "filename" l c

sym :: String -> Parser () u
sym s = token show pos (\x -> do
    str <- tokToStr x
    if str == s then Just () else Nothing)

locDef = pars $ do
    t <- ownType
    n <- name
    return $ VarDec t n

defn :: Parser Defn u
defn = do
    sym "class"
    n  <- name
    cs <- pars $ many contextDef
    fs <- pars $ many field
    ms <- pars $ many method
    return $ Defn n cs fs ms

--contextDef :: Parser [Context] u
contextDef = do
    n <- name
    return $ Context n

name :: Parser Name u
name = token show pos (\x -> case x of
    TokName _ s -> Just s
    _           -> Nothing)

cont :: Parser Context u
cont = token show pos (\x -> case x of
        TokRep _    -> Just Rep
        TokNoRep _  -> Just NoRep
        TokOwner _  -> Just Owner
        TokName _ s -> Just $ Context s)

varName :: Parser VarName u
varName = vName <|> this
    where vName = do
                n <- name
                return $ VarName n
          this  = token show pos (\x -> case x of
                  TokThis _ -> Just This
                  _         -> Nothing)
varDef :: Parser VarDec u
varDef = do
    o <- ownType
    n <- name
    return $ VarDec o n

expr :: Parser Expr u
expr = choice [newExpr, null, varNameExpr, fieldRead, fieldWrite, invocation, assignment, seq]
    where newExpr = pars $ do
            sym "new"
            o <- ownType
            return $ New o
          null = do
            sym "null"
            return Null
          varNameExpr = do
            n <- varName
            return $ VarExpr  n
          seq = pars $ do
            sym "seq"
            es <- many1 expr
            return $ Seq es
          assignment = pars $ do
            sym "="
            n <- varName
            e <- expr
            return $ Asgn n e
          fieldRead = pars $ do
            e <- expr
            VarName n <- varName
            return $ FieldRead e n
          fieldWrite = pars $ do
            sym "="
            FieldRead e1 n <- fieldRead
            e2 <- expr
            return $ FieldWrite e1 n e2
          invocation = pars $ do
            sym "invoc"
            e1 <- expr
            n  <- name
            e2 <- pars $ many expr
            return $ Invoc e1 n e2

ownType :: Parser OwnershipType u
ownType = pars $ do
    n  <- name
    c  <- cont
    cs <- pars $ many cont
    return $ OwnershipType n c cs

field :: Parser Field u
field = pars $ do
    o <- ownType
    n <- name
    return $ Field o n

method :: Parser Method u
method = pars $ do
    n  <- name
    o  <- ownType
    as <- pars $ many varDef
    ls <- pars $ many locDef
    e  <- expr
    return $ Method o n as ls e
