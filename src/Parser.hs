module Parser (parsed) where

import Text.ParserCombinators.Parsec

import AstTypes
import Lexer

parsed :: [Token] -> Either ParseError Prog
parsed = parse prog "sourcename"

prog :: GenParser Token st Prog
prog = do
    defns   <- many defn
    varDecs <- many varDec
    e       <- expr
    return $ Prog defns varDecs e

defn :: GenParser Token st Defn
defn =

varDec :: GenParser Token st VarDec
varDec = error ""

expr :: GenParser Token st Expr
expr = error ""







DefnR     : class STR '<' NamesR '>' '{' FieldsR MethodsR '}'
                                                        { Defn $1 $2 $3 $4 }
          | class STR '{' FieldsR MethodsR '}'          { Defn $1 [] $3 $4 }

LocalsR   : {- empty -}                                 { [] }
          | LocalR LocalsR                              { $1 : $2 }

LocalR    : OwnTypeR VarNameR ';'                       { VarDec $1 $2 }