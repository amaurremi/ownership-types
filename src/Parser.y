{
module Parser (
    Defn,
    Field,
    Method,
    VarDec,
    VarName(..),
    Name,
    Expr(..),
    OwnershipType,
    Context(..),
    ContextParam
) where

import Lexer
}

%tokentype { Token }

%token
    class { TokClass }
    new   { TokNew }
    rep   { TokRep }
    norep { TokNoRep }
    owner { TokOwner }
    null  { TokNull }
    void  { TokVoid }
    this  { TokThis }
    STR   { TokName $$ }
    '{'   { TokOpBr }
    '}'   { TokClBr }
    '('   { TokOpPar }
    ')'   { TokClPar }
    ','   { TokCom }
    '.'   { TokDot }
    '<'   { TokOpTr }
    '>'   { TokClTr }
    ';'   { TokSemi }
    '='   { TokAsgn }
    '|'   { TokVert }

%error { parseError }

%name parse ProgR

%%

ProgR     : DefnsR LocalsR ExprR               { Prog $1 $2 $3 } -- todo remove `evaluate`

DefnsR    : DefnR                                       { [$1] }
          | DefnR DefnR                                 { $1 : $2 }

DefnR     : class STR '<' NamesR '>' '{' FieldsR MethodsR '}'
                                                        { Defn $1 $2 $3 $4 }
          | class STR '{' FieldsR MethodsR '}'          { Defn $1 [] $3 $4 }

LocalsR   : {- empty -}                                 { [] }
          | LocalR LocalsR                              { $1 : $2 }

LocalR    : OwnTypeR VarNameR ';'                       { VarDec $1 $2 }

VarNameR  : STR                                         { VarName $1 }
          | this                                        { This }

ExprR     : new OwnTypeR                                { New $1 }
          | null                                        { Null }
          | ExprR ';' ExprR                             { Seq $1 $2 }
          | VarNameR                                    { VarExpr $1 }
          | VarNameR '=' ExprR                          { Asgn $1 $2 }
          | ExprR '.' VarNameR                          { FieldRead $1 $2 }
          | ExprR '.' VarNameR '=' ExprR                { FieldWrite $1 $2 $3 }
          | ExprR '.' STR '(' ExprsR ')'                { Invoc $1 $2 $3 }

ExprsR    : {- empty -}                                 { [] }
          | ExprR ',' ExprsR                            { $1 : $2 }

NamesR    : STR                                         { [$1] }
          | STR ',' NamesR                              { $1 : $2 }

ContextR  : STR                                         { Context $1 }
          | rep                                         { Rep }
          | norep                                       { NoRep }
          | owner                                       { Owner }

ContextsR : ContextR                                    { [$1] }
          | ContextR ',' ContextsR                      { $1 : $2 }

OwnTypeR  : STR '<' ContextR '|' ContextsR '>'          { OwnershipType $1 $2 $3 }
          | STR '<' ContextR '>'                        { OwnershipType $1 $2 [] }

FieldsR   : {- empty -}                                 { [] }
          | FieldR FieldsR                              { $1 : $2 }

FieldR    : OwnTypeR VarNameR ';'                       { Field $1 $2 }

MethodR   : OwnTypeR STR '(' ArgsR ')' '{' LocalsR '}'  { Method $1 $2 $3 $3 }

ArgsR     : {- empty -}                                 { [] }
          | ArgR                                        { [$1] }
          | ArgR ',' ArgsR                              { $1 : $2 }

ArgR      : OwnTypeR VarNameR                           { VarDec $1 $2 }

MethodsR  : {- empty -}                                 { [] }
          | MethodR MethodsR                            { $1 : $2 }

{
data Defn = Defn Name [Context] [Field] [Method]
    deriving (Eq, Show)

data Field = Field OwnershipType Name
    deriving (Eq, Show)

data Method = Method OwnershipType Name [VarDec] Expr
    deriving (Eq, Show)

data VarName = VarName Name
             | This
    deriving (Eq, Show)

data VarDec = VarDec OwnershipType VarName
    deriving (Eq, Show)

type Name = String

data Expr = New OwnershipType
          | Null
          | Seq Expr Expr
          | VarExpr VarName
          | Asgn VarName Expr
          | FieldRead Expr Name
          | FieldWrite Expr Name Expr
          | Invoc Expr Name [Expr]
    deriving (Eq, Show)

data Prog = Prog [Defn] [VarDec] Expr
    deriving (Show)

data OwnershipType = OwnershipType Name Context [Context]
    deriving (Eq, Show)

data Context = Context Name
             | Rep
             | NoRep
             | Owner
    deriving (Eq, Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}