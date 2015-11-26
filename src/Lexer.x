{
module Lexer (lexer) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                                 ;                           -- whitespace
    "//".*                                  ;                           -- comment

    class                                   { \_ -> Class }
    new                                     { \_ -> New }
    rep                                     { \_ -> Rep }
    norep                                   { \_ -> NoRep }
    owner                                   { \_ -> Owner }
    null                                    { \_ -> Null }
    void                                    { \_ -> Void }

    $alpha [$alpha $digit \_ \']*           { \s -> Name s }            -- name

    \=\=|\!\=|[\{\}\(\)\,\.\<\>\;\=\|]      { \s -> Symbol s }
    [\{\}\(\)\,\.\<\>\;\=\|]                { \s -> Symbol s }

{
data Token =
             Class
           | New
           | Rep
           | NoRep
           | Owner
           | Null
           | Void
           | Name String
           | Symbol String
    deriving (Eq, Show)

lexer = alexScanTokens
}