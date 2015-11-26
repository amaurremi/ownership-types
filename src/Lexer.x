{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                                 ;                           -- whitespace
    "//".*                                  ;                           -- comment

    class                                   { \_ -> TokClass }
    new                                     { \_ -> TokNew }
    rep                                     { \_ -> TokRep }
    norep                                   { \_ -> TokNoRep }
    owner                                   { \_ -> TokOwner }
    null                                    { \_ -> TokNull }
    void                                    { \_ -> TokVoid }
    this                                    { \_ -> TokThis }

    $alpha [$alpha $digit \_ \']*           { \s -> TokName s }        -- name

    \{                                      { \_ -> TokOpBr }
    \}                                      { \_ -> TokClBr }
    \(                                      { \_ -> TokOpPar }
    \)                                      { \_ -> TokClPar }
    \,                                      { \_ -> TokCom }
    \.                                      { \_ -> TokDot }
    \<                                      { \_ -> TokOpTr }
    \>                                      { \_ -> TokClTr }
    \;                                      { \_ -> TokSemi }
    \=                                      { \_ -> TokAsgn }
    \|                                      { \_ -> TokVert }

{
data Token =
             TokClass
           | TokNew
           | TokRep
           | TokNoRep
           | TokOwner
           | TokNull
           | TokVoid
           | TokThis
           | TokName String
           | TokEq
           | TokNeq
           | TokOpBr
           | TokClBr
           | TokOpPar
           | TokClPar
           | TokCom
           | TokDot
           | TokOpTr
           | TokClTr
           | TokSemi
           | TokAsgn
           | TokVert
    deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}