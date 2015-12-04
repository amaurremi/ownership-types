{
module Lexer (Token(..), AlexPosn(..), lexer, tokenPos) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                                 ;                           -- whitespace
    ";".*                                   ;                           -- comment

    class                                   { \p _ -> TokClass p }
    new                                     { \p _ -> TokNew p }
    rep                                     { \p _ -> TokRep p }
    norep                                   { \p _ -> TokNoRep p }
    owner                                   { \p _ -> TokOwner p }
    null                                    { \p _ -> TokNull p }
    end                                     { \p _ -> TokEnd p }
    Unit                                    { \p _ -> TokUnitType p }
    this                                    { \p _ -> TokThis p }
    invoc                                   { \p _ -> TokInvoc p }
    seq                                     { \p _ -> TokSeq p }

    $alpha [$alpha $digit \_ \']*           { \p s -> TokName p s }        -- name

    \(                                      { \p _ -> TokOp p }
    \)                                      { \p _ -> TokCl p }
    \=                                      { \p _ -> TokAsgn p }

{
data Token = TokClass AlexPosn
           | TokNew AlexPosn
           | TokRep AlexPosn
           | TokNoRep AlexPosn
           | TokOwner AlexPosn
           | TokNull AlexPosn
           | TokVoid AlexPosn
           | TokEnd AlexPosn
           | TokUnitType AlexPosn
           | TokThis AlexPosn
           | TokName AlexPosn String
           | TokOp AlexPosn
           | TokCl AlexPosn
           | TokAsgn AlexPosn
           | TokSeq AlexPosn
           | TokInvoc AlexPosn
    deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens

tokenPos :: Token -> AlexPosn
tokenPos (TokClass p)    = p
tokenPos (TokNew p)      = p
tokenPos (TokRep p)      = p
tokenPos (TokNoRep p)    = p
tokenPos (TokOwner p)    = p
tokenPos (TokNull p)     = p
tokenPos (TokEnd p)      = p
tokenPos (TokUnitType p) = p
tokenPos (TokThis p)     = p
tokenPos (TokName p _)   = p
tokenPos (TokOp p)       = p
tokenPos (TokCl p)       = p
tokenPos (TokAsgn p)     = p
tokenPos (TokSeq p)      = p
tokenPos (TokInvoc p)    = p
}