{
module Lex (Token(..), tokenize) where
}

%wrapper "basic"

-- From the Metamath book, chapter 4:
--
-- > The only only characters that are allowed to appear in a Metamath source
-- > file are the 94 non-whitespace printable ASCII characters [...] plus the
-- > following characters which are the "white-space" characters: space (a
-- > printable character), tab, carriage return, line feed, and form feed.

-- All valid ASCII characters except `$`.
$valid = [\33-\35\37-\126]

-- Currently comments and file imports are not supported. The lexer will fail
-- upon encountering `$(`, `$)`, `$[`, or `$]`.
tokens :-
  $white+ ;
  "$c"    { const Constant }
  "$v"    { const Variable }
  "$d"    { const Disjoint }
  "$f"    { const Floating }
  "$e"    { const Essential }
  "$a"    { const Axiom }
  "$p"    { const Proof }
  "$="    { const Equal }
  "$."    { const Dot }
  "${"    { const Begin }
  "$}"    { const End }
  $valid+ { Token }

{
data Token
  = Constant  -- `$c`
  | Variable  -- `$v`
  | Disjoint  -- `$d`
  | Floating  -- `$f`
  | Essential -- `$e`
  | Axiom     -- `$a`
  | Proof     -- `$p`
  | Equal     -- `$=`
  | Dot       -- `$.`
  | Begin     -- `${`
  | End       -- `$}`
  | Token String
  deriving (Eq, Show)

tokenize :: String -> Maybe [Token]
tokenize str = go ('\n', [], str)
  where
  go input@(_, _, str) =
    case alexScan input 0 of
      AlexEOF -> Just []
      AlexError _ -> Nothing
      AlexSkip input len -> go input
      AlexToken input len act -> do
        rest <- go input
        return (act (take len str) : rest)
}
