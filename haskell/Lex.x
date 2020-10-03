{
module Lex (Token(..), tokenize) where
}

%wrapper "basic"

-- From the Metamath book (4.1.1):
--
-- > The only only characters that are allowed to appear in a Metamath source
-- > file are the 94 non-whitespace printable ASCII characters [...] plus the
-- > following characters which are the "white-space" characters: space (a
-- > printable character), tab, carriage return, line feed, and form feed.

-- Valid *label tokens* (letters, digits, and the characters `-`, `.`, and
-- `_`).
$label = [\45\46\48-\57\65-\90\95\97-\122]
-- Valid *math symbol token* characters (printable, non-whitespace ASCII
-- characters except `$`).
$symbol = [\33-\35\37-\126]

-- Currently only the *basic language* of Metamath is supported (see section
-- 4.2), this means comments and file imports are not supported. The lexer will
-- fail upon encountering `$(`, `$)`, `$[`, or `$]`.
tokens :-
  $white+   ;
  "$c"      { const Constant }
  "$v"      { const Variable }
  "$d"      { const Disjoint }
  "$f"      { const Floating }
  "$e"      { const Essential }
  "$a"      { const Axiom }
  "$p"      { const Proof }
  "$="      { const Equal }
  "$."      { const Dot }
  "${"      { const Begin }
  "$}"      { const End }
  $label+   { Label }
  $symbol+  { Symbol }

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
  | Label String
  | Symbol String
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
