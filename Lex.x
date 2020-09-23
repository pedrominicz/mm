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

$valid = [\33-\126]

tokens :-
  $white+ ;
  $valid+ { Token }

{
data Token
  = Token String
  deriving (Show)

tokenize :: String -> Maybe [Token]
tokenize str = go ('\n', [], str)
  where
  go input@(_, _, str) =
    case alexScan input 0 of
      AlexEOF            -> Just []
      AlexError _        -> Nothing
      AlexSkip input len -> go input
      AlexToken input len act -> do
        rest <- go input
        return (act (take len str) : rest)
}
