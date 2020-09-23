module Test where

import Lex

main :: IO ()
main = print $
  tokenize "Hello, world!"
