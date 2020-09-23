module Test where

import Lex

import Data.Functor
import Data.List
import System.Exit

test0 :: Bool
test0 = tokenize "hello world ${ $} $c $v $f $e $d $a $p $. $= ! ~" == Just [Token "hello", Token "world", Begin, End, Constant, Variable, Floating, Essential, Disjoint, Axiom, Proof, Dot, Equal, Token "!", Token "~"]

tests :: [Bool]
tests =
  [ test0
  ]

errors :: [String]
errors = elemIndices False tests <&> \i ->
  unwords ["Test", show i, "failed."]

main :: IO ()
main = do
  sequence_ $ putStrLn <$> errors
  if null errors
    then putStrLn "All tests were successful."
    else exitFailure
