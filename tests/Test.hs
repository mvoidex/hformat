module Main (
	main
	) where

import Prelude.Unicode

import Text.Format

import Test.Hspec

main ∷ IO ()
main = hspec $ do
	describe "positional arguments" $ do
		it "should format unnamed arguments"
			(format "{} + {} = {}" ~~ (10 ∷ Int) ~~ (12 ∷ Int) ~~ (22 ∷ Int) ≡ "10 + 12 = 22")
		it "should format positional arguments"
			(format "{0} + {0} = {1}" ~~ (10 ∷ Int) ~~ (20 ∷ Int) ≡ "10 + 10 = 20")
	describe "named arguments" $
		it "should format named arguments"
			(format "{x} + {y} = {z}" ~~ "x" ~% (1 ∷ Int) ~~ "y" ~% (2 ∷ Int) ~~ "z" ~% (3 ∷ Int) ≡ "1 + 2 = 3")
	describe "default values" $ do
		it "should accept default values for positional arguments"
			(format "{0:foo} is {1:bar}" ~~ "blah" ≡ "blah is bar")
		it "should accept default values for named arguments"
			(format "{x:12} + {y:13}" ~~ "y" ~% (10 ∷ Int) ≡ "12 + 10")
	describe "lists" $
		it "should accept list of values"
			(format "{0} + {x:10} = {1}" ~~ [fmt (3 ∷ Int), "x" ~% (5 ∷ Int), fmt (8 ∷ Int)] ≡ "3 + 5 = 8")
	describe "escape" $
		it "should escape curly braces"
			(format "{} is not {{}}" ~~ "{}" ≡ "{} is not {}")
	describe "mix" $
		it "should process mixed arguments"
			(format "{1:foo} and {} are {what:args}" ~~ "what" ~% "quux" ~~ (10 ∷ Int) ~~ (20 ∷ Int) ≡ "20 and 10 are quux")
	describe "prebuild" $
		it "should show partially formatted" $
			show (format "{0} ≡ {1}" ~~ "foo" ∷ Format) ≡ "foo ≡ {1}"
