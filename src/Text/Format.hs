{-# LANGUAGE FlexibleInstances, UndecidableInstances, DefaultSignatures #-}

-- | Format string with named args
--
-- >-- Named args
-- >"My name is {name}, I am {age} years old" ~~ ("name" %= "Joe") ~~ ("age" %= 24) ≡ "My name is Joe, I am 24 years old"
-- >-- Arg can have default value
-- >"{var:x} = {val:10}" ~~ ("var" %= y) ≡ "y = 10"
-- >-- Numeric position can be used
-- >"{0} {1} {0}" ~~ "foo" ~~ "bar" ≡ "foo bar foo"
-- >-- Positions can be omitted
-- >"{} {}" ~~ "foo" ~~ 10 ≡ "foo 10"
-- >-- Double braces to escape them
-- >"{} and {{}}" ~~ 10 ≡ "10 and {}"
module Text.Format (
	FormatArg(..), Format(..), Formatter(..),
	build,
	FormatBuild(..), Hole(..), fmt, FormatResult(..),
	format, (~~), (%=)
	) where

import Prelude.Unicode

import Control.Applicative
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Data.String
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP

data FormatArg = FormatNamed String Builder | FormatPos Builder

data Format = Format {
	formatString ∷ String,
	formatArgs ∷ [FormatArg] }

instance IsString Format where
	fromString str = Format str []

data Formatter = Formatter {
	formatter ∷ Either String Int,
	formatterDefault ∷ Maybe String }

instance Show Formatter where
	show (Formatter f def) = "{" ++ either id show f ++ maybe "" (':':) def ++ "}"

instance Read Formatter where
	readsPrec _ = readP_to_S $ between (char '{') (char '}') $ do
		n ← munch1 (∉ ":}")
		v ← option Nothing $ do
			_ ← char ':'
			v' ← munch1 (≢ '}')
			return $ Just v'
		return $ Formatter (maybe (Left n) Right $ readMaybe n) v

build ∷ Format → Text
build fstr = B.toLazyText $ mconcat $ build' 0 fstr where
	build' ∷ Int → Format → [Builder]
	build' _ (Format "" _) = []
	build' i (Format ('{':'{':fstr') args) = B.singleton '{' : build' i (Format fstr' args)
	build' i (Format ('}':'}':fstr') args) = B.singleton '}' : build' i (Format fstr' args)
	build' i (Format ('{':'}':fstr') args) = formatArg' (Formatter (Right i) Nothing) args : build' (succ i) (Format fstr' args)
	build' i (Format ('{':fstr') args) = case reads ('{':fstr') of
		[] → error $ "Can't parse formatter at " ++ fstr'
		(f, fstr''):_ → formatArg' f args : build' i (Format fstr'' args)
	build' i (Format fstr' args) = fromString s : build' i (Format fstr'' args) where
		(s, fstr'') = break (≡ '{') fstr'
	formatArg' ∷ Formatter → [FormatArg] → Builder
	formatArg' (Formatter (Left name) defVal) args = fromMaybe (error $ "Argument " ++ name ++ " not set") (lookArg <|> fmap B.fromString defVal) where
		lookArg = do
			FormatNamed _ fval ← find byName args
			return fval
		byName (FormatNamed n _) = n ≡ name
		byName _ = False
	formatArg' (Formatter (Right i) defVal) args = fromMaybe (error $ "Argument at index " ++ show i ++ " not set") (lookIdx <|> fmap B.fromString defVal) where
		lookIdx = do
			FormatPos fval ← listToMaybe $ drop i $ filter isPos args
			return fval
		isPos (FormatPos _) = True
		isPos _ = False

-- | FormatBuild class, by default using @show@
class FormatBuild a where
	formatBuild ∷ a → Builder
	default formatBuild ∷ Show a ⇒ a → Builder
	formatBuild = B.fromString ∘ show

instance FormatBuild String where
	formatBuild = B.fromString

instance FormatBuild Char where
	formatBuild = B.singleton

instance FormatBuild Int
instance FormatBuild Integer
instance FormatBuild Double
instance FormatBuild Float
instance FormatBuild Bool

instance FormatBuild Text where
	formatBuild = B.fromLazyText

class Hole a where
	hole ∷ a → [FormatArg]

instance {-# OVERLAPPING #-} Hole FormatArg where
	hole = return

instance {-# OVERLAPPING #-} Hole [FormatArg] where
	hole = id

instance {-# OVERLAPPING #-} Hole [[FormatArg]] where
	hole = concat

instance {-# OVERLAPPABLE #-} FormatBuild a ⇒ Hole a where
	hole = return ∘ FormatPos ∘ formatBuild

fmt ∷ Hole a ⇒ a → [FormatArg]
fmt = hole

class FormatResult r where
	formatResult ∷ Format → r

instance FormatResult Format where
	formatResult = id

instance {-# OVERLAPPING #-} FormatResult Text where
	formatResult = build

instance {-# OVERLAPPABLE #-} IsString s ⇒ FormatResult s where
	formatResult = fromString ∘ unpack ∘ formatResult

format ∷ FormatResult r ⇒ String → r
format = formatResult ∘ fromString

infixl 7 ~~

(~~) ∷ (Hole a, FormatResult r) ⇒ Format → a → r
fstr ~~ arg = formatResult $ fstr { formatArgs = formatArgs fstr ++ hole arg }

infixr 8 %=

(%=) ∷ FormatBuild a ⇒ String → a → [FormatArg]
name %= value = [FormatNamed name (formatBuild value)]
