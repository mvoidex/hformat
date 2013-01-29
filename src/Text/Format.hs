{-# LANGUAGE DefaultSignatures, FlexibleInstances #-}

module Text.Format (
    FormatArg, FormatArgs,
    FormatShow(..),
    format,
    (%=)
    ) where

import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)

type FormatArg = (String, Text)
type FormatArgs = [FormatArg]

-- | format "My name is $name, I am $age years old" $ ["name" %= "Jack", "age" %%= 24]
format :: String -> FormatArgs -> Text
format fmt args = pack $ format' fmt args where
    format' :: String -> FormatArgs -> String
    format' [] args = []
    format' ('$':('$':fmt)) args = '$' : format' fmt args
    format' ('$':fmt) args = case name of
        "" -> '$' : format' fmt args
        _ -> maybe (error $ "Format argument " ++ name ++ " expected") unpack (lookup name args) ++ format' fmt' args
        where
        (name, fmt') = span isAlpha fmt
    format' (c:fmt) args = c : format' fmt args

-- | FormatShow class
class FormatShow a where
    formatShow :: a -> Text
    default formatShow :: Show a => a -> Text
    formatShow = pack . show

instance FormatShow String where
    formatShow = pack

instance FormatShow Char where
    formatShow = pack . return

instance FormatShow Int
instance FormatShow Integer
instance FormatShow Double
instance FormatShow Float
instance FormatShow Bool

instance FormatShow Text where
    formatShow = id

infixr 1 %=

-- | Used to form argument
(%=) :: FormatShow a => String -> a -> FormatArg
name %= value = (name, formatShow value)
