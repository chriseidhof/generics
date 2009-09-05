{-# LANGUAGE KindSignatures #-}
module Generics.Regular.Extras where

import Generics.Regular
import Data.Char (isAlpha, toUpper)

prodFst (x :*: y) = x
prodSnd (x :*: y) = y

-- | Capitalizes the first letter and filters out all the non-alpha characters.
humanReadable :: String -> String
humanReadable = filter isAlpha . capitalize



-- | Capitalize the first letter of the string.
capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

-- | Generates a human-readable version of a selector.
h :: Selector s => t s (f :: * -> *) r -> String	
h = humanReadable . selName
