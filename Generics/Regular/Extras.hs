{-# LANGUAGE KindSignatures #-}
module Generics.Regular.Extras where

import Generics.Regular
import Data.Char (isAlpha, toUpper)

prodFst (x :*: y) = x
prodSnd (x :*: y) = y

humanReadable :: [Char] -> [Char]
humanReadable = filter isAlpha

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

h :: Selector s => t s (f :: * -> *) r -> String	
h = humanReadable . capitalize . selName
