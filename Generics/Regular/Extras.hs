module Generics.Regular.Extras where

import Generics.Regular

prodFst (x :*: y) = x
prodSnd (x :*: y) = y
