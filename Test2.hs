module Test2 where

import Generics.EMGM hiding (Enum, Read, Show, show)
import Generics.EMGM.Common.Base 

class MyRep r where
  Field         :: String -> MyRep r -> MyRep r
  (:*:)         :: Rep r1 -> MyRep r2 -> MyRep (r1,r2)
  Con           :: String -> MyRep r -> MyRep r


