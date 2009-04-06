{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ModelName where

import Records
import Relations

data ModelName a = ModelName {toModelName :: a -> String}
instance Labeled ModelName where
  lconstant    = err
  lprod ra rb  = err
  lfield l r   = err
  lcon   l r   = ModelName $ \_ -> l
  ltype ep r   = ModelName $ \x -> toModelName r (from ep x)

instance Rep ModelName (BelongsTo a) where
  rep = err

instance Rep ModelName String where
  rep = err

err = error "ModelName not defined"

modelName :: (Rep ModelName a) => a -> String
modelName x = toModelName rep x 
