{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators   #-}
module Basil.Interface (runBasil, find, new, attr, Basil (), BasilState) where

import Basil.Core
import Basil.Cache
import Generics.MultiRec.Base hiding (index)
import qualified Control.Monad.State as ST
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Record.Label hiding (set)
import Prelude hiding (mod)

type Basil phi env p a = (Persist p phi, EnumTypes phi env) => ST.StateT (BasilState phi env) (p phi) a

data BasilState phi env = BasilState { _cache :: Cache phi env
                                     , _freshVariable :: Int
                                     } 

$(mkLabels [''BasilState])

runBasil :: forall phi p env a . (Persist p phi, EnumTypes phi env) => Basil phi env p a -> p phi (a, BasilState phi env)
runBasil comp = ST.runStateT comp (BasilState (emptyState (allTypes :: Witnesses phi env)) 0)

find :: (Persist p phi, El phi ix) => Int -> Basil phi env p (Ref phi ix)
find ix = undefined -- todo return (Ref proof ix)

findCache :: (Persist p phi, El phi ix) => Ref phi ix -> Basil phi env p (Maybe ix)
findCache (Ref tix ix) = do st <- getM cache
                            return (M.lookup ix $ get cached $ lookupCache (index tix) st)

attr :: (Persist p phi, El phi ix) => Ref phi ix -> (ix :-> att) -> Basil phi env p att
attr r@(Ref tix ix) at = do val <- findCache r
                            case val of
                                 Just x  -> return $ get at x
                                 Nothing -> error "Not found in cache."

new :: (Persist p phi, El phi ix) 
    => ix -> {- relations -> -} Basil phi env p (Ref phi ix)
new i {- rels-} = do let tix = proof
                     freshId <- getM freshVariable
                     modM freshVariable (+1)
                     let saveData     = mod cached (M.insert (Fresh freshId) i)
                         addToTainted = mod tainted (S.insert (Fresh freshId))
                     modM cache (modCache (saveData . addToTainted) (index tix))
                     -- TODO :save rels
                     return (Ref tix (Fresh freshId))
                

-- rel :: (Persist p phi, El phi ix, HasRelations phi ix) => Ref phi ix -> relIndex -> Basil phi env p (Value phi (Index relIndex (Relations phi ix)))
-- rel = undefined
