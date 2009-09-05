{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module Generics.Regular.Happstack where

import Control.Applicative
import Control.Applicative.Error
import Control.Applicative.State hiding (get)
import Control.Monad.Identity
import Text.Formlets
import qualified Text.XHtml.Strict.Formlets as F
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((+++), (<<))
import Happstack.Server
import Generics.Regular
import Generics.Regular.Formlets hiding (XFormlet)
import Generics.Regular.Database
import Generics.Regular.ModelName
import Generics.Regular.Views
import Generics.Regular.WebTypes
import Data.Record.Label
import Data.List (intercalate)
import Control.Monad.Reader (ask)
import qualified Data.Record.Label as L

data TW a

type LiftDB = forall a . DB a -> ServerPartT IO a

type BiDirectional a b = (a -> b, b -> a)
type SP a = ServerPartT IO a

data Config a view edit table create = Config { convertView  :: a :-> view
                                              , convertEdit  :: a :-> edit
                                              , convertTable :: a :-> table
                                              , convertCreate :: Either (a -> create, create -> a) (SP a, a :-> create)
                                              , afterFind :: Int -> a -> SP a
                                              }
defaultConfig = Config id' id' id' (Left (id,id)) (const return)
 where id' = label id const

toTW :: Config a b c d e-> TW a
toTW = undefined

toTWView :: Config a b c d e-> TW b
toTWView = undefined
toTWEdit :: Config a b c d e -> TW c
toTWEdit = undefined

toTWTable :: Config a b c d e -> TW d
toTWTable = undefined
toTWCreate :: Config a b c d e -> TW e
toTWCreate = undefined

currentPath r = let lRest = length (intercalate "/" $ rqPaths r)
                    revP  = reverse (rqUri r)
                in reverse (drop lRest revP)


-- generic CRUD

crudHandler :: (Regular a, Regular view, Regular edit, Regular table, Regular create
               , GValues (PF a), GColumns (PF a), GModelName (PF a), GParse (PF a) 
               ,GHtml (PF view), GFormlet (PF edit), GFormlet (PF create), GTable (PF table), GModelName (PF view)
               ,Show a, Show view
               ) 
            => TW a -> Config a view edit table create -> LiftDB -> ServerPartT IO Response
crudHandler tw cf db = askRq >>= \r ->
  let cPath = currentPath r in
            (dir "create" $ handleCreate cf db)
            `mplus` (dir "view" $ path (handleRead cf db))
            `mplus` (dir "edit" $ path (handleEdit cf db))
            `mplus` (handleList cf cPath db)


-- CRUD things
--
handleList :: (Regular a, Regular table, GTable (PF table), GValues (PF a), GColumns (PF a), GModelName (PF a), GParse (PF a), Show a) 
           => Config a v e table c -> String -> LiftDB -> ServerPartT IO Response
handleList cf path db = do x <- db $ findAll (unTw $ toTW cf) []
                           okHtml $   gtable (map (get (unWrap $ convertTable cf) . snd) x)
                                 +++ (X.hotlink (path ++ "create") << "Add item")
            where unTw = undefined :: TW a -> a

handleRead :: (Regular a, Regular view, 
               Show a, Show view,
               GHtml (PF view), GModelName (PF view),
               GValues (PF a), GColumns (PF a), GModelName (PF a), GParse (PF a)
               ) 
           => Config a view edit table create -> LiftDB -> String -> ServerPartT IO Response
handleRead cf db (xs) = do liftIO $ print xs
                           x <- findDB cf db (read xs)
                           okHtml $ maybe X.noHtml (ghtml . get (unWrap $ convertView cf)) x

findDB :: (Regular a, GValues (PF a), GColumns (PF a), GModelName (PF a), GParse (PF a), Show a) 
       => Config a v e t c -> LiftDB -> Int -> SP (Maybe a)
findDB cf db i = do x <- db $ find (unTw $ toTW cf) i
                    case x of
                         Nothing -> return Nothing
                         Just x' -> Just <$> afterFind cf i x'
                       where unTw = undefined :: TW a -> a

handleEdit :: (Regular a, Regular edit, GFormlet (PF edit), GValues (PF a), GColumns (PF a), GModelName (PF a), GParse (PF a), Show a) 
       => Config a view edit table create -> LiftDB -> String -> ServerPartT IO Response
handleEdit cf db xs = do let i = read xs
                         elem <- findDB cf db i
                         let proj = fmap (get (unWrap $ convertEdit cf)) elem
                         withForm (mkForm (toTWEdit cf) proj) showErrorsInline (editDB i db cf (fromJust $ elem))
 where fromJust (Just x) = x -- todo

editDB :: (Regular a, GValues (PF a), GColumns (PF a), GModelName (PF a), Show a) 
       => Int -> LiftDB -> Config a view edit table create -> a -> edit -> ServerPartT IO Response
editDB i db cfg src new = do let x = set (unWrap $ convertEdit cfg) new src
                             db (update x i)
                             okHtml "Item updated."

handleCreate :: (Regular a, Regular create
               , GFormlet (PF create), GValues (PF a), GColumns (PF a), GModelName (PF a), Show a) 
       => Config a view edit table create -> LiftDB -> ServerPartT IO Response
handleCreate cf db = withForm (mkForm (toTWCreate cf) Nothing) showErrorsInline (createDb db cf)

createDb :: (Regular a, GValues (PF a), GColumns (PF a), GModelName (PF a), Show a) 
       => LiftDB -> Config a view edit table create -> create -> ServerPartT IO Response
createDb db cfg x' = do f <- case convertCreate cfg of
                          Left (_,x) -> return x
                          Right (d,(Wrap c)) -> do defVal <- d
                                                   return $ \x -> set c x defVal
                        ix <- db $ new (f x')
                        okHtml $ show ix ++ " is successfully registered"

mkForm :: (Regular a, GFormlet (PF a)) => TW a -> XFormlet a
mkForm tw = gformlet


-- Happstack specific stuff
type XFormlet a = F.XHtmlFormlet (ServerPartT IO) a
type XForm a = F.XHtmlForm (ServerPartT IO) a

htmlPage :: (X.HTML a) => a -> X.Html
htmlPage content = (X.header << (X.thetitle << "Testing forms"))
 +++ (X.body << content)

okHtml :: (X.HTML a) => a -> ServerPartT IO Response
okHtml content = ok $ toResponse $ htmlPage $ content

withForm :: XForm a -> (X.Html -> [String] -> ServerPartT IO Response) -> (a -> ServerPartT IO Response) -> ServerPartT IO Response 
withForm frm handleErrors handleOk = msum
 [ methodSP GET $ createForm [] frm >>= okHtml
 , withDataFn lookPairs $ \d ->
     methodSP POST $ handleOk' $ simple d
 ]
 where
   handleOk' d = do
     let (extractor, html, _) = runFormState d frm
     let v = extractor  
     v' <- v
     case v' of
       Failure faults -> do 
         f <- createForm d frm
         handleErrors f faults
       Success s      -> handleOk s
   simple d = map (\(k,v) -> (k, Left v)) d

showErrorsInline :: X.Html -> [String] -> ServerPartT IO Response
showErrorsInline renderedForm errors =
 okHtml $ X.toHtml (show errors) +++ renderedForm

createForm :: Env -> XForm a -> ServerPartT IO X.Html
createForm env frm = do
 let (extractor, xml, endState) = runFormState env frm
 xml' <- xml
 return $ X.form X.! [X.method "POST"] << (xml' +++ X.br +++ X.submit "submit" "Submit")
