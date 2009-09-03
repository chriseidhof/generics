{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module Generics.Regular.Happstack where

import Control.Applicative
import Control.Applicative.Error
import Control.Applicative.State hiding (get)
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
import qualified Data.Record.Label as L

data TW a

type LiftDB = forall a . DB a -> ServerPartT IO a

data Config a view edit = Config {convertView :: a :-> view, convertEdit :: a :-> edit}
defaultConfig = Config {convertView = label id const, convertEdit = label id const}

toTW :: Config a b c -> TW a
toTW = undefined

toTWView :: Config a b c -> TW b
toTWView = undefined

toTWEdit :: Config a b c -> TW c
toTWEdit = undefined

-- generic CRUD

crudHandler :: (Regular a, Regular view, Regular edit
               ,GHtml (PF a), GFormlet (PF a), GTable (PF a), GValues (PF a), GColumns (PF a), GModelName (PF a), GParse (PF a) 
               ,GHtml (PF view), GFormlet (PF edit), GTable (PF view), GModelName (PF view)
               ,Show a, Show view
               ) 
            => TW a -> Config a view edit -> LiftDB -> ServerPartT IO Response
crudHandler tw cf db = (create tw db)
            `mplus` (dir "view" $ uriRest (handleRead cf db))
            `mplus` (dir "edit" $ uriRest (handleEdit cf db))
            `mplus` (handleList tw db)


-- CRUD things
--
handleList :: (Regular a, GTable (PF a), GValues (PF a), GColumns (PF a), GModelName (PF a), GParse (PF a), Show a) 
           => TW a -> LiftDB -> ServerPartT IO Response
handleList tw  db = do x <- db $ findAll (unTw tw) []
                       okHtml $ gtable $ map snd x
            where unTw = undefined :: TW a -> a

handleRead :: (Regular a, GHtml (PF a), GValues (PF a), GColumns (PF a), GModelName (PF a), GParse (PF a), Show a,
               Regular view, GHtml (PF view), GModelName (PF view), Show view) 
           => Config a view edit -> LiftDB -> String -> ServerPartT IO Response
handleRead cf db (x:xs) = do x <- findDB (toTW cf) db (read xs)
                             okHtml $ maybe X.noHtml (ghtml . get (unWrap $ convertView cf)) x

findDB :: (Regular a, GValues (PF a), GColumns (PF a), GModelName (PF a), GParse (PF a), Show a) 
       => TW a -> LiftDB -> Int -> ServerPartT IO (Maybe a)
findDB tw db i = do x <- db $ find (unTw tw) i
                    return x
            where unTw = undefined :: TW a -> a

handleEdit :: (Regular a, Regular edit, GFormlet (PF edit), GValues (PF a), GColumns (PF a), GModelName (PF a), GParse (PF a), Show a) 
       => Config a view edit -> LiftDB -> String -> ServerPartT IO Response
handleEdit cf db (x:xs) = do let i = read xs
                             elem <- findDB (toTW cf) db i
                             let proj = fmap (get (unWrap $ convertEdit cf)) elem
                             withForm Nothing (mkForm (toTWEdit cf) proj) showErrorsInline (editDB i db cf (fromJust $ elem))
 where fromJust (Just x) = x -- todo

editDB :: (Regular a, GValues (PF a), GColumns (PF a), GModelName (PF a), Show a) 
       => Int -> LiftDB -> Config a view edit -> a -> edit -> ServerPartT IO Response
editDB i db cfg src new = do let x = set (unWrap $ convertEdit cfg) new src
                             db (update x i)
                             okHtml "Item updated."

create :: (Regular a, GFormlet (PF a), GValues (PF a), GColumns (PF a), GModelName (PF a), Show a) 
       => TW a -> LiftDB -> ServerPartT IO Response
create tw db = withForm (Just "create") (mkForm tw Nothing) showErrorsInline (createDb db)

createDb db x = do ix <- db $ new x
                   okHtml $ show ix ++ " is successfully registered"

mkForm :: (Regular a, GFormlet (PF a)) => TW a -> XFormlet a
mkForm tw = gformlet


-- Happstack specific stuff
type XFormlet a = F.XHtmlFormlet a
type XForm a = F.XHtmlForm a

htmlPage :: (X.HTML a) => a -> X.Html
htmlPage content = (X.header << (X.thetitle << "Testing forms"))
 +++ (X.body << content)

okHtml :: (X.HTML a) => a -> ServerPartT IO Response
okHtml content = ok $ toResponse $ htmlPage $ content

withForm :: Maybe String -> XForm a -> (X.Html -> [String] -> ServerPartT IO Response) -> (a -> ServerPartT IO Response) -> ServerPartT IO Response 
withForm name frm handleErrors handleOk = maybe id dir name $ msum
 [ anyPath $ methodSP GET $ createForm [] frm >>= okHtml
 , anyPath $ withDataFn lookPairs $ \d ->
     methodSP POST $ handleOk' $ simple d
 ]
 where
   handleOk' d = do
     let (extractor, html, _) = runFormState d frm
     let v = extractor  
     case v of
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
 return $ X.form X.! [X.method "POST"] << (xml +++ X.br +++ X.submit "submit" "Submit")
