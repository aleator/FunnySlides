{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules, RankNTypes, FlexibleContexts#-}
module Slides where
import Lucid
import Data.String
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Control.Applicative
import Data.Dynamic
import Network.HTTP.Base

import FunnyTypes
import Polls
qrCode i = img_ [src_ ("http://chart.apis.google.com/chart?cht=qr&chs=200x200&chld=H|0&chl="<>T.pack (urlEncode i))]
qrLink   n = qrCode ("http://functional-programming.it.jyu.fi/slides/"<>show n<>"/slide.html")

reveal :: T.Text -> Slide () -> Slide ()
reveal title slides = html_ $ do
                head_ $ do
                 title_ (toHtml title) 
                 --script_ [src_ "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"] (mempty :: String)
                 script_ [src_ "jquery.js"] (mempty :: String)
                 link_ [rel_ "stylesheet", href_ "reveal/css/reveal.css"]
                 link_ [rel_ "stylesheet", href_ "style.css"]
                 link_ [rel_ "stylesheet", href_ "reveal/css/theme/blood.css", id_ "theme"]
                 meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0, maximum-scale=1.0,user-scalable=no,minimal-ui"]
                 meta_ [charset_ "UTF-8"]
                body_ $ do
                 div_ [class_ "reveal"] (div_ [class_ "slides",style_ "background:black;color:white"] $ do
                        img_  [style_ "position:absolute;left:5px;bottom:5px; max-width:10%"
                              , src_ "/images/haskell.logo.svg"]
                        img_  [style_ "position:absolute;right:5px;bottom:5px; max-width:15%"
                              , src_ "/images/jyu.logo.png"]
                        slides)
                 script_ [src_ "reveal/lib/js/head.min.js"] (mempty :: String)
                 script_ [src_ "reveal/js/reveal.js"] (mempty :: String)
                 revealOpts

revealOpts :: Slide ()
revealOpts = script_ 
    "Reveal.initialize({\
        \progress: true,\n\
        \history: true,\n\
        \center: true,\n\
        \transition: 'slide', // none/fade/slide/convex/concave/zoom\n\
        \});"




secHead :: Slide a -> Slide a
secHead h = section_ (h1_ h)

subsec :: Monad m => HtmlT m () -> HtmlT m () -> HtmlT m ()
subsec title rest = section_ $ do   
    h2_ title 
    rest

sect title rest = section_ $ do 
    _ <- secHead title
    rest
                

--simpleMCQ :: [Slide ()] -> Slide ()
    
                
item :: String -> Slide ()
item = li_ . toHtml 
t :: String -> Slide ()
t = toHtml

apoll ident title stem choices = pollster title stem (poll' ident choices) 

pollster :: (Data.String.IsString a, Monad m) =>
             HtmlT m () -> HtmlT m () -> (a -> HtmlT m ()) -> HtmlT m ()
pollster title stem pollHtml = do
   subsec title $ do    
    p_ (stem<>" (by yourself)")
    pollHtml "I"
   subsec title $ do    
    p_ (stem<>" (in groups)")
    pollHtml "G"

row :: Monad m => [HtmlT m ()] -> HtmlT m ()
row items = tr_ (mapM_ td_ items)
brow :: Monad m => [HtmlT m ()] -> HtmlT m ()
brow items = tr_ (mapM_ (td_ [style_ "border:1px solid gray;min-width:1ex;height:1.6em"])  items)

dd,dt :: String -> Slide ()
dd = dd_ . toHtml 
dt = dt_ . toHtml 

ux = "✘"
uo = "○"

space :: Monad m => HtmlT m ()
space = span_ [style_ "width:2ex"] (mempty)
cloud :: Monad m => Text -> HtmlT m ()
cloud c = span_ [style_ $ "color:"<>c] "☁"
eqto,ja :: Monad m => HtmlT m () -> HtmlT m () -> HtmlT m ()
eqto a b = a<>" = "<>b
ja a b = a<>" ja "<>b
