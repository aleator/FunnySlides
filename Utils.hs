{-#LANGUAGE OverloadedStrings#-}
module Utils where
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid
import Data.Aeson
import Data.Text
import qualified Data.List (intersperse)
import Lucid.Base
import Lucid
import FunnyTypes

reduction :: Monad m => [HtmlT m ()] -> HtmlT m ()
reduction []     = mempty
reduction [x]    = code_ x
reduction (x:xs) = code_ x <> br_ [] <> "⇒" <> br_ [] <> reduction xs


bullets, numbers :: (Monad m,Foldable f) => f (HtmlT m ()) -> HtmlT m ()
bullets = ul_ . foldMap li_
numbers = ol_ . foldMap li_

strike,u,noTitle, red, green, blue, yellow, large, small, smaller :: Monad m => HtmlT m () -> HtmlT m ()
strike = span_ [style_ "color:#F44"]
noTitle = span_ [style_ "text-transform:none;"]
red   = span_ [style_ "color:#52E2DD"]  
green = span_ [style_ "color:#7FD796"] 
blue  = span_ [style_ "color:blue"]  
yellow  = span_ [style_ "color:yellow"]  
large  = span_ [style_ "font-size:150%"] 
small  = span_ [style_ "font-size:smaller"] 
smaller  = span_ [style_ "font-size:70%"] 
u = span_ [style_ "text-decoration:underline; text-decoration-color:#A77"]

onLeft, onRight :: Monad m => HtmlT m a -> HtmlT m a 
onLeft xs  = div_ [style_ "float:left;width:50%"] xs
onRight xs = div_ [style_ "float:right;width:50%"] xs

left, right :: Monad m => HtmlT m a -> HtmlT m a 
left = div_ [class_ "leftSide"]
right = div_ [class_ "rightSide"]

note,smallnote :: Monad m => HtmlT m () -> HtmlT m ()
note = div_ [class_ "note"]
smallnote = div_ [class_ "note small"]

box :: Monad m => HtmlT m () -> HtmlT m ()
box  = div_ [style_ "text-align:center"] . div_ [class_ "box"] . code_

rbox :: Html () -> Html ()
rbox  =  div_ [class_ "box", style_ "text-align:left;display:inline"] . code_ 

esim :: Monad m => [HtmlT m ()] -> HtmlT m ()
esim items = div_ [class_ "note example"]
           (h4_ "esim."<>
           (code_ . mconcat $ Data.List.intersperse (br_ []) items))

sfig :: Text -> Text -> Text -> Slide ()
sfig s uri txt = figure_  $ do
                img_ [src_ (uri),style_ ("width:"<>s<>"; background:wheat")]
                br_ []
                caption_ (small $ toHtml txt)

bfig :: Text -> Text -> Slide ()
bfig uri txt = figure_  $ do
                img_ [src_ (uri),style_ "width:500px; background:wheat"]
                br_ []
                caption_ (small $ toHtml txt)

mfig :: Text -> Text -> Slide ()
mfig uri txt = figure_  $ do
                img_ [src_ (uri),style_ "height:400px; background:wheat"]
                br_ []
                caption_ (small $ toHtml txt)

fig :: Text -> Text -> Slide ()
fig uri txt = figure_ [style_ "float:left;"] $ do
                img_ [src_ (uri),style_ "width:200px"]
                br_ []
                caption_ (small $ toHtml txt)

a <:> b = a <> code_ b
a <-> b = a <> " " <> b
frag :: Monad m => HtmlT m () -> HtmlT m ()
frag = p_ isFrag

isFrag = [class_ "fragment"]

olist :: Monad m => [HtmlT m ()] -> HtmlT m ()
olist = ol_ . mapM_ li_

list :: Monad m => [HtmlT m ()] -> HtmlT m ()
list = ul_ . mapM_ li_

encodeToText :: Value -> Text
encodeToText = decodeUtf8 . LBS.toStrict . encode 

refresh :: Text -> Value -> Attribute
refresh wdgt dta = termRaw "onclick" (
                        "$.ajax('/"<>wdgt<>"'\
                        \,{data:'"<>encodeToText dta<>"'\
                        \ ,type:'post'\
                        \ ,contentType:'application/json; charset=utf-8'\
                        \ ,success: function(res){$('#"<>wdgt<>"').html(res.html)}\
                        \ ,error: function(fl,st,q){console.log([fl,st,q])}\
                        \ ,dataType:'json'})")

