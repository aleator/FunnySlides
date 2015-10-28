{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules, RankNTypes, ScopedTypeVariables #-}
module FunnyMain where
import Lucid
import Control.Monad.State
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as LT
import Data.Dynamic
import Data.Monoid
import Snap.Core hiding (Response)
import Snap.Util.FileServe
import Snap.Util.Readable
import Snap.Http.Server
import Blaze.ByteString.Builder
import Control.Applicative
import Data.Aeson
import Control.Lens hiding ((.=))
import Data.IORef

import FunnyTypes
import Cookies

ourFancyCookieName = "jyu-fp-slide-cookie"

defaultMain :: [Slide ()] ->  IO ()
defaultMain pages = do
    (builders,routes) <- runStateT (mapM execHtmlT pages) mempty
    dataHolder <- newIORef routes
    let otherRoute = do
                routes' <- liftIO $ readIORef dataHolder
                route [(encodeUtf8 rt,method POST $ do
                                   rb <- readRequestBody 100000
                                   cookie <- theCookie ourFancyCookieName
                                   case eitherDecode rb of
                                    Right req -> do
                                           ((bldr::Map.HashMap T.Text T.Text -> Builder,newState::Dynamic),newIS::InnerState)
                                                <- liftIO $ runStateT (runHtmlT (fn cookie state' req)) routes'
                                           let theHtml = bldr mempty
                                           liftIO $ print newState
                                           liftIO $ modifyIORef dataHolder (setState rt newState)
                                           writeLBS . encode $
                                             object ["html".= LT.decodeUtf8 (toLazyByteString theHtml)]
                                    Left err -> do
                                        modifyResponse (setResponseStatus 400 "Decoding error")
                                        writeText $ "error"<> (T.pack (show err))
                                        getResponse >>= finishWith)
                           | (rt,(R fn,state')) <- Map.toList routes']
    putStrLn "Serving slides"
    quickHttpServe $ route 
     [
      (":n/images/",serveDirectory "images/")
     ,("/images/",serveDirectory "images/")
     ,("images/",serveDirectory "images/")
     ,(":n/slide.html", do
            Just n <- (>>=fromBS) <$> getParam "n" 
            ensureCookie ourFancyCookieName
            if n-1 < length builders
                then writeLBS (toLazyByteString (builders!!(n-1)))
                else error "No such slide")
     ,(":n/reveal/",serveDirectory "reveal.js/")
     ,(":n/style.css",serveFile "style.css")
     ,(":n/jquery.js",serveFile "jquery.min.js")
     ] <|> otherRoute 

