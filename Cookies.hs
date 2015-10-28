{-#LANGUAGE OverloadedStrings#-}
module Cookies where
import Control.Monad
import Control.Monad.Trans (liftIO,MonadIO)
import Data.Time.Clock
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.Random
import Snap.Core

randomVal :: (Monad m,MonadIO m) => m BS.ByteString
randomVal = liftIO (BS.pack <$> replicateM 25 (randomRIO (97,122)))
theCookie queryCookieName = fmap (maybe "No-Session" cookieValue) (getCookie queryCookieName)

ensureCookie :: (Monad m,MonadSnap m,MonadIO m) => BS.ByteString -> m ()
ensureCookie queryCookieName = do
    rc <- getCookie queryCookieName
    case rc of
        Just _ -> return ()
        Nothing -> do
            rv <- randomVal
            liftIO $ print ("Issuing new cookie",rv)
            curTime   <- liftIO $ getCurrentTime
            let tokenExpires = addUTCTime (24*60*60) curTime  
            let newCookie = Cookie
                             queryCookieName
                             rv
                             (Just tokenExpires)
                             Nothing
                             (Just "/")
                             False
                             True
            modifyResponse (addResponseCookie newCookie)
