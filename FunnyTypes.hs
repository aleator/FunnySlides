module FunnyTypes where
import Lucid
import Data.Aeson
import Data.Dynamic
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Control.Monad
import Control.Monad.State
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString as BS

type Session = BS.ByteString
newtype Response   =  R (Session -> Dynamic -> Value -> (Slide Dynamic))
type InnerState = HashMap Ident (Response,Dynamic)
type Slide a = HtmlT (StateT InnerState IO) a
type Ident = Text

io :: IO a -> Slide a
io = lift . lift

getState :: Typeable a => Ident -> Slide a
getState ident = lift (get >>= maybe (error "Bad use of Dynamics") return . (fromDynamic.snd<=<Map.lookup ident))

addWidget :: Ident -> Response -> Dynamic -> InnerState -> InnerState
addWidget ident widget state hmap = Map.insert ident (widget,state) hmap
setState :: Ident -> Dynamic -> InnerState -> InnerState
setState ident a hmap  = case Map.lookup ident hmap of
                         Nothing -> error "No such widget" 
                         Just (resp,_) -> Map.insert ident (resp,a) hmap
