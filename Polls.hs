{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules, RankNTypes, ParallelListComp#-}
module Polls where
import Lucid
import Control.Monad.State
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as LT
import Data.Monoid
import Blaze.ByteString.Builder
import Data.Aeson
import Control.Arrow ((&&&))
import Control.Lens hiding ((.=))
import Data.Aeson.Lens
import Data.Dynamic
import Tally
import FunnyTypes
import Debug.Trace

poll' :: Text -> [Html ()] -> Text -> Slide ()
poll' n i ident = poll (n<>ident) i

poll ::  Text -> [Html ()] -> Slide ()
poll name items = do
          let start = toDyn (mempty :: ScoreCard (Count,Count))
          lift (modify (addWidget name (R hrender::Response) start)) 
          _ <- hrender "" start (object [])
          return ()
   where
         submitAnswer = 
                   "var checks=[];\
                   \$('#"<>name<>"')\
                   \.find('input:radio')\
                   \.each(function(){console.log(this); \
                                    \ if ($(this).is(':checked'))\
                                    \ {checks.push(1);}\
                                    \ else if ($(this).is(':checked')) \
                                    \ {checks.push(1);}\
                                    \ else {checks.push(0);}\
                                    \});\
                   \console.log(checks);\
                   \console.log(JSON.stringify(checks));\
                   \$.ajax('../"<>name<>"'\
                   \,{data:JSON.stringify({'check':checks})\
                   \ ,type:'post'\
                   \ ,dataType:'json'\
                   \ ,contentType:'application/json; charset=utf-8'\
                   \ ,success: function(res){$('#"<>name<>"').replaceWith(res.html)}\
                   \ ,error: function(fl,st,q){console.log([fl,st,q])}\
                   \ })"
         hrender :: Session -> Dynamic -> Value -> Slide Dynamic
         hrender s d t = let (a,b) = runIdentity (runHtmlT (render s d t))
                       in toHtmlRaw (LT.decodeUtf8 (toLazyByteString (a mempty)))>>return b
         render :: Session -> Dynamic -> Value -> Html Dynamic
         render cookie dyn txt = do
                let count False = Count 0
                    count True  = Count 1 
                    parsed :: Tally (Count,Count)
                    pairThem [] = []
                    pairThem (x:y:xs) = (x,y):pairThem xs
                    pairThem [x] = []
                    parsed = Tally $ map numToC $ pairThem (txt ^.. key "check" . _Array . traverse . _Number)
                    numToC :: (Ord a, Num a) => (a,a) -> (Count,Count)
                    numToC (y,x) = (Count (if x>0 then 1 else 0), Count (if y>0 then 1 else 0))
                    countToBool (Count a, Count b) 
                        | a>b = Just True
                        | b>a = Just False
                        | otherwise = Nothing
                    orig :: ScoreCard (Count,Count)
                    orig = (fromDyn dyn (error "Bad Dynamic"))
                    next = insert name cookie parsed orig 
                    firstGo = mempty == orig
                div_ [class_ "mcq", id_ (name)] $ do  
                 trace (show parsed) (return ())
                 span_ [class_ "totalAnswers"] (toHtml (show (totalAnswers name next)))
                 ol_ [style_ "text-align:left;width:70%;"] $ do
                     sequence_ [li_ [data_ "sel" (T.pack (show i)) ] . label_  $ do
                                   option
                                   div_ [class_ "choicebtns"]  $ do
                                          div_ [class_ "choice falseS"] $ do 
                                           input_ [label_ "Incorrect" 
                                                   ,name_ ("TF"<>name<>"_"<>T.pack (show i))
                                                   ,id_ ("TFT"<>name<>"_"<>T.pack (show i))
                                                   ,(if isFalse then checked_ else data_ "nothing" "nothing")
                                                   ,type_ "radio"
                                                   ,value_ (T.pack $ show i)]
                                           label_ [for_ ("TFT"<>name<>"_"<>T.pack (show i))] $ do
                                                span_ (span_ "") 
                                                "F"
                                          div_ [class_ "choice trueS"] $ do 
                                           input_ [label_ "Correct"
                                                   ,name_ ("TF"<>name<>"_"<>T.pack (show i))
                                                   ,id_ ("TFT"<>name<>"_"<>T.pack (show i))
                                                   ,(if isTrue then checked_ else data_ "nothing" "nothing")
                                                   ,type_ "radio"
                                                   ,value_ (T.pack $ show i)]
                                           label_ [for_ ("TF"<>name<>"_"<>T.pack (show i))] $ do
                                                span_ (span_ "") 
                                                "T"
                                   div_ [style_ $
                                            "width:"<>T.pack (show relTrue)<>"%;height:1.5%;\
                                            \ text-align:left; margin:3px; font-size:small;\
                                            \  "
                                        ,class_ "resultbar True"] (toHtml (show cntTrue))
                                   div_ [style_ $
                                            "width:"<>T.pack (show relFalse)<>"%;height:1.5%;\
                                            \ text-align:left; margin:3px; font-size:small;\
                                            \  "
                                        ,class_ "resultbar False"] (toHtml (show cntFalse))
                                 | i <- [1::Int ..]
                                 | option <- items
                                 | relTrue  <- relativeBy name fst next++repeat 0
                                 | relFalse <- relativeBy name snd next++repeat 0
                                 | cntTrue  <- absoluteBy name fst next++repeat 0
                                 | cntFalse <- absoluteBy name snd next++repeat 0
                                 | mySelection <- map countToBool (fromTally parsed) ++ repeat Nothing
                                 , let isTrue  = mySelection == Just True
                                 , let isFalse = mySelection == Just False]
                 button_ [class_ "answerbutton",onclick_ submitAnswer] 
                    (if firstGo then "Set answer" else "Refresh / update your answer")
                 return (toDyn next)

fromTally (Tally ts) = ts
