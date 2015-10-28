{-#LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, DeriveFunctor#-}
module Tally where
import Data.Monoid
import qualified Data.Aeson as Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Typeable

---------------
-- Tallying

newtype Tally a = Tally [a] deriving (Eq,Show,Ord,Typeable,Functor)
instance Monoid a => Monoid (Tally a) where
    mempty  = Tally []
    mappend (Tally as) (Tally bs) = Tally (zipM as bs)
instance Aeson.ToJSON a => Aeson.ToJSON (Tally a) where
    toJSON (Tally as) = Aeson.toJSON as

newtype Count = Count Int deriving (Eq,Show,Ord)
instance Monoid Count where
    mempty  = Count 0
    mappend (Count a) (Count b) = Count (a+b)
instance Aeson.ToJSON Count where
    toJSON (Count n) = Aeson.toJSON n

one  = Count 1
zero = Count 0

sumRow = zipWith mappend

zipM [] [] = []
zipM [] (x:xs) = x:zipM [] xs
zipM (x:xs) [] = x:zipM xs []
zipM (x:xs) (y:ys) = x<>y:zipM xs ys

type ScoreCard a = M.Map T.Text (M.Map BS.ByteString (Tally a))
type ScoreSheet a = M.Map BS.ByteString (Tally a)

totalAnswers :: T.Text -> ScoreCard a -> Int
totalAnswers qname sc = case M.lookup qname sc of 
                        Nothing -> 0
                        Just m -> M.size m

toTally :: [Bool] -> Tally Count
toTally = Tally . map (\x -> if x then one else zero)

insert :: T.Text -> BS.ByteString -> Tally a -> ScoreCard a -> ScoreCard a
insert qname cookie tally sc = M.insertWith' (<>) qname new sc
   where 
    new = M.singleton cookie tally
 
tally :: Monoid a => T.Text -> ScoreCard a -> Tally a
tally qname scorecard = mconcat $  M.elems 
    $ (M.findWithDefault mempty qname scorecard)

relativeBy :: T.Text -> (a->Count) -> ScoreCard a -> [Int]
relativeBy qname p c = case M.lookup qname c of
        Nothing -> []
        Just sc -> let
                    total = M.size sc
                    Tally ts = mconcat (map (fmap p) $ M.elems sc)
                    in map (\(Count i) -> round (100*fromIntegral i / fromIntegral total)) ts

absoluteBy :: T.Text -> (a->Count) -> ScoreCard a -> [Int]
absoluteBy qname p c = case M.lookup qname c of
        Nothing -> []
        Just sc -> let
                    total = M.size sc
                    Tally ts = mconcat (map (fmap p) $ M.elems sc)
                    in map (\(Count n)-> n) ts

-- Calculate relative percentages of tally
--relative :: Tally a -> [Int]
--relative (Tally ts) = let
--    Count total = mconcat ts
--    in map (\(Count i) -> round (100*fromIntegral i / fromIntegral total)) ts
--
--
--absolute :: Tally Count -> [Int]
--absolute (Tally ts) = map (\(Count t) -> t) ts


---------------
