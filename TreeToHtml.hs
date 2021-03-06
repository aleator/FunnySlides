{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE ExtendedDefaultRules#-}
module TreeToHtml where
import Lucid
import Data.Tree
import Data.Monoid
import Data.Foldable (foldMap)

lucidify :: Monad m => Tree (HtmlT m ()) -> HtmlT m ()
lucidify t = ul_ [class_ "tree"] (recur t)
 where
  recur (Node a []) = li_ (span_ (a))
  recur (Node a ns) = (li_ . label_) (cb <> span_ (a) <> ul_ (foldMap recur ns))
  cb = input_ [type_ "checkbox"]

