module Exercise1 where

import JoinList
import Data.Monoid

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) x y = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m
