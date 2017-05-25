module Operators
       ( leftZipWith
       ) where

import Data.List (List(..), (:), reverse)

leftZipWith :: forall a b c. (a -> b -> c) -> (a -> c) -> List a -> List b -> List c
leftZipWith f g = zip Nil
  where
    zip cs (a:as) (b:bs) = zip ((f a b) : cs) as bs
    zip cs (a:as) Nil    = zip ((g a)   : cs) as Nil
    zip cs Nil    _      = reverse cs
