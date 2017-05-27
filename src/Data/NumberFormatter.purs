module Data.NumberFormatter
       ( formatNumber
       ) where

import Prelude
import Data.Array (head, null, tail)
import Data.Char (fromCharCode)
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import Data.Number.Format (fixed, toStringWith)
import Data.String (Pattern(..), joinWith, singleton, split)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)

formatNumber :: Int -> Number -> String
formatNumber precision x = insertSpacers $ toStringWith (fixed precision) x
  where
    insert s = do
      match3Regex    <- regex "(\\d)(?=(\\d\\d\\d)+(?!\\d))" global
      let parts      =  split (Pattern ".") s
      integralPart   <- maybe (Left "toStringWith yielded empty array") Right (head parts)
      fractionalPart <- maybe (Left "toStringWith yielded empty array") Right (tail parts)
      let inserted   =  replace match3Regex ("$1" <> (singleton $ fromCharCode 160)) integralPart
      if null fractionalPart
        then pure inserted
        else pure $ inserted <> "." <> joinWith "" fractionalPart
    insertSpacers s = either id id $ insert s
