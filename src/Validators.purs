module Validators
       ( Error
       , Errors
       , Validated
       , toValidated
       , toValidated'
       , matches
       , matches'
       ) where

import Prelude
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.String.Regex (Regex, test)
import Data.Validation.Semigroup (V, invalid, unV)

type Error     = String
type Errors    = Array Error
type Validated = V Errors

toValidated :: forall a. Either Errors a -> Validated a
toValidated (Left errors) = invalid errors
toValidated (Right a)     = pure a

toValidated' :: forall a. Either Error a -> Validated a
toValidated' = lmap singleton >>> toValidated

matches :: Regex -> Error -> String -> Validated String
matches regex _     value | test regex value = pure value
matches _     error _                        = invalid [error]

matches' :: Validated Regex -> Error -> String -> Validated String
matches' v error x = unV invalid (\regex -> matches regex error x) v
