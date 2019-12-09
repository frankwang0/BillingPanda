module Domain.Validation where

import ClassyPrelude
--import Text.Regex.PCRE.Heavy

type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b

validate constructor validations val =
    case concatMap (\f -> maybeToList $ f val) validations of
        [] -> Right $ constructor val
        errs -> Left errs