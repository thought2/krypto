module Algorithm.Caesar where

import Prelude
import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Enum as Enum
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (rem)
import Data.Maybe (Maybe)
import Data.PosInt (PosInt)
import Data.PosInt as PosInt
import Data.String.CodeUnits as String
import Data.Traversable (traverse)

type Config
  = { shift :: Int
    , alphabet :: Array Char
    }

defaultConfig :: Config
defaultConfig =
  let
    alphabet = Enum.enumFromTo 'A' 'Z'
  in
    { shift: 13
    , alphabet
    }

data Error
  = ErrCharNotInAlphabet Char

derive instance genericError :: Generic Error _

instance showError :: Show Error where
  show = genericShow

-- | For a given positive length and any index, returns a new index which is guaranteed to be within the length.
-- | Negative values are considered to be taken from the end.
-- | Exceeding values are wrapped.
foo :: PosInt -> Int -> Int
foo length index =
  let
    n = PosInt.toInt length
  in
    index `rem` n
      # \x -> if x >= 0 then x else x + n

-- | Like `index`, but guaranteed to return a value as the Array is rotated in both directions
indexRot :: forall a. Array a -> Int -> Maybe a
indexRot xs i =
  let
    n = Array.length xs # PosInt.fromIntTrunc
  in
    Array.index xs (foo n i)

encryptChar :: Config -> Char -> Maybe Char
encryptChar { alphabet, shift } char = do
  idx <- Array.findIndex (_ == char) alphabet
  indexRot alphabet (shift + idx)

encryptArray :: Config -> Array Char -> Either Error (Array Char)
encryptArray config xs =
  traverse
    ( \char ->
        encryptChar config char
          # Either.note (ErrCharNotInAlphabet char)
    )
    xs

encrypt :: Config -> String -> Either Error String
encrypt config string =
  encryptArray config (String.toCharArray string)
    <#> String.fromCharArray
