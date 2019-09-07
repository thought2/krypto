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
import Data.Refined (GreaterThan, Refined)
import Data.Refined as Refine
import Data.String.CodeUnits as String
import Data.Traversable (traverse)
import Data.Typelevel.Num (D0)

type PosInt
  = Refined (GreaterThan D0) Int

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
rotInt :: PosInt -> Int -> Int
rotInt length index =
  let
    n = Refine.unrefine length
  in
    index `rem` n
      # \x -> if x >= 0 then x else x + n

-- | Like `index`, but guaranteed to return a value as the Array is rotated in both directions
indexRot :: forall a. Array a -> Int -> Maybe a
indexRot xs i =
  let
    n = Array.length xs # Refine.unsafeRefine
  in
    Array.index xs (rotInt n i)

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

decrypt :: Config -> String -> Either Error String
decrypt config string =
  encryptArray config (String.toCharArray string)
    <#> String.fromCharArray
