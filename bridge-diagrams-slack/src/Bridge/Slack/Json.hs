module Bridge.Slack.Json
  ( PrefixedSnakeCaseJson (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    GFromJSON,
    GToJSON',
    Options (..),
    ToJSON (..),
    Value,
    Zero,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.Data (Typeable, typeOf)
import Data.Kind (Type)
import GHC.Generics (Generic (..))
import Text.Casing (quietSnake)

newtype PrefixedSnakeCaseJson a = PrefixedSnakeCaseJson a

instance
  ( Generic a,
    Typeable a,
    GToJSON' Value Zero (Rep a)
  ) =>
  ToJSON (PrefixedSnakeCaseJson a)
  where
  toJSON (PrefixedSnakeCaseJson a) = genericToJSON (snakeCaseOptions $ typeNameSize @a) a

instance
  ( Generic a,
    Typeable a,
    GFromJSON Zero (Rep a)
  ) =>
  FromJSON (PrefixedSnakeCaseJson a)
  where
  parseJSON = fmap PrefixedSnakeCaseJson . genericParseJSON (snakeCaseOptions $ typeNameSize @a)

typeNameSize :: forall a. Typeable (a :: Type) => Int
typeNameSize = length $ show $ (typeOf @a) undefined

snakeCaseOptions :: Int -> Options
snakeCaseOptions prefixSize =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = quietSnake . drop prefixSize
    }
