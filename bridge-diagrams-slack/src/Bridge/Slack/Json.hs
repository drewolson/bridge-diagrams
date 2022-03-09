module Bridge.Slack.Json
  ( SnakeCaseJson (..),
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
import Data.Data (Proxy (Proxy))
import GHC.Generics (Generic (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import Text.Casing (quietSnake)

newtype SnakeCaseJson (l :: Nat) a = SnakeCaseJson a

instance
  ( KnownNat l,
    Generic a,
    GToJSON' Value Zero (Rep a)
  ) =>
  ToJSON (SnakeCaseJson l a)
  where
  toJSON (SnakeCaseJson a) = genericToJSON (snakeCaseOptions $ natToInt @l Proxy) a

instance
  ( KnownNat l,
    Generic a,
    GFromJSON Zero (Rep a)
  ) =>
  FromJSON (SnakeCaseJson l a)
  where
  parseJSON = fmap SnakeCaseJson . genericParseJSON (snakeCaseOptions $ natToInt @l Proxy)

natToInt :: forall n proxy. KnownNat n => proxy n -> Int
natToInt proxy = fromIntegral $ natVal @n proxy

snakeCaseOptions :: Int -> Options
snakeCaseOptions prefixSize =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = quietSnake . drop prefixSize
    }
