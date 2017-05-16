{-# LANGUAGE DeriveGeneric #-}
module Models where

import GHC.Generics
import Data.Aeson
import Codec.Picture.Types

data Tool = ZipImage | Wipe | Blend | Checkerboard deriving (Eq, Read, Show, Generic) --tbd implement more than zipimage
data Switch = Off | On deriving (Eq, Read, Show, Generic)

instance ToJSON Tool
instance FromJSON Tool
instance ToJSON Switch
instance FromJSON Switch
