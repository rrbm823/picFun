{-# LANGUAGE DeriveGeneric #-}
module Models where

import GHC.Generics
import Data.Aeson
import Codec.Picture.Types

data Tool = ZipImage | Spiral | Frame | Checkerboard deriving (Eq, Read, Show, Generic) --tbd implement more than zipimage

instance ToJSON Tool
instance FromJSON Tool
