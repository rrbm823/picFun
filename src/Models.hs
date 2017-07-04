{-# LANGUAGE DeriveGeneric #-}
module Models where

import GHC.Generics
import Data.Aeson

data Tool = ZipImage | Spiral | Frame | Checkerboard | Rave | Draw deriving (Eq, Read, Show, Generic) --tbd implement more than zipimage

instance ToJSON Tool
instance FromJSON Tool
         
