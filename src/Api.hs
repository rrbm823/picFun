{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import Models
import Htmls (HTMLBlaze)
import Data.Proxy
import Servant.API
import qualified Data.Text as T
import Servant.JuicyPixels
import Codec.Picture.Types

type ImageAPI =
  "postImg"
  :> Capture "true/false" Bool
  :> ReqBody '[JPEG 100] DynamicImage
  :> Post '[JSON] Bool
  :<|> "image"
  :> Capture "tool" String
  :> Get '[JPEG 100] DynamicImage
  :<|> "show"
  :> Capture "tool" String
  :> Get '[HTMLBlaze] Tool

imageApi :: Proxy ImageAPI
imageApi = Proxy

