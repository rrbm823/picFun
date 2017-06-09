{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Api where

import Models
import Htmls (HTMLBlaze)
import Data.Proxy
import Servant.API
import qualified Data.Text as T
import Servant.JuicyPixels
import Codec.Picture.Types
import Data.ByteString.Lazy as BL
import qualified Network.HTTP.Media as M
import GHC.TypeLits
import Codec.Picture

data GIFAnim

instance Accept GIFAnim where
    contentType _ = "image" M.// "gif"

instance MimeRender GIFAnim [Image PixelRGB8] where
    mimeRender _ = either error id . encodeGifAnimation 3 LoopingForever

instance MimeUnrender GIFAnim DynamicImage where
    mimeUnrender _ = decodeGif . BL.toStrict

type ImageAPI =
  "postImg"
  :> Capture "true/false" Bool
  :> ReqBody '[JPEG 100] DynamicImage
  :> Post '[JSON] Bool
  :<|> "image"
  :> Capture "tool" String
  :> Get '[JPEG 100] DynamicImage
  :<|> "gif"
  :> Capture "tool" String
  :> Get '[GIFAnim] ([Image PixelRGB8])
  :<|> "show"
  :> Capture "tool" String
  :> Get '[HTMLBlaze] Tool

imageApi :: Proxy ImageAPI
imageApi = Proxy

