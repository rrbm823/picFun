{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Api where

import Models
import Htmls (HTMLBlaze, SVGFrontend)
import Data.Proxy
import Servant.API
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Servant.JuicyPixels
import Codec.Picture.Types
import Graphics.Svg.Core (renderBS, Element, toElement)
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Media as M
import GHC.TypeLits
import Codec.Picture
data GIFAnim (delay :: Nat)
              
instance Accept GIFAnim where
    contentType _ = "image" M.// "gif"
      
instance (KnownNat delay, delay <= 10) => Accept ( GIFAnim delay ) where
    contentType _ = "image" M.// "gif"

instance (KnownNat delay, delay <= 10) => MimeRender ( GIFAnim delay ) [Image PixelRGB8] where
  mimeRender _ = let delay = fromInteger $ natVal (Proxy :: Proxy delay)
    in either error id . encodeGifAnimation delay LoopingForever

instance MimeUnrender GIFAnim DynamicImage where
    mimeUnrender _ = decodeGif . BL.toStrict

data Svg = Svg

instance Accept Svg where
    contentType _ = "image" M.// "svg+xml"

instance MimeRender Svg Element where
    mimeRender _ = renderBS

instance MimeUnrender Svg Element where
    mimeUnrender _ = Right . toElement . TE.decodeUtf8 . BL.toStrict

type SvgAPI = "SVG" :> Capture "lvl" Int :> Get '[Svg] Element
     :<|> "showSVG" :> Get '[HTMLBlaze] SVGFrontend
     :<|> Raw
     
svgApi :: Proxy SvgAPI
svgApi = Proxy

type ImageAPI = "postImg"
  :> Capture "true/false" Bool
  :> ReqBody '[JPEG 100] DynamicImage
  :> Post '[JSON] Bool
  :<|> "image"
  :> Capture "tool" String
  :> Get '[JPEG 100] DynamicImage
  :<|> "gif"
  :> Capture "tool" String
  :> Get '[GIFAnim 10] [Image PixelRGB8]
  :<|> "show"
  :> Capture "tool" String
  :> Get '[HTMLBlaze] Tool
  :<|> Raw
  
imageApi :: Proxy ImageAPI
imageApi = Proxy

