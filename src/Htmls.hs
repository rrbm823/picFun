{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Htmls where

import Control.Monad
import qualified Data.Text as T
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8
import Data.String
import Data.ByteString.Lazy as B (writeFile)
import Models
import Network.HTTP.Media hiding (Accept)
import Servant.API

data HTMLBlaze

instance Accept HTMLBlaze where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToMarkup a => MimeRender HTMLBlaze a where
    mimeRender _ = renderHtml . toHtml

instance MimeRender HTMLBlaze Html where
    mimeRender _ = renderHtml
  
instance ToMarkup Tool where
  toMarkup ZipImage = docTypeHtml $ do
    H.head $ do
      script "var handleLeft = function(f) { var oReq = new XMLHttpRequest(); oReq.open('POST', '../postImg/True', true); oReq.setRequestHeader('Content-Type', 'image/jpeg'); oReq.send(f); }; var handleRight = function(f) { var oReq = new XMLHttpRequest(); oReq.open('POST', '../postImg/False', true); oReq.setRequestHeader('Content-Type', 'image/jpeg'); oReq.send(f); };"
    body $ do
      H.p $ toHtml ("Combine two pictures to create a cool new image" :: String)
      br
      H.table $ H.tr $ do
        H.td $ do
          --H.form ! A.action "../postImg/Left" ! A.method "POST" ! A.content "image/jpeg" $ do
            H.input ! A.type_ "file" ! A.accept "image/jpeg" ! A.onchange "handleLeft(this.files[0])"
            H.input ! A.type_ "file" ! A.accept "image/jpeg" ! A.onchange "handleRight(this.files[0])"
            --H.input ! A.type_ "submit"
          
        H.td $ do
          H.img ! A.src "../image/ZipImage" ! A.style "width: 50%; height: 50%"
          --H.form ! A.action "../postImg/Right" ! A.method "POST" ! A.content "image/jpeg" $ do
            --H.input ! A.type_ "file" ! A.accept "image/*"
            --H.input ! A.type_ "submit"
      
