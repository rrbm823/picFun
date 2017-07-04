{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Htmls where

import Control.Monad
import Data.Monoid
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
  toMarkup Draw = docTypeHtml $ do
    H.head $ do
      H.style $ "html, body {width: 100%;height: 100%;margin: 0px;border: 0;overflow: hidden; /*  Disable scrollbars */display: block;  /* No floating content on sides */}"
      link ! A.rel "stylesheet" ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" 
      meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    body $ do
      H.p ! A.style "text-align: center" $ toHtml ("Draw something on an image or nothing with some friends. Yay!" :: String)
      H.div ! A.class_ "container" $ H.canvas ! A.id "theImage" $ ""
      H.div ! A.class_ "container" $ do
        H.div ! A.class_ "col-sm-6 col-md-4 col-lg-4" $ H.input ! A.class_ "btn btn-default" ! A.type_ "button" ! A.onclick "picUpdate()" ! A.value "Update This Img"
        H.div ! A.class_ "col-sm-6 col-md-4 col-lg-4" $ H.input ! A.class_ "btn btn-default" ! A.type_ "button" ! A.onclick "showPic()" ! A.value "Show Current Img"
        H.div ! A.class_ "col-sm-12 col-md-4 col-lg 4" $  H.input ! A.class_ "btn btn-default" !  A.type_ "file" ! A.accept "image/jpeg" ! A.onchange "handlePic(this.files[0])"
      script ! A.type_ "text/javascript" ! A.src "../canvas.js" $ ""
      --script ! A.type_ "text/javascript" ! A.src "../loading.js" $ ""

  toMarkup t = docTypeHtml $ do
    H.head $ do
      link ! A.rel "stylesheet" ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
      meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    body $ do
      H.p ! A.style "text-align: center" $ toHtml ("Combine two pictures to create a cool new image" :: String)
      H.div ! A.class_ "container" $ do
        H.canvas ! A.id "theImage" $ ""
      H.div ! A.class_ "container" $ do
        H.div ! A.class_ "col-sm-6 col-md-6 col-lg-4" $ H.input ! A.class_ "btn btn-default" ! A.type_ "file" ! A.accept "image/jpeg" ! A.onchange "handlePic2(this.files[0],'True')"
        H.div ! A.class_ "col-sm-6 col-md-6 col-lg-4" $ H.input ! A.class_ "btn btn-default" ! A.type_ "file" ! A.accept "image/jpeg" ! A.onchange "handlePic2(this.files[0],'False')"    
        H.div ! A.class_ "col-sm-12 col-md-8 col-lg-4" $ H.input ! A.class_ "btn btn-default" ! A.type_ "button" ! A.onclick "picUpdate2()" ! A.value "Combine!"
      script ! A.type_ "text/javascript" ! A.src "../loading.js" $ ""
      script $ "picUpdate2 = function() { img.src ='" <> l <> "?d=' + Date.now(); showPic(); };"
        where
          l = case t of
            ZipImage -> "../image/ZipImage" 
            Frame -> "../image/Frame"
            Spiral -> "../image/Spiral"
            Checkerboard -> "../image/Checkerboard"

