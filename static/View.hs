{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module View where

import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Miso.Svg as Svg
import Safe
import Miso.Svg hiding (onMouseDown, onClick, style_, width_, height_, id_)
import Miso.String hiding (zip, length, replicate)
import Miso
import Model
import Action
import Grid

viewModel :: Model -> View Action
viewModel (Model (i,j) c s pixelSize l y _ gridstore title fillSwitch) =
  div_
  [ width_ "100%",
    toStyle [ 
      ("display","grid")
      ,("grid-template-columns", "280px 270px 1fr 1fr")
      ,("grid-template-rows", "360px 130px 80px 1fr 1fr")
      ,("grid-gap", "1rem")
      ]
  ]
  [ spectrum
  , canvas
  , controls
  , display
  , colors
  , sizing
  , div_ [ ] [ canvas_ [ id_ "hiddenCanvas", hidden_ "true", width_ "500", height_ "500" ] []]
  ]    
  where
    (d,e) = size y
    toStyle = style_ . M.fromList
    spectrum = div_ [toStyle [("grid-row", "1/2"), ("grid-column","1/3")]] [canvas_ [ id_ "spectrum", width_ "550", height_ "360", onLoad Begin, onClick PickSpectrum] []]
    colorCircle c x y = g_ [] [ellipse_ [
                                  cx_ x
                                  , cy_ y
                                  , onClick (PickColor c)
                                  , style_ $ M.fromList [("fill", c)]
                                  , rx_ "15"
                                  , ry_ "15" ] []
                              ]
    canvas = svg_ [ toStyle [ ("grid-row", "1/3"), ("grid-column","3/4"),("border-style","solid") ]
                  , height_ $ pack (show $ 2*d) <> "0px"
                  , width_ $ pack (show $ 2*e) <> "0px"
                    
                  ]
      [g_ [] [cell (if fillSwitch then Fill (j,k) else Selected (j,k)) (j,k) (k*20) (j*20) ( Just $ y ! (j,k) ) (if s == (j,k) then "5" else "1") | j <- [0..(d-1)], k <- [0..(e-1)] ]
      ]
    cell :: (Show a) => Action --an action to do when clicked
         -> a --an id
         -> Int --horiz pos
         -> Int --vert pos
         -> Maybe JSString --color
         -> MisoString --border width
         -> View Action
    cell s i x y r w = rect_ [ x_ (pack $ show x)
                       , y_ (pack $ show y)
                       , id_ (pack $ show i)
                       , onClick s
                       , width_ "15"
                       , height_ "15"
                       , fill_ $ maybe "rgba(0,0,0,0)" id r
                       , style_
                         $ M.fromList [
                           ("stroke-width", w)
                           , ("stroke", "black")
                           ]
                   ] []
    colors = div_ [ toStyle [("grid-row","2/3"),("grid-column","1/2") ] ] [
      svg_ [style_ $ M.fromList [ ("border-style", "solid") ] , height_ "130px", width_ "280px"]
      [colorCircle "rgba(0,255,0,255)" "20" "28"
      , colorCircle "rgba(255,255,0,255)" "60" "24"
      , colorCircle "rgba(255,0,0,255)" "100" "21"
      , colorCircle "rgba(255,0,255,255)" "140" "20"
      , colorCircle "rgba(0,0,255,255)" "180" "21"
      , colorCircle "rgba(0,255,255,255)" "220" "24"
      , colorCircle "rgba(128,128,128,255)" "260" "28"
      , colorCircle c "140" "88"
      
      ]
      
      ]
    controls = div_ [ toStyle [("grid-row","3/4"),("grid-column","1/4")] ]
      [input_ [type_ "button", onClick Paint, value_ "Draw"] []
      , input_ [ type_ "button", onClick UpdatePic, value_ "Save"] []
      , input_ [ type_ "text", onInput Rename, id_ "title",  value_ title] []
      , select_ [ onInput Review, value_ "View a drawing" ] (fmap picoption $ M.keys gridstore)
      , input_ [ type_ "file", id_ "fileReader", accept_ "image/*", onChange ReadFile ] []
      , div_ [] [ text "fill" ]
      , input_ [ type_ "checkbox", onClick SwitchFill, checked_ fillSwitch] []
      , input_ [type_ "number", onInput (PickColor . setOpacity c) , value_ ( maybe "0" findOpacity $ cursor y) ] []
      
      ]
    setOpacity x n
      | Miso.String.take 4 x == "rgba" =  ( <> n <> ")" ) . Miso.String.reverse . Miso.String.dropWhile (/=',') . Miso.String.reverse . Miso.String.init $ x
      | True                           =  ( "rgba" <> ) . Miso.String.dropWhile (/='(') . ( <> "," <> n <> ")" ) . Miso.String.init $ x 
    findOpacity x
      | Miso.String.take 4 x == "rgba" = (!!3) . Miso.String.split (==',') . Miso.String.tail . Miso.String.init $ x
      | True                           = "255"
    sizing = div_ [ toStyle [("grid-row", "2/3"),("grid-column","2/3") ] ] [
      input_ [type_ "text", onInput resizeR, value_ (pack $ show d) ] []
      ,input_ [type_ "text", onInput resizeC, value_ (pack $ show e) ] []
      ,input_ [type_ "number", onInput resizeP, value_ (pack $ show pixelSize) ] []
      ]
    resizeP t@(readMay.unpack -> Nothing :: Maybe Int) = Id
    resizeP t@(readMay.unpack -> Just x :: Maybe Int)
      | 15 > x && x > 0 = PixelResize x
      | True = Id
    resizeR t@(readMay.unpack -> Nothing :: Maybe Int) = Id
    resizeR t@(readMay.unpack -> Just x :: Maybe Int)
      | 42 > x && x > 0 = RedrawGrid $ YY $ resize (Y (S.replicate e c) (Just c) 0)  (_unyy y) x
      | True = Id
    resizeC t@(readMay.unpack -> Nothing :: Maybe Int) = Id
    resizeC t@(readMay.unpack -> Just x :: Maybe Int)
      | 42 > x && x > 0 = RedrawGrid $ YY $ fmap (flip (resize c) x) (_unyy y)
      | True = Id
    display = div_ [ toStyle [("grid-row","4/5"),("grid-column","1/4") ] ] [pixelpics $ M.keys gridstore]  
    picoption s = option_ [ value_ s ] [ text s ]
    pixelpics ss = div_ [] $ Prelude.concatMap (\s -> [
      div_ [ ] [text s]
      , canvas_ [ id_ s, width_ (pack $ show (pixelSize*e)), height_ (pack $ show (pixelSize*d))] []
      ]) ss    
    
onChange :: action -> Attribute action
onChange r = on "change" emptyDecoder (const r)
