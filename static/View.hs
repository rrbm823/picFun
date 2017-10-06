{-# LANGUAGE OverloadedStrings #-}
module View where

import qualified Data.Map as M
import qualified Miso.Svg as Svg
import Miso.Svg hiding (onClick, style_, width_, height_, id_)
import Miso.String hiding (zip, length)
import Miso
import Model
import Action

viewModel :: Model -> View Action
viewModel (Model (x,y) c s l _ gridstore title) = let d = (round $ sqrt $ fromIntegral (length l)) :: Int in
  div_ [ width_ "100%", style_ $ M.fromList [ ("white-space", "nowrap" ) ] ] [
      div_ [] [spectrum, div_ [] [span_ [] [text title], pixelpic]]
      , svg_ [ style_ $ M.fromList [ ("position", "relative") , ("border-style", "solid") ]
           , height_ "400px"
           , width_ "400px"
           ] [g_ [] [cell (j+(d*k)) (j*10) (k*10) ((\(Just j) -> j) $ M.lookup (j+(d*k)) l) (if s == j+(d*k) then "5" else "1") | j <- [0..(d-1)],k <- [0..(d-1)]  ]
             ]
      
      , div_ [ style_ $ M.fromList [ ("display","block") ] ] [
          svg_ [
              style_ $ M.fromList [ ("border-style", "solid") ]
              , height_ "130px"
              , width_ "280px"
              ] [colorCircle "rgb(0,255,0)" "20" "28"
                , colorCircle "rgb(255,255,0)" "60" "24"
                , colorCircle "rgb(255,0,0)" "100" "21"
                , colorCircle "rgb(255,0,255)" "140" "20"
                , colorCircle "rgb(0,0,255)" "180" "21"
                , colorCircle "rgb(0,255,255)" "220" "24"
                , colorCircle "rgb(128,128,128)" "260" "28"
                , colorCircle c "140" "88"
                  --, coloredRect c
                ]
          , input_ [type_ "button", onClick Paint, value_ "Draw!"] []
          , input_ [type_ "button", onClick UpdatePic, value_ "Save!"] []
          , input_ [ onInput Rename, id_ "title", type_ "text", value_ "Put Title Here"] []
          , select_ [ value_ "View a drawing"] (fmap picoption $ M.keys gridstore)
            
          ]
      ]
  where
    picoption s = option_ [ value_ s ] []
    pixelpic = canvas_ [ id_ ("picture"), width_ "40", height_ "40"] []
    spectrum = canvas_ [ id_ "spectrum", width_ "550", height_ "360", onLoad Begin, onClick PickColor2] []
    colorCircle c x y = g_ [] [ellipse_ [
                                  cx_ x
                                  , cy_ y
                                  , onClick (PickColor c)
                                  , style_ $ M.fromList [("fill", c)]
                                  , rx_ "15"
                                  , ry_ "15" ] []
                              ]
    colorWheel = g_ [] [ellipse_ [
                            cx_ "100"
                            , cy_ "100"
                            , rx_ "175"
                            , ry_ "175"
                            ] []
                        ] 
    svgStyle :: M.Map MisoString MisoString
    svgStyle = M.fromList [("fill","pink")
                 , ("stroke", "purple")
                 , ("stroke-width", "2")
                 , ("fill-opacity", "0.4")
                 ] 
    cell :: Int --an id number
         -> Int --horiz pos
         -> Int --vert pos
         -> JSString --color
         -> MisoString --border width
         -> View Action
    cell s x y r w = rect_ [ x_ (pack $ show x)
                       , y_ (pack $ show y)
                       , id_ (pack $ show s)
                       , onClick $ Selected s
                       , width_ "10"
                       , height_ "10"
                       , fill_ r
                       , style_
                         $ M.fromList [
                           ("stroke-width", w)
                           , ("stroke", "black")
                           ]
                   ] []
