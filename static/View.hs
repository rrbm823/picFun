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
viewModel (Model (x,y) c s l _ gridstore title fillSwitch) = let d = (round $ sqrt $ fromIntegral (length l)) :: Int in
  div_ [ width_ "100%",
         toStyle [ 
           ("display","grid")
           ,("grid-template-columns", "550px 1fr 1fr")
           ,("grid-template-rows", "130px 200px 70px 1fr 1fr")
           ,("grid-gap", "1rem")
           --,("white-space", "nowrap" )
           ]
       ] [ spectrum
         , svg_ [ toStyle [ ("grid-row", "1/3"), ("grid-column","2/3"),("border-style", "solid") ]
                , height_ "400px"
                , width_ "400px"
                ] [g_ [] [let i = j+(d*k) in
                      cell (if fillSwitch then (Fill i) else Selected i) i (j*10) (k*10) (M.lookup i l) (if s == i then "5" else "1")
                      | j <- [0..(d-1)],k <- [0..(d-1)]  ]
               ]
         , div_ [ toStyle [("grid-row","3/4"),("grid-column","1/6")] ] [input_ [type_ "button", onClick Paint, value_ "Draw"] []
                   , input_ [ type_ "button", onClick UpdatePic, value_ "Save"] []
                   , input_ [ onInput Rename, id_ "title", type_ "text", value_ title] []
                   , select_ [ onInput Review, value_ "View a drawing" ] (fmap picoption $ M.keys gridstore)
                   , p_ [] []
                   , input_ [ type_ "checkbox", onClick SwitchFill, value_ "Fill", checked_ fillSwitch] []
                   ]
         , div_ [ toStyle [("grid-row","4/5"),("grid-column","1/5") ] ] $ [pixelpics $ M.keys gridstore]
             
         , div_ [ toStyle [("grid-row","1/2"),("grid-column","3/4") ] ] [
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
             
             ]
         ]
  where
    toStyle = style_ . M.fromList
    picoption s = option_ [ value_ s ] [ text s ]
    pixelpics ss = div_ [] $ Prelude.concatMap (\s -> [
      p_ [toStyle [("display", "inline")] ] [text s]
      , canvas_ [ id_ s, width_ "40", height_ "40"] []
      ]) ss
    spectrum = div_ [toStyle [("grid-row", "1/3"), ("grid-column","1/3")]] [canvas_ [ id_ "spectrum", width_ "550", height_ "360", onLoad Begin, onClick PickColor2] []]
    colorCircle c x y = g_ [] [ellipse_ [
                                  cx_ x
                                  , cy_ y
                                  , onClick (PickColor c)
                                  , style_ $ M.fromList [("fill", c)]
                                  , rx_ "15"
                                  , ry_ "15" ] []
                              ]
    cell :: Action --an action to do when clicked
         -> Int --an id
         -> Int --horiz pos
         -> Int --vert pos
         -> Maybe JSString --color
         -> MisoString --border width
         -> View Action
    cell s i x y r w = rect_ [ x_ (pack $ show x)
                       , y_ (pack $ show y)
                       , id_ (pack $ show i)
                       , onClick s
                       , width_ "10"
                       , height_ "10"
                       , fill_ $ maybe "rgba(0,0,0,0)" id r
                       , style_
                         $ M.fromList [
                           ("stroke-width", w)
                           , ("stroke", "black")
                           ]
                   ] []
