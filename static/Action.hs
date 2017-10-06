{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Action where

import Miso
import Miso.Subscription.Keyboard
import Miso.String hiding (zip, length)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Miso.Svg as Svg
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Web.Canvas
import CanvasBS
import Model

-- | Sum type for application events
data Action
  = HandleMouse (Int, Int)
  | GetArrows !Arrows
  | PickColor JSString
  | Selected Int
  | Rename JSString
  | PickColor2
  | GetName
  | UpdatePic
  | CoordPrint
  | Begin
  | Paint
  | Alert
  | Id

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleMouse newCoords) m = noEff m { mouseCoords = newCoords }
updateModel (GetArrows a@(Arrows x y)) m = (putStrLn ( "key press: " <> show (x,y) ) >> pure (Selected $ (selected m) + x - y*40)) #>  m { getArrows = a }
updateModel (PickColor p) m = ( (putStrLn $ "color change: " <> (show $ mouseCoords m)) >> pure Id) #> m {color = p}
updateModel (Selected i) m@(Model coord col s l _ _ _) = noEff m { selected = i, grid = M.updateWithKey (\_ _-> Just col) i l }
updateModel UpdatePic m = noEff m { store = M.updateWithKey (\_ _ -> Just (grid m)) (title m) (store m) }
updateModel PickColor2 m@(Model (x,y) col s l _ _ _) = m <# do
  ctx <- getCtx "spectrum"
  p <- getPixel ctx x y
  return $ PickColor p
updateModel CoordPrint m@(Model coord col s l _ _ _) = (putStrLn (show coord) >> pure Id) #> m
updateModel Alert m = (putStrLn "bingbong" >> pure Id) #> m
updateModel Begin m = m <# (getPic "../../image/First" "spectrum" >> return Id)
updateModel (Rename r) m = noEff m { title = r }
updateModel GetName m = (getName >>= return . Rename) #> m
updateModel Paint m = m <# do
  ctx <- getCtx "picture"
  let g = grid m
  draw ctx (40,40, toNumbBS $ M.elems g)
  return Id
updateModel Id m = noEff m

getPic p c = do
    ctx <- getCtx c
    canvasImage <- newImage
    drawImage' canvasImage ctx
    setSrc canvasImage p
    save ctx

foreign import javascript unsafe "$r = document.getElementById('title').value();"
  getName :: IO JSString
foreign import javascript unsafe "document.getElementById('canvas').addEventListener($1, $2);"
  canvasAddEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "var ctx = $1; var x = $2; var y = $3; var pixel = ctx.getImageData(x, y, 1, 1);  var data = pixel.data; $r = 'rgb(' + data[0] + ', ' + data[1] + ', ' + data[2]  + ')'"
  getPixel :: Context -> Int -> Int -> IO JSString

foreign import javascript unsafe "$r = document.getElementById($1).getContext('2d'); console.log($r);"
  getCtx :: MisoString -> IO Context

foreign import javascript unsafe "console.log($1);"
  log_ :: JSVal -> IO ()

foreign import javascript unsafe "$1.onload = function () {$2.drawImage($1,0,0,$1.width,$1.height,0,0,550,406)}"
  drawImage' :: Image ->  Context -> IO ()

foreign import javascript unsafe "$r = new Image();"
  newImage :: IO Image

foreign import javascript unsafe "$1.src = $2;"
  setSrc :: Image -> MisoString -> IO ()

