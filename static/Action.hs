{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Action where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Data.Maybe (catMaybes)
import Data.List
import Miso
import Miso.Subscription.Keyboard
import Miso.String (MisoString, (<>))
import qualified Miso.Svg as Svg
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Web.Canvas
--import Math.Geometry.Grid
--import Math.Geometry.Grid.Square
import CanvasBS
import Model

-- | Sum type for application events
data Action
  = HandleMouse (Int, Int)
  | GetArrows !Arrows
  | PickColor JSString
  | Rename JSString
  | Review JSString
  | Fill Int
  | Selected Int
  | PickColor2
  | UpdatePic
  | SwitchFill
  | Begin
  | Paint
  | Alert
  | Id

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleMouse newCoords) m = noEff m { mouseCoords = newCoords }
updateModel (GetArrows a@(Arrows x y)) m = (pure (Selected $ (selected m) + x - y*40)) #>  m { getArrows = a }
updateModel (PickColor p) m = noEff m {color = p}
updateModel (Selected i) m@(Model coord col s l _ _ _ _) = noEff m { selected = i, grid = M.updateWithKey (\_ _-> Just col) i l }
updateModel UpdatePic m = noEff m {
  store = M.insert (title m) (grid m) (store m)
  }
  where insert' j k l --if user doesnt want to rename (or to animate a set of frames..?)
          | j `elem` M.keys l = insert' (j <> "'") k l
          | True              = M.insert j k l       
updateModel PickColor2 m = m <# do
  let (x,y) = mouseCoords m
  ctx <- getCtx "spectrum"
  p <- getPixel ctx x y
  return $ PickColor p
updateModel Alert m = (putStrLn "bingbong" >> pure Id) #> m
updateModel Begin m = m <# do
  getPic "../../image/First" "spectrum"
  noArrowScrolling
  return Id
updateModel (Rename r) m = noEff m { title = r }
updateModel (Review r) m = noEff m { title = r, grid = maybe mempty id $ M.lookup r (store m) }
updateModel SwitchFill m = noEff m { fillSwitch = not $ fillSwitch m }
updateModel (Fill i) m@(Model _ curColor cursor curGrid _ _ _ _) =
  return (Selected i)
  #> m { grid = foldl' (flip $ M.update (const $ Just curColor)) curGrid updateIndex }
  where updateIndex = M.keys $ getFillPixels [i] M.empty ((\(Just j) -> j) (M.lookup i curGrid)) curGrid
updateModel Paint m = m <# do
  ctx <- getCtx (title m)
  let g = grid m
  draw ctx (40,40, toNumbBS $ M.elems g)
  return Id
updateModel Id m = noEff m

type Stack = M.Map Int ()

addToStack :: Stack -> Int -> Stack
addToStack = flip $ flip M.insert ()

getFillPixels :: [Int] -> Stack -> JSString -> Grid -> Stack
getFillPixels [] stack c g = M.empty
getFillPixels (i:is) stack c g = let
  neighbors = rectNeighbors i 40
  notSeen = filter (`notElem` (M.keys stack)) $ catMaybes $ (\j -> stepFill j c g) <$> neighbors
  newStack = foldl' addToStack stack notSeen
  in M.union newStack (getFillPixels (is ++ notSeen) newStack c g)
 
stepFill :: Int -> JSString -> Grid -> Maybe Int
stepFill i c0 grid =
  let j = M.lookup i grid in
    if (Just c0 == j)
    then Just i
    else Nothing
         
rectNeighbors :: Int -> Int -> [Int]
rectNeighbors i d
  | i `mod` 40 == 0 = [i+1,i-d,i+d]
  | i `mod` 40 == 39 = [i-1,i-d,i+d]
  | True = [i-1,i+1,i-d,i+d] 

getPic p c = do
    ctx <- getCtx c
    canvasImage <- newImage
    drawImage' canvasImage ctx
    setSrc canvasImage p
    save ctx

foreign import javascript unsafe "document.getElementById('canvas').addEventListener($1, $2);"
  canvasAddEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "var ctx = $1; var x = $2; var y = $3; var pixel = ctx.getImageData(x, y, 1, 1);  var data = pixel.data; $r = 'rgb(' + data[0] + ', ' + data[1] + ', ' + data[2]  + ')'"
  getPixel :: Context -> Int -> Int -> IO JSString

foreign import javascript unsafe "$r = document.getElementById($1).getContext('2d');"
  getCtx :: MisoString -> IO Context

foreign import javascript unsafe "console.log($1);"
  log_ :: JSString -> IO ()

foreign import javascript unsafe "$1.onload = function () {$2.drawImage($1,0,0,$1.width,$1.height,0,0,550,406)}"
  drawImage' :: Image ->  Context -> IO ()

foreign import javascript unsafe "$r = new Image();"
  newImage :: IO Image

foreign import javascript unsafe "$1.src = $2;"
  setSrc :: Image -> MisoString -> IO ()

foreign import javascript unsafe "window.addEventListener('keydown', function(e) { if([32, 37, 38, 39, 40].indexOf(e.keyCode) > -1) { e.preventDefault();} }, false);"
  noArrowScrolling :: IO ()
