module Example.Rectangle where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Generic
import Data.Int
import Data.Tuple
import Data.Array
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Gen (chooseInt)
import Graphics.Canvas
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen, runGen, evalGen, GenState)
import Signal.Time (Time, second, every)
import Signal.DOM 
import Signal
import DOM

type Point = Tuple Int Int

randomPoint :: Int -> Int -> Gen Point
randomPoint xmax ymax = do
  x <- chooseInt 1 xmax
  y <- chooseInt 1 ymax
  pure $ Tuple x y

colorSquare size x y color ctx = void $ do
  void $ setFillStyle color ctx
  void $ fillPath ctx $ rect ctx $ square size x y
  
square :: Int -> Int -> Int -> Rectangle
square size x y = { x: toNumber $ size*x
                  , y: toNumber $ size*y
                  , w: toNumber $ size
                  , h: toNumber $ size
                  }

--MODEL
type Path = Array Point

type Model = {xd :: Int, yd :: Int, size :: Int, mouse :: CoordinatePair, path :: Path, dir :: Point}

newtype Model' = Model' {xd :: Int, yd :: Int, size :: Int, mouse :: CoordinatePair, path :: Path, dir :: Point}

derive instance genericModel' :: Generic Model'

instance showModel :: Show Model' where
  show = gShow



main' :: Eff _ Unit
main' = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  blank <- init
  --runSignal (map renderInit blank)
  mouseSignal <- input
  paint <- pure $ foldp step initStep mouseSignal
  runSignal (map renderStep paint)
  
initStep = {xd : 1000, yd : 1000, size : 10, mouse : {x:0, y:0}, path : [Tuple 1 1], dir : Tuple 1 0}

init :: Eff _ (Signal Model)
init = do
  dims <- windowDimensions
  pure $ dims ~> (\d -> {xd : d.w, yd : d.h, size : 10, mouse : {x:0, y:0}, path : [Tuple 1 1], dir : Tuple 1 0})

renderStep :: forall eff. Partial => Model -> Eff _ Unit
renderStep mod = void do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  --colorSquare 50 mod.mouse.x mod.mouse.y purple ctx
  void $ setFillStyle white ctx
  fillPath ctx $ rect ctx
    { x: 300.0
    , y: 300.0
    , w: 50.0
    , h: 50.0
    }
    
renderInit :: forall eff. Partial => Model -> Eff _ Unit
renderInit mod = void do
  let canHeight = toNumber mod.yd * 0.9
      canWidth = toNumber mod.xd * 0.75
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  _ <- setCanvasHeight canHeight canvas
  _ <- setCanvasWidth canWidth canvas
  void $ setFillStyle black ctx
  void $ fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , w: canWidth
    , h: canHeight
    }
  
    --colorSquare 50 300 300 white ctx
  
step :: Partial => CoordinatePair -> Model -> Model
step c m = m { mouse = c }

input :: Eff _ (Signal CoordinatePair)
input = sampleOn (fps 20.0) <$> mousePos
  
inputDir :: Eff _ (Signal Point)
inputDir = let f = \l u d r -> ifs [Tuple l $ Tuple (-1) 0, Tuple u $ Tuple 0 (-1), Tuple d $ Tuple 0 1, Tuple r $ Tuple 1 0] $ Tuple 0 0
--note y goes DOWN
    in map4 f <$> (keyPressed 37) <*> (keyPressed 38) <*> (keyPressed 40) <*> (keyPressed 39)

ifs :: forall a. Array (Tuple Boolean a) -> a -> a
ifs li z = case uncons li of
             Just {head : Tuple b y, tail : tl} -> if b then y else ifs tl z
             Nothing         -> z 

fps :: Time -> Signal Time
fps x = every (second/x)

bindR :: forall a b m. (Monad m) => m a -> m b -> m b
bindR mx my = mx >>= const my

infixl 0 bindR as >>

white = "#FFFFFF"
black = "#000000"
red = "#FF0000"
yellow = "#FFFF00"
green = "#008000"
blue = "#0000FF"
purple = "#800080"

bgColor = black
mouseColor = red
wallColor = green
