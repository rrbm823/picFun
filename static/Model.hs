{-# LANGUAGE OverloadedStrings #-}
module Model where

import GHCJS.Types
import qualified Data.Map as M
import Miso.Subscription.Keyboard
import Grid

type Grid =  M.Map Int JSString
type GridStore =  M.Map JSString Grid

-- | Type synonym for an application model
data Model = Model { mouseCoords :: (Int, Int) --mousetracking for color picker
                   , color :: JSString --selected color
                   , selected :: (Int,Int) --index on grid
                   , pix :: Int 
                   , grid :: Grid --map from a pos.int to a color name or rgb value
                   , gridY :: YY JSString
                   , getArrows :: Arrows --arrows for index movement
                   , store :: GridStore --another map to store pictures along with titles
                   , title :: JSString --title to store current grid under
                   , fillSwitch :: Bool --switch for fill tool
                   }
           deriving (Show, Eq)

emptyModel :: Model
emptyModel = let oldList = zip [0..2] $ repeat "rgb(255,255,255)"
                 yList = zip [(x,y) | x <- [0..32], y <- [0..32]] $ repeat "rgba(255,255,255,255)"
             in Model (0,0) ("rgba(0,204,205,255)") (0,0) 1 (M.fromList oldList) (fromMap "brown" yList) (Arrows 0 0) mempty "My Pixel Art" False
