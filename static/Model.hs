{-# LANGUAGE OverloadedStrings #-}
module Model where

import GHCJS.Types
import qualified Data.Map as M
import Miso.Subscription.Keyboard

type Grid =  M.Map Int JSString
type GridStore =  M.Map JSString Grid

-- | Type synonym for an application model
data Model = Model { mouseCoords :: (Int, Int) --mousetracking for color picker
                   , color :: JSString --selected color
                   , selected :: Int --index on grid
                   , grid :: Grid --map from a pos.int to a color name or rgb value
                   , getArrows :: Arrows --arrows for index movement
                   , store :: GridStore --another map to store pictures along with titles
                   , title :: JSString --title to store current grid under
                   , fillSwitch :: Bool --switch for fill tool
                   }
           deriving (Show, Eq)

emptyModel :: Model
emptyModel = Model (0,0) "rgb(0,204,205)" 1 (M.fromList $ zip [0..1599] (repeat "rgb(255,255,255)")) (Arrows 0 0) mempty "My Pixel Art" False
