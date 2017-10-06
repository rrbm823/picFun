{-# LANGUAGE OverloadedStrings #-}
module Model where

import GHCJS.Types
import qualified Data.Map as M
import Miso.Subscription.Keyboard

type Grid =  M.Map Int JSString
type GridStore =  M.Map JSString Grid

-- | Type synonym for an application model
data Model = Model { mouseCoords :: (Int, Int)
                   , color :: JSString
                   , selected :: Int
                   , grid :: Grid
                   , getArrows :: Arrows
                   , store :: GridStore
                   , title :: JSString}
           deriving (Show, Eq)

emptyModel :: Model
emptyModel = Model (0,0) "rgb(0,204,205)" 1 (M.fromList $ zip [0..1599] (repeat "rgb(255,255,255)")) (Arrows 0 0) mempty "My Pixel Art"
