{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Store where

import Data.Typeable.Internal (Typeable)
--import Graphics.Svg.Core (renderBS, Element)
import Control.Comonad
import Control.Lens as L
import Data.Bits
import qualified Data.Bits.Lens as L
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.MemoCombinators
import Data.Word
--import Diagrams.Backend.SVG
--import Diagrams.Prelude as D
--import Text.Blaze.Svg.Renderer.Utf8
--import Yesod

data Store s a = Store (s -> a) s deriving Functor

instance Show a => Show (Store s a) where
  show (Store b s) = (show $ b s) ++ " 'twas!"

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

data FunList s a
    = Done a
    | More s (FunList s (s -> a))
    deriving Functor

newtype Bazaar s a = Bazaar { 
    runBazaar :: forall f. Applicative f => (s -> f s) -> f a 
  } deriving Functor
                     
experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment k (Store f s) = f <$> k s 

rule :: Num s => Word8 -> Store s Bool -> Bool
rule w (Store f s) = testBit w $ 0 & partsOf (taking 3 L.bits) .~ [f (s+1), f s, f (s-1)]

tab :: Memo s -> Store s a -> Store s a
tab opt (Store f s) = Store (opt f) s

loop :: Integral s => (Store s a -> a) -> Store s a -> [Store s a]
loop f = iterate (extend f . tab integral)

window :: (Enum s, Num s) => s -> s -> Store s a -> [a]
window l h = experiment $ \ s -> [s-l..s+h]

{--
-- diagrams code
img = renderBS . svg . grid . map (window 100 0) . take 100 . loop (rule 110) $ Store (==0) (0 :: Int)

cell :: (V b ~ V2, HasStyle b, Typeable (N b), TrailLike b) => Bool -> b
cell b = unitSquare D.# fc (if b then black else white)

grid :: [[Bool]] -> Diagram B
grid = cat unitY . reverse . map (hcat . map cell)

svg :: Diagram B -> Element
svg = renderDia SVG (SVGOptions (dims (V2 1100 900)) Nothing "test" [] True)

data App = App

instance Yesod App

mkYesod "App" [parseRoutes| / ImageR GET |]

getImageR :: MonadHandler m => m TypedContent
getImageR = sendResponse $ toTypedContent (typeSvg, toContent img) 

main' = warp 3000 App
--}
