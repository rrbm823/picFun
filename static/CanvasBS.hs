{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module CanvasBS where

import Foreign.Ptr (Ptr)
import GHCJS.Types (JSVal(..))
import GHCJS.Marshal
import JavaScript.Web.Canvas
import GHC.Word
import Data.Monoid
import Data.List
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import qualified Data.JSString as J
import qualified Data.ByteString.Unsafe as B (unsafeUseAsCString)


foreign import javascript unsafe "var pixels = new Uint8ClampedArray(new Uint8ClampedArray($4), $2, $3); $1.putImageData(new ImageData(pixels, $2, $3), 0, 0);"
  drawImageData :: forall a . Context -> Int -> Int -> JSVal -> IO ()

fromBSArray :: J.JSString -> [J.JSString]
fromBSArray = go . J.split (==',')
  where go [] = []
        go xs
          | length xs >= 4 = (mkColor $ take 4 xs) : go (drop 4 xs)
          | True = []
        mkColor [r,g,b,a] = "rgba(" <> r <> "," <> g <> "," <> b <> "," <> a <> ")"
        
toNumbBS :: [J.JSString] -> B.ByteString --Make this into a better function
toNumbBS = B.pack . concat . fmap ( (\s -> read ("[" `mappend` s `mappend` "]") :: [GHC.Word.Word8]) . J.unpack . J.init . J.drop 1 . J.dropWhile (/='(') )

toNumbBSN' n w = concat . concat . fmap (replicate n) . splitty (4*w*n) . concat . fmap (  concat . (replicate n)) . fmap ( ((\s -> read ("[" `mappend` s `mappend` "]") :: [Word8])) . J.unpack . J.init . J.drop 1 . J.dropWhile (/='(') )

toNumbBSN :: Int -> Int -> [J.JSString] -> B.ByteString --Make this into a better function
toNumbBSN n w = B.pack . concat . concat . fmap (replicate n) . splitty (4*w*n) . concat . fmap (  concat . (replicate n)) . fmap ( ((\s -> read ("[" `mappend` s `mappend` "]") :: [GHC.Word.Word8])) . J.unpack . J.init . J.drop 1 . J.dropWhile (/='(') )

splitty :: Int -> [a] -> [[a]]
splitty n [] = []
splitty n xs = take n xs : splitty n  (drop n xs)

