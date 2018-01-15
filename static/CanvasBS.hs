{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module CanvasBS where

import Foreign.Ptr (Ptr)
import GHCJS.Types (JSVal)
import JavaScript.Web.Canvas
import GHC.Word
import Data.Monoid 
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import qualified Data.JSString as J
import qualified Data.ByteString.Unsafe as B (unsafeUseAsCString)

foreign import javascript unsafe "var pixels = new Uint8ClampedArray($4.u8, $2); $1.putImageData(new ImageData(pixels, $2, $3), 0, 0);"
  blitByteString :: forall a . Context -> Int -> Int -> Ptr a -> IO ()

toNumbBS :: [J.JSString] -> B.ByteString
toNumbBS = B.pack . concat . drop 1 . fmap ( (flip mappend ([255])) . (\s -> read ("[" `mappend` s `mappend` "]") :: [GHC.Word.Word8]) . J.unpack . J.init . J.drop 4 )

seqToNumbBS :: S.Seq J.JSString -> B.ByteString
seqToNumbBS = B.pack . foldl (<>) mempty  . S.drop 1 . fmap ( (flip mappend ([255])) . (\s -> read ("[" `mappend` s `mappend` "]") :: [GHC.Word.Word8]) . J.unpack . J.init . J.drop 4 )

draw canvasJS (width, height, pixelByteString) =
  B.unsafeUseAsCString pixelByteString $ \ ptr ->
  blitByteString canvasJS width height ptr

blueScreen :: IO (Int, Int, B.ByteString)
blueScreen = do
    let width  = 128 :: Int
    let height = 128 :: Int
    let blue   = [0, 0, 255, 255] -- [Red, Green, Blue, Alpha]
    let buffer = B.pack $ concat $ replicate (width*height) blue
    return (width, height, buffer)


