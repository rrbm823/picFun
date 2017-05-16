{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Pixels where

import Servant.JuicyPixels
import Codec.Picture
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Storable as V

imag o = do
  b1 <- either (const $ return $ Image 0 0 mempty) (return . convertRGB8) =<< r
  b2 <- either (const $ return $ Image 0 0 mempty) (return . convertRGB8) =<< b
  writePng o $ zipImages b2 b1

r :: IO (Either String DynamicImage)
r = readImageWithMetadata "/home/lamb/Pictures/bobby.jpeg" >>= return . fmap fst

b :: IO (Either String DynamicImage)
b = readImage "/home/lamb/Pictures/conspiracy.jpg"

brightnessRGB8 :: Int -> Image PixelRGB8 -> Image PixelRGB8
brightnessRGB8 add = pixelMap brightFunction
     where up v = fromIntegral (fromIntegral v + add)
           brightFunction (PixelRGB8 r g b) =
                   PixelRGB8 (up r) (up g) (up b)



zipImages :: Pixel a => Image a -> Image a -> Image a
zipImages i1@(Image g1 h1 d1) i2@(Image g2 h2 d2) = let
  gmin = min g1 g2
  hmin = min h1 h2
  (Image _ _ c1) = cropImage gmin hmin i1
  (Image _ _ c2) = cropImage gmin hmin i2
  in (Image gmin hmin $ V.izipWith (\i x y -> if odd i then x else y) c1 c2 )

dynamicPixelMap :: (forall pixel . (Pixel pixel) => Image pixel -> Image pixel) -> DynamicImage -> DynamicImage
dynamicPixelMap f = aux
  where
    aux (ImageY8    i) = ImageY8 (f i)
    aux (ImageY16   i) = ImageY16 (f i)
    aux (ImageYF    i) = ImageYF (f i)
    aux (ImageYA8   i) = ImageYA8 (f i)
    aux (ImageYA16  i) = ImageYA16 (f i)
    aux (ImageRGB8  i) = ImageRGB8 (f i)
    aux (ImageRGB16 i) = ImageRGB16 (f i)
    aux (ImageRGBF  i) = ImageRGBF (f i)
    aux (ImageRGBA8 i) = ImageRGBA8 (f i)
    aux (ImageRGBA16 i) = ImageRGBA16 (f i)
    aux (ImageYCbCr8 i) = ImageYCbCr8 (f i)
    aux (ImageCMYK8 i) = ImageCMYK8 (f i)
    aux (ImageCMYK16 i) = ImageCMYK16 (f i)

dynamicPixelMap2 :: (forall pixel . (Pixel pixel) => Image pixel -> Image pixel -> Image pixel) -> DynamicImage -> DynamicImage ->  DynamicImage
dynamicPixelMap2 f = aux
  where
    aux (ImageY8 j) (ImageY8    i) = ImageY8 (f j i)
    aux (ImageY16 j) (ImageY16   i) = ImageY16 (f j i)
    aux (ImageYF j) (ImageYF    i) = ImageYF (f j i)
    aux (ImageYA8 j) (ImageYA8   i) = ImageYA8 (f j i)
    aux (ImageYA16 j) (ImageYA16  i) = ImageYA16 (f j i)
    aux (ImageRGB8 j) (ImageRGB8  i) = ImageRGB8 (f j i)
    aux (ImageRGB16 j) (ImageRGB16 i) = ImageRGB16 (f j i)
    aux (ImageRGBF j) (ImageRGBF  i) = ImageRGBF (f j i)
    aux (ImageRGBA8 j) (ImageRGBA8 i) = ImageRGBA8 (f j i)
    aux (ImageRGBA16 j) (ImageRGBA16 i) = ImageRGBA16 (f j i)
    aux (ImageYCbCr8 j) (ImageYCbCr8 i) = ImageYCbCr8 (f j i)
    aux (ImageCMYK8 j) (ImageCMYK8 i) = ImageCMYK8 (f j i)
    aux (ImageCMYK16 j) (ImageCMYK16 i) = ImageCMYK16 (f j i)


dynCrop :: Int -> Int -> DynamicImage -> DynamicImage
dynCrop w h = dynamicPixelMap $ cropImage w h

cropImage :: Pixel a => Int -> Int -> Image a -> Image a
cropImage e1 e2 img = generateImage (\x y -> pixelAt img x y) e1 e2
