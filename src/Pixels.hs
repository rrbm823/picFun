{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Pixels where

import Grid
import Servant.JuicyPixels
import Codec.Picture
import Codec.Picture.Types
import Data.List (sortBy, span)
import Data.Ord
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Storable as V

b p = do
  j <- readImage p
  case j of
    Left _ -> return g
    Right i -> return $ convertRGB8 i
    
g = generateImage (const.const $ PixelRGB8 133 150 250) 1000 1000
h = generateImage (const.const $ PixelRGB8 200 0 100) 1000 1000 

brightnessRGB8 :: Int -> Image PixelRGB8 -> Image PixelRGB8
brightnessRGB8 add = pixelMap brightFunction
     where up v = fromIntegral (fromIntegral v + add)
           brightFunction (PixelRGB8 r g b) =
                   PixelRGB8 (up r) (up g) (up b)
                   
spiral_part :: (Ord t, Floating t) => [(t,t)] -> [(t,t)] -> (Int, Int) -> Bool 
spiral_part fn fp (x,y) = closest_n > closest_p
  where
    closest_n = e_dist_int (0,0) $ head $ sortBy (comparing $ e_dist_int (x,y)) fn --inefficient sorting!
    closest_p = e_dist_int (0,0) $ head $ sortBy (comparing $ e_dist_int (x,y)) fp

spiral_check :: (Enum t, Ord t, Floating t) => (t,t) -> Bool
spiral_check c = let
  (r1,theta0) = toPolarPos c;
  r_sp_p = [ sqrt $ t + n*pi | n <- [0,2..], t <- [theta0]];
  r_sp_n = [ sqrt $ t + n*pi | n <- [1,3..], t <- [theta0]];
  (rp1, rn1) = case inf ((<r1).fst) $ zip (r_sp_p) (r_sp_n) of
    Nothing -> (0,0)
    Just j -> j
  (rp2, rn2) = case inf ((<r1).snd) $ zip (r_sp_p) (r_sp_n) of
    Nothing -> (0,0)
    Just j -> j
  in r1 < rn1 -- thats not right

inf :: (a -> Bool) -> [a] -> Maybe a
inf b as = lastMaybe $ fst $ span b as-- infimum of a sequence based on a conditional b

sup :: (a -> Bool) -> [a] -> Maybe a
sup b as = headMaybe $ snd $ span b as-- supremum of a sequence based on a conditional b

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe xs = Just $ head xs

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs

f_spiral_n ts = [ (z * sqrt t * cos t, z * sqrt t * sin t) | z <- [-1], t <- ts  ]
f_spiral_p ts = [ (z * sqrt t * cos t, z * sqrt t * sin t) | z <- [1], t <- ts  ]

r_spiral_n ts = [ (z * sqrt t, t) | z <- [-1], t <- ts ]
r_spiral_p ts = [ (z * sqrt t, t) | z <- [1], t <- ts ]

e_dist_int :: Num t => (Int, Int) -> (t,t) -> t
e_dist_int (x2',y2') (x1,y1)  = let x2 = fromIntegral x2'; y2 = fromIntegral y2' in (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)

e_dist (x2,y2) (x1,y1)  = (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)

toPolarPos x = let (r,p) = toPolar x in
  if p < 0 then (r,p + 2*pi) else (r,p)
                              
toPolar (x,y) = (sqrt (x*x + y*y) , atan_ y x) where
  atan_ j i
    | i > 0 = atan (j / i)
    | i == 0 && j > 0 = pi / 2
    | i == 0 && j < 0 = - pi / 2
    | i == 0 && j == 0 = 0 / 0 
    | j < 0 = atan (j / i) - pi
    | j >= 0 = atan (j / i) + pi

toCart (r,theta) = (r*cos(theta), r*sin(theta))

sprlImages :: Pixel a => Image a -> Image a -> IO (Image a)
sprlImages i1@(Image g1 h1 d1) i2@(Image g2 h2 d2) = let
  gmin = min g1 g2
  hmin = min h1 h2
  gminF = fromIntegral gmin :: Float
  hminF = fromIntegral hmin :: Float
  c1 = cropImage gmin hmin i1
  c2 = cropImage gmin hmin i2
  in do
     c1' <- thawImage c1
     let sp = (\(a,b) -> (round a + gmin `div` 2, round b + hmin `div` 2)) <$> f_spiral_n [0..gminF*gminF] ++ f_spiral_p [0..gminF*gminF]
     mapM_ (\(a,b) -> writePixel c1' a b $ pixelAt c2 a b) $ filter (\(a,b) -> a >= 0 && b >= 0 && a < gmin && b < hmin) sp
     freezeImage c1'
  
imageInImage :: Pixel a => Image a -> Image a -> IO (Image a)
imageInImage img1@(Image e f _) img2@(Image g h _)
  | e >= g && f >= h = makeFrame img1 img2 $ frame g h e f
  | e >= g && f < h = makeFrame img1 (cropImage g f img2) $ frame g f e f
  | e < g && f >= h = makeFrame (cropImage e h img1) img2 $ frame e f e h  
  | True = makeFrame img2 img1 $ frame e f g h

makeFrame :: Pixel a => Image a -> Image a -> [(Int,Int)] -> IO (Image a)
makeFrame i1 i2 box = do
        i <- thawImage i1
        mapM (\(x,y) -> writePixel i x y $ pixelAt i2 (x - (fst $ head box)) (y - (snd $ head box))) box
        freezeImage i

frame i1 i2 g1 g2 = let width_margin = (g1 - i1) `div` 2; height_margin = (g2 - i2) `div` 2; in [(a,b) | a <- [width_margin .. i1 + width_margin - 1], b <- [height_margin .. i2 + height_margin - 1]]

checkerboard  img1@(Image e f _) img2@(Image g h _) = let
  min1 = min e g
  min2 = min f h
  min' = min min1 min2
  i1@(Image i _ _) = cropImage min' min' img1
  i2@(Image _ _ _) = cropImage min' min' img2
  in (makeCheckerImage i1 i2)
     where makeCheckerImage l m = do
             l' <- thawImage l
             let w = imageHeight l `div` 8
             let checkers1 =  concatMap (\i -> fmap (\(a,b) -> (a -1 ,b + i*w - 1)) [(x,y) | i <- [0,2,4,6], width <- [w], x <- [i*width..(i+1)*width], y <- [0..width]]) [1,3,5,7]
             let checkers2 =  concatMap (\i -> fmap (\(a,b) -> (a,b + i*w)) [(x,y) | i <- [1,3,5,7], width <- [w], x <- [i*width..(i+1)*width], y <- [0..width]]) [0,2,4,6]
             mapM (\(x,y) -> writePixel l' x y $ pixelAt m x y) $ checkers1 ++ checkers2
             freezeImage l'
             
zipImages :: Pixel a => Image a -> Image a -> Image a
zipImages i1@(Image g1 h1 d1) i2@(Image g2 h2 d2) = let
  gmin = min g1 g2
  hmin = min h1 h2
  (Image _ _ c1) = cropImage gmin hmin i1
  (Image _ _ c2) = cropImage gmin hmin i2
  in (Image gmin hmin $ V.izipWith (\i x y -> if odd i then x else y) c1 c2 )

dynCrop :: Int -> Int -> DynamicImage -> DynamicImage
dynCrop w h = dynamicPixelMap $ cropImage w h

cropImage :: Pixel a => Int -> Int -> Image a -> Image a
cropImage e f img@(Image g h _)
  | e >= g && f >= h = img
  | e >= g = generateImage (\x y -> pixelAt img x y) g f
  | f >= h = generateImage (\x y -> pixelAt img x y) e h  
  | True = generateImage (\x y -> pixelAt img x y) e f

cropImageOffset :: Pixel a => Int -> Int -> Int -> Int -> Image a -> Image a
cropImageOffset e1 e2 o1 o2 img = generateImage (\x y -> pixelAt img (x + o1) (y + o2)) e1 e2
