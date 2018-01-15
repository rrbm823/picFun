{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Servant
import Servant.API
import Network.Wai.Handler.Warp
import Codec.Picture.Types
import Codec.Picture
import Pixels
import Models
import Htmls
import Api
import Automata
import Data.ByteString as B (readFile)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans

imagApp :: IO ()
imagApp = do
  left <- atomically $ newTVar $ ImageRGB8 $ generateImage (const.const $ PixelRGB8 0 200 0) 300 300
  right <- atomically $ newTVar $ ImageRGB8 $ generateImage (const.const $ PixelRGB8 0 0 0) 300 300
  run 8080 $ serve imageApi (zipSrv left right)

svgApp :: IO ()
svgApp = run 8081 $ serve svgApi $
  serveSvg :<|> return SVGFrontend :<|> serveDirectory "static" where
  serveSvg n = liftIO $ do
    return $ svg . grid . map (window n 0) . take n . loop (rule 110) $ Store (==0) (0 :: Int)

zipSrv :: TVar DynamicImage -> TVar DynamicImage -> Server ImageAPI
zipSrv i1 i2 = postImg :<|> image :<|> gif :<|> (return . read) :<|> serveDirectory "static"
  where
    postImg e p
      | e = liftIO $ do
          print "modifying left img"
          atomically $ do
            modifyTVar i1 (const p)
            -- readTVar i1
            return e
      | True = liftIO $ do
          print "modifying right img"
          atomically $ do
            modifyTVar i2 (const p)
            -- readTVar i2
            return e
    gif r = case read r of
       Rave -> liftIO $ do
         img <- readTVarIO i1
         return $ take 7 $ iterate (brightnessRGB8 (10)) (convertRGB8 img)
       Checkerboard ->  liftIO $ do
         leftImg <- readTVarIO i1
         rightImg <- readTVarIO i2
         img1 <- checkerboard (convertRGB8 leftImg) (convertRGB8 rightImg)
         img2 <- checkerboard (convertRGB8 rightImg) (convertRGB8 leftImg)
         return $ [img1, img2]
       Spiral -> liftIO $ do
         leftImg <- readTVarIO i1
         rightImg <- readTVarIO i2
         mapM (\s -> sprlImages s (convertRGB8 leftImg) (convertRGB8 rightImg)) $ take 10 (iterate (+0.3) 0.0)
       ZipImage -> liftIO $ do
         leftImg <- readTVarIO i1
         rightImg <- readTVarIO i2
         mapM (\n -> zipImages n (convertRGB8 leftImg) (convertRGB8 rightImg)) [2..10]
    image r = case read r of
       Draw -> liftIO $ do
         img <- readTVarIO i1
         return $ ImageRGB8 $ convertRGB8 img
       ZipImage -> liftIO $ do
         leftImg <- readTVarIO i1
         rightImg <- readTVarIO i2
         imgResult <- zipImages 2 (convertRGB8 leftImg) (convertRGB8 rightImg)
         return $ ImageRGB8 imgResult
       Spiral -> liftIO $ do
         leftImg <- readTVarIO i1
         rightImg <- readTVarIO i2
         imgResult <- sprlImages 0 (convertRGB8 leftImg) (convertRGB8 rightImg)
         return $ ImageRGB8 imgResult
       Frame -> liftIO $ do
         leftImg <- readTVarIO i1
         rightImg <- readTVarIO i2
         imgResult <- imageInImage (convertRGB8 leftImg) (convertRGB8 rightImg) 
         return $ ImageRGB8 imgResult
       Checkerboard ->  liftIO $ do
         leftImg <- readTVarIO i1
         rightImg <- readTVarIO i2
         imgResult <- checkerboard (convertRGB8 leftImg) (convertRGB8 rightImg) 
         return $ ImageRGB8 imgResult
       First -> liftIO $ do
         leftImg <- readTVarIO i1
         r <- readPng "static/image/spectrum.png"
         case r of
           (Right j) -> return j
           (Left l) -> return leftImg
         
                                                                   
