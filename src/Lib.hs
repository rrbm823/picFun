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
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans

imagApp :: IO ()
imagApp = do
  left <- atomically $ newTVar $ ImageRGB8 $ generateImage (const.const $ PixelRGB8 0 200 0) 300 300
  right <- atomically $ newTVar $ ImageRGB8 $ generateImage (const.const $ PixelRGB8 0 0 0) 300 300
  run 8080 $ serve imageApi (zipSrv left right)

zipSrv :: TVar DynamicImage -> TVar DynamicImage -> Server ImageAPI
zipSrv i1 i2 = postImg :<|> display :<|> (return . read)
  where
    postImg e p
      | e = liftIO $ atomically $ do
          modifyTVar i1 (const p)
          readTVar i1
          return e
      | True = liftIO $ atomically $ do
          modifyTVar i2 (const p)
          readTVar i2
          return e
    display r = case read r of
       ZipImage -> liftIO $ do
         leftImg <- readTVarIO i1
         rightImg <- readTVarIO i2
         return $ ImageRGB8 $ zipImages (convertRGB8 leftImg) (convertRGB8 rightImg)
       Spiral -> liftIO $ do
         leftImg <- readTVarIO i1
         rightImg <- readTVarIO i2
         imgResult <- sprlImages (convertRGB8 leftImg) (convertRGB8 rightImg)
         return $ ImageRGB8 imgResult
       Frame -> liftIO $ do
         leftImg <- readTVarIO i1
         rightImg <- readTVarIO i2
         imgResult <- imageInImage (convertRGB8 leftImg) (convertRGB8 rightImg) 
         return $ ImageRGB8 imgResult
