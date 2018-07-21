{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Action where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as S
import qualified Data.JSString as J
import qualified Data.Vector.Generic
import qualified Data.Vector as V hiding (modify,length)
import qualified Data.Vector.Mutable as V
import Data.Sequence (ViewL(..), ViewR(..),(<|),(|>))
import Data.Maybe (catMaybes)
import Data.List
import Data.Tree
import Data.JSString (unpack)
import Lens.Micro.Platform
import Control.Concurrent.MVar
import Miso hiding (update)
import Miso.Subscription.Keyboard
import Miso.String (MisoString, (<>), pack)
import qualified Miso.Svg as Svg
import JavaScript.Web.Canvas
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
import CanvasBS
import Model
import Grid
import Foreign

-- | Sum type for application events
data Action
  = HandleMouse (Int, Int)
  | GetArrows !Arrows
  | PickColor JSString
  | Rename JSString
  | Review JSString
  | RedrawGrid (YY JSString)
  | Fill (Int,Int)
  | Opacity JSString
  | Selected (Int,Int)
  | PixelResize Int
  | PickSpectrum
  | UpdatePic
  | UpdateGrid
  | SwitchFill
  | ReadFile
  | Begin
  | PixelPaint
  | Paint
  | Id

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleMouse newCoords) m = noEff m { mouseCoords = newCoords }

updateModel (GetArrows a@(Arrows x y)) m = let (i,j) = selected m in (pure $ Selected (i-y,j+x) ) #>  m { getArrows = a }

updateModel (PickColor p) m = noEff m {color = p}

updateModel (Selected i) m@(Model _ col _ _ _ y _ _ _ _) = noEff m { selected = i, gridY = update col i y }

updateModel UpdatePic m = noEff m {
  store = M.insert (title m) (grid m) (store m)
  }
  where insert' j k l --if user doesnt want to rename (or to animate a set of frames..?)
          | j `elem` M.keys l = insert' (j <> "'") k l
          | True              = M.insert j k l       

updateModel (PixelResize i) m = noEff m { pix = i }

updateModel PickSpectrum m = m <# do
  let (x,y) = mouseCoords m
  ctx <- getCtx "spectrum"
  p <- getPixel ctx x y
  return $ PickColor p

updateModel (RedrawGrid i) m = noEff m {gridY = i }

updateModel (Rename r) m = noEff m { title = r }

updateModel (Review r) m = noEff m { title = r, grid = maybe mempty id $ M.lookup r (store m) }

updateModel SwitchFill m = noEff m { fillSwitch = not $ fillSwitch m }

updateModel (Fill i) m@(Model _ curColor cursor _ _ yy _ _ _ _) = do
{--  m <- mutateV $ _unvy $ (\c -> if c == vy ! i then (False,c) else (False,c)) <$> vy
  filled <- yySeek'' curColor $ MY m
  v' <- freezeM $ _unmy filled
  return (Fill $ fmap snd $ VY v')
--}
  return Id -- (Selected i)
  #> m { gridY = maybe yy id $ (fmap.fmap) (\(b,c) -> if b then curColor else c) $ foldl' merge' (Just yGrid) $ yyBuild (yy ! i) (adjustYTo i yGrid) }
  where yGrid = fmap (False,) yy
  
updateModel Paint m = m <# do
  let (d,e) = size $ gridY m
  
  ctx <- getCtx (title m)
  let g = gridY m
  let p = pix m
  v <- toJSVal $ toNumbBSN' p d $ toList g
  --log_ v
  --draw ctx (p*d, p*e, toNumbBSN p d $ toList g)
  drawImageData ctx (p*d) (p*e) (v)
  return Id

updateModel ReadFile m = m <# do
  let (d,e) = size $ gridY m
  
  g <- readImageData d e
  return $ RedrawGrid g
  
updateModel Id m = noEff m

updateModel Begin m = m <# do
  getPic "../../image/First" "spectrum"
  noArrowScrolling
  return Id

readImageData d e = do
  fileReaderInput <- getElementById "fileReader"
  file <- getFile fileReaderInput
  reader <- newReader
  canvas <- getCtx "hiddenCanvas"
  g <- newEmptyMVar
  setOnLoad1 reader =<< do
    img <- newImage
    cb <- asyncCallback $ do
        drawImage img 0 0 d e canvas
        picdata <- getPixels canvas 0 0 e d
        r <- return $ fromBSArray picdata
        putMVar g (fromMap "brown" $ zip [(x,y) | x <- [0..(d-1)], y <- [0..(e-1)]] r :: YY JSString)
    imgSetOnLoad img cb
    asyncCallback1 $ \file -> do
      r <- readResult file
      setSrc img r
  readResultURL reader file
  readMVar g
  
getPic p c = do
  ctx <- getCtx c
  canvasImage <- newImage
  drawImage' canvasImage ctx
  setSrc canvasImage p
  save ctx

















--------bad----code--------
type FCell = (Bool,JSString)

mutateV :: V a -> IO (M a)
mutateV v@(V l c i) = do
  m <- V.thaw l
  return $ M m c i

freezeM :: M a -> IO (V a)
freezeM m@(M l c i) = do
  v <- V.freeze l
  return $ V v c i

adjustMTo (i,j) m = adjustMVCol j =<< adjustMRow i m

adjustMVCol :: Int -> MY a -> IO ()
adjustMVCol j m = V.modify ( _ml $ _unmy m ) (adjustYin j) (_mi $ _unmy m)

adjustMCCol j m = m & ( unmy . mc . mapped ) %~  adjustYin j

adjustMRow :: Int -> MY a -> IO (MY a)
adjustMRow i my = do
  r <- adjustMin i $ _unmy my
  return $ MY r

adjustMin :: Int -> M a -> IO (M a)
adjustMin a m@(M v y _) = do
  c <- v `V.read` a
  return $ m { _mc = Just $ c
             , _mi = a }

shiftMDown m@(M _ _ i)
  | i <= 0 = return $ m {_mc = Nothing, _mi = 0}
  | True = adjustMin (_mi m - 1) m
    
shiftMUp m@(M v _ i) = let l = V.length v in
  if i < l - 1 then adjustMin (_mi m + 1) m 
    else return $ m {_mc = Nothing, _mi = l}

updateArray :: JSString -> M (Y FCell) -> IO (M (Y FCell))
updateArray c m@(M v (Just y) i) = do
  let newRow = yRowSeek'' c y
  V.write v i newRow
  return m { _mc = Just newRow }

yRowSeek'' :: JSString -> (Y FCell) -> (Y FCell)
yRowSeek'' c y@(Y row Nothing colIndex) = y
yRowSeek'' c y@(Y row (Just (s,v)) colIndex)
  | s      = y
  | c /= v = y { _yl = S.adjust (\(a,b) -> (True,b)) colIndex row
               , _yc = Just (True,v) }
  | True   =
    let (l, r) = S.splitAt colIndex row;  
        seqRunL u (S.viewl -> S.EmptyL)  = S.empty
        seqRunL u (S.viewl -> (x :< xs)) = if c == snd x
                                           then u x <| seqRunL u xs
                                           else x <| xs
        seqRunR u (S.viewr -> S.EmptyR)  = S.empty
        seqRunR u (S.viewr -> (xs :> x)) = if c == snd x
                                           then seqRunR u xs |> u x
                                           else xs |> x
        updateRow = seqRunR (\(a,b) -> (True,b)) l <> seqRunL (\(a,b) -> (True,b)) r 
    in y { _yl = updateRow
         , _yc = Just (True,v) }

yySeek'' :: JSString -> MY FCell -> IO (MY FCell)
yySeek'' c !my@(MY m@(M array Nothing rowIndex)) = return my
yySeek'' c !my@(MY m@(M array (Just row) rowIndex)) = do
  case _yc row of
      Nothing -> return my
      Just (True,_) -> return my
      Just (False,_) -> do
          mm <- updateArray c m
          rowUp <- shiftMUp mm
          rowDown <- shiftMDown mm
          branch mm rowUp
          branch mm rowDown     
    where filterSeen = S.findIndicesL fst
          filterMatching = S.findIndicesL ((/= c) . snd)
          toRow = maybe (Y S.empty Nothing (-1)) id . _mc
          seeked = filterSeen . _yl . toRow
          toCheck =  filterMatching . _yl . toRow
          branch old ud = foldl' (>>=) (return $ MY ud) $ (flip fmap) (subSeqs (+1) (seeked old `intersect` (toCheck ud \\ seeked ud))) $ \case 
            [] -> return 
            i:_ -> yySeek'' c . adjustMCCol i

-------------------------

yRowSeek :: JSString -> (Index Y) -> (Y JSString) -> M.Map (Index YY) ()
yRowSeek c i y@(Y row Nothing colIndex) = M.empty
yRowSeek c i y@(Y row (Just v) colIndex)
  | c /= v   = M.empty
  | True     = let (l, r) = S.splitAt colIndex row;
                   rightMatch = maybe (length row) (colIndex+) $ S.findIndexL (/= c) r;
                   leftMatch = maybe 0 (+1) $ S.findIndexR (/= c) l; 
               in M.fromList $ fmap (\c -> ((i,c),())) [leftMatch .. rightMatch -1]

yySeek color stack yy@(YY y@(Y array (Just row) rowIndex)) =
  case _yc row of
      Nothing -> M.empty 
      _ -> M.union newStack $ M.union (branch rowUp toCheckUp) (branch rowDown toCheckDown)      
  where for = flip fmap
        rowSeen = fmap snd $ filter ((==rowIndex).fst) $ M.keys newStack
        matched = yRowSeek color rowIndex row
        newStack = M.union matched stack
        filterMatching = S.findIndicesL (color==)
        toRow =  maybe (Y S.empty Nothing (-1)) id . _yc . _unyy
        rowUp = shift YN yy
        rowDown = shift YS yy
        toCheckUp =  filterMatching $ _yl $ toRow rowUp 
        toCheckDown =  filterMatching $ _yl $ toRow rowDown
        --begin recursion
        branch ud [] = M.empty 
        branch ud chk =  foldl' M.union M.empty $ for (subSeqs (+1) (rowSeen `intersect` chk))
          $ \case 
          [] -> M.empty
          i:_ -> if M.member (index $ _unyy ud, i) newStack then M.empty else yySeek color newStack (adjustYCol i ud)
















----------------------------------------
yRowSeek' :: JSString -> (Y (Bool, JSString)) -> (Y (Bool, JSString))
yRowSeek' c y@(Y row Nothing colIndex) = y
yRowSeek' c y@(Y row (Just (s,v)) colIndex)
  | s        = y
  | c /= v   = y { _yl = S.adjust (\(a,b) -> (True,b)) colIndex row
                 , _yc = Just (True,v) }
  | True     = let (l, r) = S.splitAt colIndex row;
                   updateRow = seqRunR (\(a,b) -> (True,b)) (False,c) l <> seqRunL (\(a,b) -> (True,b)) (False,c) r 
               in y { _yl = updateRow
                    , _yc = Just (True,v) }

--disconnected runs in the array are partitioned with subSeqs

subSeqs :: (Eq a, Num a) => (a -> a) -> [a] -> [[a]]
subSeqs f x = case x \\ t of 
    [] -> [t]
    d -> t:subSeqs f d
  where
    t = takeRun x
    takeRun [] = []
    takeRun (x:[]) = x:[] 
    takeRun (x:xs) = if f x == head xs then x : takeRun xs else x : []
    
type ForestS a = [TreeS a]
data TreeS a = NodeS {
        rootLabel :: !a,         -- ^ label value
        subForest :: ForestS a   -- ^ zero or more child trees
     }  deriving (Eq, Read, Show)

instance Foldable TreeS where
    foldMap f (NodeS x ts) = f x `mappend` foldMap (foldMap f) ts                    

yyBuild color !yy@(YY y@(Y array (Just row) rowIndex)) =
  case _yc row of
      Nothing -> NodeS Nothing [] 
      Just (True,_) -> NodeS Nothing [] 
      Just (False,_) -> NodeS (Just nuyy) (branch rowUp toCheckUp ++ branch rowDown toCheckDown)        
  where filterSeen = S.findIndicesL fst
        filterMatching = S.findIndicesL ((color==).snd)
        toRow =  maybe (Y S.empty Nothing (-1)) id . _yc . _unyy
        updateArray = update (yRowSeek' color row) rowIndex
        nuyy = yy { _unyy = updateArray y }
        rowUp = shift YN nuyy
        rowDown = shift YS nuyy
        seeks = _yl $ toRow nuyy
        toCheckUp =  filterMatching $ _yl $ toRow rowUp 
        toCheckDown =  filterMatching $ _yl $ toRow rowDown
        --begin recursion
        branch ud [] = [NodeS Nothing []]
        branch ud chk =  (flip fmap) (subSeqs (+1) (filterSeen seeks `intersect` chk))
          $ \case 
          [] -> NodeS Nothing []
          i:_ -> yyBuild color $ adjustYCol i ud { _unyy = updateArray $ _unyy ud }

seqRunL :: Eq a => (a -> a) -> a -> S.Seq a -> S.Seq a
seqRunL u c x@(S.viewl -> S.EmptyL)  = S.empty
seqRunL u c (S.viewl -> (x :< xs)) = if c == x then u x <| seqRunL u c xs else x <| xs

seqRunR :: Eq a => (a -> a) -> a -> S.Seq a -> S.Seq a
seqRunR u c (S.viewr -> S.EmptyR)  = S.empty
seqRunR u c (S.viewr -> (xs :> x)) = if c == x then seqRunR u c xs |> u x else xs |> x

merge' :: Eq t =>  Maybe (YY (Bool, t)) -> Maybe (YY (Bool, t)) -> Maybe (YY (Bool, t))
merge' y1 Nothing = y1
merge' y1 (Just y2) = let mergeArrays (Y a1 _ _) (Y a2 _ _) = Y (S.zipWith (\(i1,i2) (j1,_) -> (or [i1, j1] , i2)) a1 a2) Nothing (-1)
           in  ( & (unyy . yl) %~ ( S.zipWith mergeArrays $ (y2 ^. unyy ^. yl) ) ) <$> y1
