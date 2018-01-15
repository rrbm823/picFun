{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Action where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as S
import qualified Data.Vector as V hiding (modify,length)
import qualified Data.Vector.Mutable as V
import Data.Sequence (ViewL(..), ViewR(..),(<|),(|>))
import Data.Maybe (catMaybes)
import Data.List
import Data.Tree
import Lens.Micro.Platform
import Miso hiding (update)
import Miso.Subscription.Keyboard
import Miso.String (MisoString, (<>), pack)
import qualified Miso.Svg as Svg
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Web.Canvas
--import Math.Geometry.Grid
--import Math.Geometry.Grid.Square
import CanvasBS
import Model
import Grid

-- | Sum type for application events
data Action
  = HandleMouse (Int, Int)
  | GetArrows !Arrows
  | PickColor JSString
  | Rename JSString
  | Review JSString
  | Fill (VY JSString)
  | Fill2 (Int,Int)
  | Selected (Int,Int)
  | PickSpectrum
  | UpdatePic
  | SwitchFill
  | Begin
  | Paint
  | Alert
  | Id

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleMouse newCoords) m = noEff m { mouseCoords = newCoords }
--updateModel (GetArrows a@(Arrows x y)) m = (pure (Selected $ (selected m) + x - y*40)) #>  m { getArrows = a }
updateModel (GetArrows a@(Arrows x y)) m = let (i,j) = selected m in (pure $ Selected (i+x,j-y) ) #>  m { getArrows = a }
updateModel (PickColor p) m = noEff m {color = p}
updateModel (Selected i) m@(Model _ col _ _ y _ _ _ _) =
  (log_ (pack $ show $  update col i y ! i ) >> pure Id) #> m { selected = i
          , gridY = update col i y }
updateModel UpdatePic m = noEff m {
  store = M.insert (title m) (grid m) (store m)
  }
  where insert' j k l --if user doesnt want to rename (or to animate a set of frames..?)
          | j `elem` M.keys l = insert' (j <> "'") k l
          | True              = M.insert j k l       
updateModel PickSpectrum m = m <# do
  let (x,y) = mouseCoords m
  ctx <- getCtx "spectrum"
  p <- getPixel ctx x y
  return $ PickColor p
updateModel Alert m = (putStrLn "bingbong" >> pure Id) #> m
updateModel Begin m = m <# do
  getPic "../../image/First" "spectrum"
  noArrowScrolling
  return Id
updateModel (Rename r) m = noEff m { title = r }
updateModel (Review r) m = noEff m { title = r, grid = maybe mempty id $ M.lookup r (store m) }
updateModel SwitchFill m = noEff m { fillSwitch = not $ fillSwitch m }
updateModel (Fill2 i) m@(Model _ curColor cursor _ vy _ _ _ _) = do
  m <- mutateV $ _unvy $ (\c -> if c == vy ! i then (False,c) else (False,c)) <$> vy
  filled <- yySeek'' curColor $ MY m
  v' <- freezeM $ _unmy filled
  return (Selected i)
  return (Fill $ fmap snd $ VY v')
  #> m
updateModel (Fill v) m = noEff m 
--m { gridY = maybe yGrid id $ (fmap.fmap) (\(b,c) -> if b then curColor else c) $ foldl' merge' (Just yy) $ yyBuild (yGrid ! i) (adjustYTo i yy) }        
updateModel Paint m = m <# do
  ctx <- getCtx (title m)
  let g = gridY m
  draw ctx (40,40, toNumbBS $ toList g)
  return Id
updateModel Id m = noEff m

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

getPic p c = do
    ctx <- getCtx c
    canvasImage <- newImage
    drawImage' canvasImage ctx
    setSrc canvasImage p
    save ctx

foreign import javascript unsafe "document.getElementById('canvas').addEventListener($1, $2);"
  canvasAddEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "var ctx = $1; var x = $2; var y = $3; var pixel = ctx.getImageData(x, y, 1, 1);  var data = pixel.data; $r = 'rgb(' + data[0] + ', ' + data[1] + ', ' + data[2]  + ')'"
  getPixel :: Context -> Int -> Int -> IO JSString

foreign import javascript unsafe "$r = document.getElementById($1).getContext('2d');"
  getCtx :: MisoString -> IO Context

foreign import javascript unsafe "console.log($1);"
  log_ :: JSString -> IO ()

foreign import javascript unsafe "$1.onload = function () {$2.drawImage($1,0,0,$1.width,$1.height,0,0,550,406)}"
  drawImage' :: Image ->  Context -> IO ()

foreign import javascript unsafe "$r = new Image();"
  newImage :: IO Image

foreign import javascript unsafe "$1.src = $2;"
  setSrc :: Image -> MisoString -> IO ()

foreign import javascript unsafe "window.addEventListener('keydown', function(e) { if([32, 37, 38, 39, 40].indexOf(e.keyCode) > -1) { e.preventDefault();} }, false);"
  noArrowScrolling :: IO ()
