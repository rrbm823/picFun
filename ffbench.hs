{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe (catMaybes)
import Data.List
import Data.Tree
import Data.Monoid
import Data.Sequence (ViewL(..),ViewR(..), (<|), (|>))
import qualified Data.Sequence as S
import Data.Vector ((!?))
import qualified Data.Vector as V hiding (modify,length)
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Storable as MV
import Lens.Micro
import Grid  

main = do
  --let yyk = (False,) <$> yy
  m <- mutateV v
  filled <- yySeek'' $ MY m
  v' <- freezeM $ _unmy filled
  print $ (! 4) v'


f =  [1..128*128]
f' = let z = [i+128*j | i <- [5..11], j <- [0..70]] in (\x -> if x `elem` z then "blue" else "red") <$> f
g = M.fromList [((i,j), "red") | i <- [1..128], j <- [1..128]]
g' = foldl' ( flip $ M.update (const $ Just "blue")) g [(i,j) | i <- [5..11], j <- [0..70]]
h = V.fromList [ "red" | i <- [0..127*127]]
h' = h V.// [(i+127*j,"blue") | i <- [5..11], j <- [0..70]]
j = fromMap "red" [((i,j), "red") | i <- [0..9], j <- [0..9]] :: YY String
j' = foldl' ( flip $ update "blue" ) j $ [(i,j) | i <- [3,5], j <- [0..3]] ++ [(i,j) | i <- [0,4], j <- [2..5]] ++ [(6,6),(6,7),(7,7),(7,8)]
k = fromMap "red" [((i,j), "red") | i <- [0..511], j <- [0..511]] :: VY String
k' = foldl' ( flip $ update "blue" ) k $ [(i,j) | i <- [1,4,200], j <- [7..11]] ++ [(i,j) | i <- [5,99,244,404], j <- [3..99]]
l = fromMap "red" [((i,j), "red") | i <- [0..9], j <- [0..9]] :: VY String
l' = foldl' ( flip $ update "blue" ) l $ [(i,j) | i <- [3,7], j <- [1..4]] ++ [(i,j) | i <- [3..7], j <- [1,4]]

--yy@(YY y@(Y array (Just row) rowIndex)) = adjustYTo (4,2) l'

vy@(VY v@(V array (Just row) rowIndex)) = (\c -> if c == "red" then (False,True) else (False,False)) <$> k'

-- @(MY m@(Y array (Just row) rowIndex))

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

updateArray :: M (Y FCell) -> IO (M (Y FCell))
updateArray m@(M v (Just y) i) = do
  let newRow = yRowSeek'' y
  V.write v i newRow
  return m { _mc = Just newRow }

yySeek'' :: MY FCell -> IO (MY FCell)
yySeek'' !my@(MY m@(M array Nothing rowIndex)) = return my
yySeek'' !my@(MY m@(M array (Just row) rowIndex)) = do
  case _yc row of
      Nothing -> return my
      Just (True,_) -> return my
      Just (False,_) -> do
          mm <- updateArray m
          rowUp <- shiftMUp mm
          rowDown <- shiftMDown mm
          branch mm rowUp
          branch mm rowDown     
    where filterSeen = S.findIndicesL fst
          filterMatching = S.findIndicesL snd
          toRow = maybe (Y S.empty Nothing (-1)) id . _mc
          seeked = filterSeen . _yl . toRow
          toCheck =  filterMatching . _yl . toRow
          branch old ud = foldl' (>>=) (return $ MY ud) $ (flip fmap) (subSeqs (+1) (seeked old `intersect` (toCheck ud \\ seeked ud))) $ \case 
            [] -> return 
            i:_ -> yySeek'' . adjustMCCol i 
          --}

type ForestS a = [TreeS a]
data TreeS a = NodeS {
        rootLabel :: !a,         -- ^ label value
        subForest :: ForestS a   -- ^ zero or more child trees
     }  deriving (Eq, Read, Show)

instance Foldable TreeS where
    foldMap f (NodeS x ts) = f x `mappend` foldMap (foldMap f) ts

merge' :: Eq t =>  Maybe (YY (Bool, t)) -> Maybe (YY (Bool, t)) -> Maybe (YY (Bool, t))
merge' y1 Nothing = y1
merge' y1 (Just y2) = let mergeArrays (Y a1 _ _) (Y a2 _ _) = Y (S.zipWith (\(i1,i2) (j1,_) -> (or [i1, j1] , i2)) a1 a2) Nothing (-1)
           in  ( & (unyy . yl) %~ ( S.zipWith mergeArrays $ (y2 ^. unyy ^. yl) ) ) <$> y1

type FCell = (Bool,Bool)
yRowSeek'' :: (Y FCell) -> (Y FCell)
yRowSeek'' y@(Y row Nothing colIndex) = y
yRowSeek'' y@(Y row (Just (s,v)) colIndex)
  | s     = y
  | not v = y { _yl = S.adjust (\(a,b) -> (True,b)) colIndex row
              , _yc = Just (True,v) }
  | True  =
    let (l, r) = S.splitAt colIndex row;  
        seqRunL u (S.viewl -> S.EmptyL)  = S.empty
        seqRunL u (S.viewl -> (x :< xs)) = if snd x
                                           then u x <| seqRunL u xs
                                           else x <| xs
        seqRunR u (S.viewr -> S.EmptyR)  = S.empty
        seqRunR u (S.viewr -> (xs :> x)) = if snd x
                                           then seqRunR u xs |> u x
                                           else xs |> x
        updateRow = seqRunR (\(a,b) -> (True,b)) l <> seqRunL (\(a,b) -> (True,b)) r 
    in y { _yl = updateRow
         , _yc = Just (True,v) }

yySeek' !yy@(YY y@(Y array (Just row) rowIndex)) =
  case _yc row of
      Nothing -> NodeS Nothing [] 
      Just (True,_) -> NodeS Nothing [] 
      Just (False,_) -> NodeS (Just nuyy) (branch rowUp toCheckUp ++ branch rowDown toCheckDown)        
  where filterSeen = S.findIndicesL fst
        filterMatching = S.findIndicesL snd 
        toRow =  maybe (Y S.empty Nothing (-1)) id . _yc . _unyy
        updateArray = update (yRowSeek'' row) rowIndex
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
          i:_ -> yySeek' $ adjustYCol i ud { _unyy = updateArray $ _unyy ud }
  

printYY = mapM_ (print._yl) . _yl . _unyy :: YY (Bool, String)  -> IO () 

subSeqs f x = case x \\ t of 
    [] -> [t]
    d -> t:subSeqs f d
  where
    t = takeRun x
    takeRun [] = []
    takeRun (x:[]) = x:[] 
    takeRun (x:xs) = if f x == head xs then x : takeRun xs else x : []   

seqRunL :: Eq a => (a -> a) -> a -> S.Seq a -> S.Seq a
seqRunL u c x@(S.viewl -> S.EmptyL)  = S.empty
seqRunL u c (S.viewl -> (x :< xs)) = if c == x then u x <| seqRunL u c xs else x <| xs

seqRunR :: Eq a => (a -> a) -> a -> S.Seq a -> S.Seq a
seqRunR u c (S.viewr -> S.EmptyR)  = S.empty
seqRunR u c (S.viewr -> (xs :> x)) = if c == x then seqRunR u c xs |> u x else xs |> x

merge y1 y2 = let
  mergeArrays (Y a1 _ _) (Y a2 _ _) = Y (S.zipWith (\(i1,i2) (j1,_) -> (or [i1, j1] , i2)) a1 a2) Nothing (-1)
  in y1 & (unyy . yl) %~ ( S.zipWith mergeArrays (y2 ^. unyy ^. yl) )

yRowSeek' :: String -> (Y (Bool, String)) -> (Y (Bool, String))
yRowSeek' c y@(Y row Nothing colIndex) = y
yRowSeek' c y@(Y row (Just (s,v)) colIndex)
  | s        = y
  | c /= v   = y { _yl = S.adjust (\(a,b) -> (True,b)) colIndex row
                 , _yc = Just (True,v) }
  | True     = let (l, r) = S.splitAt colIndex row;
                   updateRow = seqRunR (\(a,b) -> (True,b)) (False,c) l <> seqRunL (\(a,b) -> (True,b)) (False,c) r 
               in y { _yl = updateRow
                    , _yc = Just (True,v) }


yySeek color !yy@(YY y@(Y array (Just row) rowIndex)) =
  case _yc row of
      Nothing -> Node Nothing [] 
      Just (True,_) -> Node Nothing [] 
      Just (False,_) -> Node (Just nuyy) (branch rowUp toCheckUp ++ branch rowDown toCheckDown)        
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
        branch ud [] = [Node Nothing []]
        branch ud chk =  (flip fmap) (subSeqs (+1) (filterSeen seeks `intersect` chk))
          $ \case 
          [] -> Node Nothing []
          i:_ -> yySeek color $ adjustYCol i ud { _unyy = updateArray $ _unyy ud }

adjustYTo (i,j) = adjustYCol j . adjustYRow i
adjustYCol j y = y & (unyy . yl . mapped ) %~ (adjustYin j) & (unyy . yc . mapped ) %~ (adjustYin j)
adjustYRow i = YY . adjustYin i . _unyy
adjustYin a yy@(Y arr y _) = yy { _yc = arr ?! a
                                , _yi = a }

rowSeek :: String -> (Index Y) -> (Y String) -> M.Map (Index YY) ()
rowSeek c i y@(Y row Nothing colIndex) = M.empty
rowSeek c i y@(Y row (Just v) colIndex)
  | c /= v   = M.empty
  | True     = let (l, r) = S.splitAt colIndex row;
                   rightMatch = maybe (length row) (colIndex+) $ S.findIndexL (/= c) r;
                   leftMatch = maybe 0 (+1) $ S.findIndexR (/= c) l; 
               in M.fromList $ fmap (\c -> ((i,c),())) [leftMatch .. rightMatch - 1]

ySeek color stack yy@(YY y@(Y array (Just row) rowIndex)) =
  case _yc row of
      Nothing -> M.empty 
      _ -> M.union newStack $ M.union (branch rowUp toCheckUp) (branch rowDown toCheckDown)      
  where for = flip fmap        
        matched = rowSeek color rowIndex row
        newStack = M.union matched stack
        rowSeen = fmap snd $ filter ((==rowIndex).fst) $ M.keys newStack
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
          i:_ -> if M.member (index $ _unyy ud, i) newStack then M.empty else ySeek color newStack (adjustYCol i ud)


--------------------------------------
type Grid = M.Map Int String 

type GridV = V.Vector Bool

type Grid3 = [(Bool,String)]

data GridPos a = GridPos {pos :: a, neighbors :: [GridPos a]} --hmm

type Grid2 = M.Map (Int,Int) String

type Stack2 = M.Map (Int,Int) ()

type Stack = M.Map Int ()

addToStack :: Stack -> Int -> Stack
addToStack = flip $ flip M.insert ()

addToStack2 :: Stack2 -> (Int,Int) -> Stack2
addToStack2 = flip $ flip M.insert ()

getFillPixels :: [Int] -> Stack -> String -> Grid -> Stack
getFillPixels [] stack c g = M.empty
getFillPixels (i:is) stack c g = let
  neighbors = rectNeighbors i 40
  notSeen = filter (`notElem` (M.keys stack)) $ catMaybes $ (\j -> stepFill j c g) <$> neighbors
  newStack = foldl' addToStack stack notSeen
  in M.union newStack (getFillPixels (is ++ notSeen) newStack c g)

getFillPixels2 :: [(Int,Int)] -> Stack2 -> String -> Grid2 -> Dir -> Stack2
getFillPixels2 [] stack c g _ = M.empty
getFillPixels2 (i:is) stack c g d = let
  neighbors = rectNeighbors2 i 128 128 d
  notSeen = filter (`notElem` (M.keys stack)) $ catMaybes $ (\j -> stepFill2 j c g) <$> neighbors
  newStack = foldl' addToStack2 stack notSeen
  in M.union newStack (getFillPixels2 (is ++ notSeen) newStack c g d)

getFillPixels3 :: [Int] -> Stack -> String -> GridV -> Stack
getFillPixels3 [] stack c grid = M.empty
getFillPixels3 (i:is) stack c grid = let
  g = length grid
  l = round $ sqrt $ fromInteger $ toInteger g
  neighbors
    | i < l = filter (>= 0) $ rectNeighbors i l
    | i > g - l = filter (< g) $ rectNeighbors i l
    | True = rectNeighbors i l
  notSeen = filter (`notElem` (M.keys stack)) $ catMaybes $ (\j -> stepFill3 j grid) <$> neighbors
  newStack = foldl' addToStack stack notSeen
  in M.union newStack (getFillPixels3 (is ++ notSeen) newStack c grid)

rectNeighbors :: Int -> Int -> [Int]
rectNeighbors i d
  | i `mod` d == 0     = [i+1,i-d,i+d]
  | i `mod` d == (d-1) = [i-1,i-d,i+d]
  | True               = [i-1,i+1,i-d,i+d] 

stepFill3 :: Int -> GridV -> Maybe Int
stepFill3 i g = let j = g V.! i in
  if j
  then Just i
  else Nothing

stepFill2 :: (Int,Int) -> String -> Grid2 -> Maybe (Int,Int)
stepFill2 i c0 grid =
  let j = M.lookup i grid in
    if (Just c0 == j)
    then Just i
    else Nothing
     
stepFill :: Int -> String -> Grid -> Maybe Int
stepFill i c0 grid =
  let j = M.lookup i grid in
    if (Just c0 == j)
    then Just i
    else Nothing

step :: Int -> String -> (Bool,String) -> Maybe Int
step i c0 (False,c1)
  | c0 == c1 = Just i
  | True     = Nothing
step i c0 (True,c1) = Nothing

data Dir = LeftWay | RightWay

rectNeighbors2 :: (Int,Int) -> Int -> Int -> Dir -> [(Int,Int)]
rectNeighbors2 (i,j) t u LeftWay
  | i > 0 && j > 0 = [(i,j+1),(i-1,j),(i,j-1)]
  | i == 0 = []
  | j == 0 = []
rectNeighbors2 (i,j) t u RightWay
  | i < t && j < u = [(i+1,j),(i,j+1),(i,j-1)]
  | i == t = []
  | j == u = []

(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
  | i < 0 = Nothing
  | True = let d = length xs in if d <= i then Nothing else Just (xs !! i)
