{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Grid where

import qualified Data.List.NonEmpty as NE
import Control.Concurrent
import Data.Functor.Identity (Identity(..))
import Data.Bool (bool)
import Control.Arrow ((***))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Data.Functor.Compose (Compose(..))
import Control.Comonad
import Data.Functor.Rep hiding (index)
import Control.Comonad.Cofree
import Data.Distributive
import qualified Data.Functor.Rep as R

import qualified Data.Vector as V
import Data.Vector.Mutable (IOVector)
import qualified Data.Sequence as S

import Lens.Micro.TH
import Lens.Micro.Platform
import Data.List
import Data.Ord
import Data.Maybe (fromMaybe)
import Data.Monoid


data Y a = Y { _yl :: S.Seq a
             , _yc :: Maybe a
             , _yi :: Int
             } deriving (Eq, Show)

newtype YY a = YY { _unyy :: Y (Y a) }
  deriving (Eq,Show)

makeLenses ''Y
makeLenses ''YY

data V a = V { _vl :: V.Vector a
             , _vc :: Maybe a
             , _vi :: Int
             } deriving (Eq, Show)

newtype VV a = VV { _unvv :: V (V a) }

makeLenses ''V
makeLenses ''VV

newtype VY a = VY { _unvy :: V (Y a) } deriving (Eq,Show)

makeLenses ''VY

data M a = M { _ml :: IOVector a
             , _mc :: Maybe a
             , _mi :: Int
             }

makeLenses ''M

newtype MY a = MY { _unmy :: M (Y a) }

makeLenses ''MY


-- | Class for a modular bounded container
--
-- Examples of functions provided for a simple one dimensional list, where appropriate
class Zipper z where
  type Index z
  data Direction z

  -- | Shift in a direction
  shift :: Direction z -> z a -> z a

  -- | Retrieve current cursor value
  cursor :: z a -> Maybe a

  -- | Retrieve current index value
  index :: z a -> Index z

  -- | Retrieve neighborhood of current cursor.
  neighborhood :: z a -> [a]

  -- | Destruct to list maintaining order of @(Index z)@, e.g. @(Z ls c rs) -> ls ++ [c] ++ rs@.
  toList :: z a -> [a]

  -- | Destruct a list into a mapping with indices
  toMap :: (Comonad z) => z a -> [(Index z, a)]
  toMap = toList . extend ((,) <$> index <*> extract)

  -- | Construct zipper from mapping (provide default value so this is always safe, no bottoms)
  fromMap :: Ord (Index z) => a -> [(Index z, a)] -> z a

  -- | Lookup by possibly denormalised index (still safe from modularity).
  --
  -- e.g. [1,2] ! 2 == 1
  (!) :: z a -> (Index z) -> a

  -- | Adjust value at specified index
  adjust :: (a -> a) -> Index z -> z a -> z a

  -- | Update value at specified index
  update :: a -> Index z -> z a -> z a
  update = adjust . const

  resize :: a -> z a -> (Index z) -> z a

  -- | Get size (maximum of @Index z@).
  size :: z a -> (Index z)

(?!) :: S.Seq a -> Int -> Maybe a
(?!) xs i
  | i < 0 = Nothing
  | length xs <= i = Nothing
  | True = Just (xs `S.index` i)

instance Zipper Y where
  type Index Y = Int
  data Direction Y = YL | YR deriving (Eq, Show)
  cursor = _yc
  index = _yi
  resize a y@(Y l c i) n = let s = size y in
    case compare s n of
      LT -> Y (l <> S.replicate (n - s) a) c i
      GT -> uncurry (Y (S.take n l)) $ if i < n then (c,i) else (Just a,n) 
      EQ -> y 
  size (Y l _ _) = S.length l 
  (!) (Y l c _) k = l `S.index` k
  adjust f k y@(Y l c i) = case l ?! k of
    Nothing  -> y
    (Just j) -> y { _yl = S.adjust f k l, _yc = if k == i then f <$> c else c  }
  toList (Y l c i) = foldr (:) [] l
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m = Y (S.fromList ys) (Just . snd $ minimumBy (comparing fst) m) 0
    where ys = fmap snd m
         
  shift d v@(Y l c i)
    | S.null l = v -- shifting length zero amounts to nothing
    | d == YL   = Y l ( l ?! (i - 1) ) (i - 1)
    | d == YR   = Y l ( l ?! (i + 1) ) (i + 1)

instance Functor Y where
  fmap f (Y l c i) = Y (fmap f l) (fmap f c) i

instance Comonad Y where
  extract = maybe (error "cursor not on grid") id . _yc 
  duplicate y = Y (S.fromFunction (size y - 1) fn) (Just y) ( y ^. yi )
    where fn k = compose (k + 1) ( shift YL ) y

compose :: Int -> (a -> a) -> (a -> a)
compose = (foldr (.) id .) . replicate

instance Zipper V where
  type Index V = Int
  data Direction V = VL | VR deriving (Eq, Show)

  cursor = _vc
  index = _vi
  size (V l _ _) = V.length l + 1
  (!) v k = (_vl v) V.! k
  adjust f k v@(V l c i) = case l V.!? k of
    Nothing  -> v
    (Just j) -> v { _vl = V.update l (V.fromList [(k,j)]), _vc = if k == i then f <$> c else c  }
  neighborhood (V l _ _)
    | V.length l <= 2 = V.toList l
    | otherwise       = map ((V.!) l) [0, V.length l - 1]
  toList (V l c i) = V.toList l
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m = V (V.fromList vs) (Just . snd $ minimumBy (comparing fst) m) 0
    where vs = fmap snd m
          
  shift d v@(V l c i)
    | V.null l = v 
    | d == VL   = V l (l V.!? (i - 1) ) (i-1)
    | d == VR   = V l (l V.!? (i + 1) ) (i+1)

instance Functor V where
  fmap f (V l c i) = V (fmap f l) (fmap f c) i

instance Zipper VY where
  type Index VY = (Int, Int)
  data Direction VY = VN | VE | VS | VW deriving (Eq, Show)
  cursor vy = cursor =<< (_vc $ _unvy vy)
  adjust f (a, b) vy@(VY v@(V l c r)) = case l V.!? a of
    Nothing  -> vy
    (Just p) -> let
      yc =  adjust f b p in
      shift VN $ shift VS $ VY $ v { _vl = V.update l (V.fromList [(a,yc)]) } 
  (!) z (x, y) = (_unvy z) ! x ! y
  resize a ( VY (v@(V l c i))) (n,m) = let s = length l
                                           nullRow = (Y (S.replicate m a) (Just a) 0)
                                           comparison = case compare s n of
                                             LT -> uncurry (V (V.take n l)) $ if i < n then (c,i) else (Just nullRow , n) 
                                             GT -> V (l <> V.replicate (n-s) nullRow) c i
                                             EQ -> v
                                       in VY $ fmap (\yrow -> resize a yrow m) comparison
  size z = (x, y)
    where x = z ^. unvy ^. to size
          y = maximum $ size <$> z ^. unvy ^. vl
  index z = (x, y)
    where x = z ^. unvy ^. vi
          y = maybe (error "out of bounds") _yi $ z ^. unvy ^. vc
  shift VE = (& unvy %~ shift VR)
  shift VW = (& unvy %~ shift VL)
  shift VN = (& unvy %~ fmap (shift YR))
  shift VS = (& unvy %~ fmap (shift YL))
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m  =  VY $ V (V.fromList cs) (Just $ head cs) 0
    where cs        = fmap (fromMap a) $ (fmap.fmap) (& _1 %~ snd) g 
          g         = groupBy (\a b -> (fst $ fst a) == (fst $ fst b)) m
          l         = length m

instance Zipper YY where
  type Index YY = (Int, Int)
  data Direction YY = YN | YE | YS | YW deriving (Eq, Show)

  cursor y = cursor =<< (_yc $ _unyy y)
  toList = foldl (++) [] . fmap toList . _yl . _unyy
  adjust f (a, b) yy@(YY y@(Y l c r)) = case l ?! a of
    Nothing  -> yy
    (Just p) -> let
      yc =  adjust f b p in
      shift YN $ shift YS $ YY $ y { _yl = S.update a yc l }
  (!) z (x, y) = (_unyy z) ! x ! y
  resize a ( YY (v@(Y l c i))) (n,m) = let s = length l
                                           nullRow = (Y (S.replicate m a) (Just a) 0)
                                           comparison = case compare n s of
                                             LT -> uncurry (Y (S.take n l)) $ if i < n then (c,i) else (Just nullRow , n) 
                                             GT -> Y (l <> S.replicate (n-s) nullRow) c i
                                             EQ -> v
                                       in YY $ fmap (\yrow -> resize a yrow m) comparison
  size z = (x, y)
    where x = z ^. unyy ^. to size
          y = maximum $ size <$> z ^. unyy ^. yl
  index z = (x, y)
    where x = z ^. unyy ^. yi
          y = maybe (error "out of bounds") _yi $ z ^. unyy ^. yc 
  shift YE = (& unyy %~ fmap (shift YR))
  shift YW = (& unyy %~ fmap (shift YL))
  shift YN = (& unyy %~ shift YR)
  shift YS = (& unyy %~ shift YL)
  {--neighborhood (YY (Y l c _)) = ns ++ ew
    where ns  = neighborhood c
          ewc = if (S.length l <= 2)
                   then F.toList l
                   else map (S.index l) [0, S.length l - 1]
          ew  = concatMap neighborhood' ewc
          neighborhood' z = (z ^. zc) : neighborhood z --}
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m  = YY $ Y (S.fromList cs) (Just $ head cs) 0
    where cs        = fmap (fromMap a) $ (fmap.fmap) (& _1 %~ snd) g 
          g         = groupBy (\a b -> (fst $ fst a) == (fst $ fst b)) m
          l         = length m

instance Functor VY where
  fmap f = VY . (fmap . fmap) f . _unvy

instance Functor YY where
  fmap f = YY . (fmap . fmap) f . _unyy

instance Comonad YY where
  extract = maybe (error "cursor not on grid")  id . cursor
  duplicate z = YY $ Y
    (fromF (xT - 1) mkCol) (Just $ Y (fromF (yT - 1) ( mkRow z)) (Just z) y) x
    where
      mkRow zx j = compose (j + 1) (shift YS) zx
      mkCol i    = let zx = compose (i + 1) (shift YW) z
                    in Y (fromF (yT - 1) (mkRow zx)) (Just zx) (zx ^. to index  ^. _2)
      (xT,yT)    = size z
      (x,y)      = index z
      fromF      = S.fromFunction

adjustYTo (i,j) = adjustYCol j . adjustYRow i
adjustYCol j y = y & (unyy . yl . mapped ) %~ (adjustYin j) & (unyy . yc . mapped ) %~ (adjustYin j)
adjustYRow i = YY . adjustYin i . _unyy
adjustYin a yy@(Y arr y _) = yy { _yc = arr ?! a
                                , _yi = a }



------------------------------------------------
tickTime :: Int
tickTime = 200000

--main :: IO ()
--main = loop (extend basicRule) start

loop :: (GridS Bool -> GridS Bool) -> GridS Bool -> IO ()
loop stepper g = do
  --putStr "\ESC[2J" -- Clear terminal screen
  putStrLn (render g)
  threadDelay tickTime
  loop stepper (stepper g)

newtype VBounded a = VBounded (V.Vector a)
  deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
  distribute = distributeRep

gridSize :: Int
gridSize = 40

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i
    | i < 0  = v V.! 0
    | i >= gridSize = v V.! (gridSize - 1)
    | True = v V.! i
  tabulate desc = VBounded $ V.generate gridSize desc

type GridS a = Store (Compose VBounded VBounded) a
type Coord = (Int, Int)

type Rule = GridS Bool -> Bool
type Rule2 = GridS String -> Bool

-- Offsets for the neighbouring 8 tiles, avoiding (0, 0) which is the cell itself
neighbourCoords :: [(Int, Int)]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

mkGridS2 :: [Coord] -> GridS String
mkGridS2 xs = store lookup (0, 0)
  where
    lookup crd = if crd `elem` xs then "blue" else "red"

mkGrid :: [Coord] -> GridS Bool
mkGrid xs = store lookup (0, 0)
  where
    lookup crd = crd `elem` xs

basicRule :: Rule
basicRule g =
  alive || (not alive && numNeighboursAlive >= 1)
  where
    alive = extract g
    addCoords (x, y) (x', y') = (x + x', y + y')
    neighbours = experiment (\s -> addCoords s <$> neighbourCoords) g
    numNeighboursAlive = length (filter id neighbours)

render :: GridS Bool -> String
render (StoreT (Identity (Compose g)) _) = foldMap ((++ "\n") . foldMap (bool "." "#")) g

pat :: [Coord] -> Coord -> [Coord]
pat xs (x, y) = fmap ((+x) *** (+y)) xs

glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

start :: GridS Bool
start = mkGrid $
     glider `pat` (0, 0)
  ++ beacon `pat` (15, 5)
  ++ blinker `pat` (16, 4)
