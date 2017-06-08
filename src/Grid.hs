{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Grid where

import Codec.Picture
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Storable as V

import Data.Monoid ((<>))
import qualified Data.Foldable as F
import Data.List (nub, intercalate)
import Data.Maybe (mapMaybe, fromMaybe)

import Data.Sequence (ViewL(..), ViewR(..), (|>), (<|), (><))
import qualified Data.Sequence as S
import Control.Comonad
import qualified Lens.Micro.Internal as L
import Lens.Micro
import Lens.Micro.TH

-- With this interpretation, for a board of size @n x n@
-- the @(n + 1)@th column/row is the same as the boundary at the @1@th column/row.
type Board = ZZ St

-- | Indexer for the 'Board'
type Cell = (Int, Int)

-- | Possible cell states
data St = Off | On
  deriving (Eq,Show)

-- | One dimensional finite list with cursor context
--
-- The first element of the sequence at '_zl' can be thought of
-- as /to the left/ of the cursor, while the last element is /to the right/
-- of the cursor. The cursor value and index are '_zc' and '_zi' respectively.
-- This can be thought of as a circle.
-- Warning: must have length greater than zero!
data Z a = Z { _zl :: S.Seq a
             , _zc :: a
             , _zi :: Int
             } deriving (Eq, Show)

newtype ZZ a = ZZ { _unzz :: Z (Z a) }
  deriving (Eq,Show)

makeLenses ''Z
makeLenses ''ZZ

vecToSeq v = S.fromFunction ( V.length v ) ((V.!) v)
--helper functions for indexing

data V a = V { _vl :: V.Vector a
             , _vc :: a
             , _vi :: Int
             } deriving (Eq, Show)

newtype VV a = VV { _unvv :: V (V a) }

makeLenses ''V
makeLenses ''VV

instance ZipperS V where
  type IndexS V = Int
  data DirectionS V = VL | VR deriving (Eq, Show)

  cursorS = _vc
  indexS = _vi
  sizeS (V l _ _) = V.length l + 1
  (!>) v k = v ^. vix k
  adjustS f k v = v & vix k %~ f
  neighborhoodS (V l _ _)
    | V.length l <= 2 = V.toList l
    | otherwise       = map ((V.!) l) [0, V.length l - 1]
  toListS (V l c i) = V.toList . V.reverse $ l <> (V.cons c f)
    where (f, b) = V.splitAt i l
  fromMapS _ [] = error "Zipper must have length greater than zero."
  fromMapS a m = V (V.fromList ys) (iToa 0) 0
    where ys = map iToa rng
          iToa i = fromMaybe a $ lookup i m
          l    = maximum . (0:) $ map fst m
          rng  = if l == 0 then [] else [l,(l-1)..1]
  shiftS d v@(V l c i)
    | V.null l = v -- shifting length zero amounts to nothing
    | d == VL   = V (V.snoc xs c) x xi
    | d == VR   = V (V.cons c ys) y yi
    where
      (x, xs) = (V.head l, V.tail l)
      (ys, y) = (V.init l, V.last l)
      xi        = (i - 1) `mod` sizeS v
      yi        = (i + 1) `mod` sizeS v

instance Zipper Z where
  type Index Z = Int
  data Direction Z = L | R deriving (Eq, Show)
  cursor = _zc
  index = _zi
  normalize z = (`mod` (size z))
  size (Z l _ _) = S.length l + 1
  (!) z k = z ^. zix k
  adjust f k z = z & zix k %~ f
  neighborhood (Z l _ _)
    | S.length l <= 2 = F.toList l
    | otherwise       = map (S.index l) [0, S.length l - 1]
  toList (Z l c i) = F.toList . S.reverse $ b >< (c <| f)
    where (f,b) = S.splitAt i l
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m  = Z (S.fromList ys) (iToa 0) 0
    where ys = map iToa rng
          iToa i = fromMaybe a $ lookup i m
          l    = maximum . (0:) $ map fst m
          rng  = if l == 0 then [] else [l,(l-1)..1]
  shift d z@(Z l c i)
    | S.null l = z -- shifting length zero amounts to nothing
    | d == L   = Z (xs |> c) x xi
    | d == R   = Z (c <| ys) y yi
    where
      (x :< xs) = S.viewl l
      (ys :> y) = S.viewr l
      xi        = (i - 1) `mod` size z
      yi        = (i + 1) `mod` size z

instance Functor Z where
  fmap f (Z l c i) = Z (fmap f l) (f c) i

instance Comonad Z where
  extract = cursor
  duplicate z = Z (S.fromFunction (size z - 1) fn) z (z ^. zi)
    where fn k = compose (k + 1) (shift L) $ z

-- | This interpretation is a 2D zipper (Z (Z a)).
--
-- The outer layer is a zipper of columns (x coordinate),
-- and each column is a zipper of @a@ values (y coordinate).
-- Warning: Keep inner column sizes consistent!
instance Zipper ZZ where
  type Index ZZ = (Int, Int)
  data Direction ZZ = N | E | S | W deriving (Eq, Show)

  cursor = _zc . _zc . _unzz
  toList = concatMap toList . toList . _unzz
  adjust f (x, y) z = z & (unzz . zix x . zix y) %~ f
  (!) z (x, y) = z ^. unzz ^. zix x ^. zix y
  normalize z (x,y) = (nx, ny)
    where nx = x `mod` z ^. to size ^. _1
          ny = y `mod` z ^. to size ^. _2
  size z = (x, y)
    where x = z ^. unzz ^. to size
          y = z ^. unzz ^. zc ^. to size
  index z = (x, y)
    where x = z ^. unzz ^. zi
          y = z ^. unzz ^. zc ^. zi
  shift E = (& unzz %~ shift R)
  shift W = (& unzz %~ shift L)
  shift N = (& unzz %~ fmap (shift R))
  shift S = (& unzz %~ fmap (shift L))
  neighborhood (ZZ (Z l c _)) = ns ++ ew
    where ns  = neighborhood c
          ewc = if (S.length l <= 2)
                   then F.toList l
                   else map (S.index l) [0, S.length l - 1]
          ew  = concatMap neighborhood' ewc
          neighborhood' z = (z ^. zc) : neighborhood z
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m  = ZZ $ Z (S.fromList cs) (iToc 0) 0
    where cs        = map iToc rc
          iToc i    = fromMap a . insDef . map (& _1 %~ snd) $ filter ((==i) . fst . fst) m
          l         = maximum . (0:) $ map (fst . fst) m
          h         = maximum . (0:) $ map (snd . fst) m
          rc        = if l == 0 then [] else [l,(l-1)..1]
          insDef xs = if h `elem` (map fst xs) then xs else (h,a) : xs

instance Functor ZZ where
  fmap f = ZZ . (fmap . fmap) f . _unzz

instance Comonad ZZ where
  extract = cursor
  duplicate z = ZZ $ Z
    (fromF (xT - 1) mkCol) (Z (fromF (yT - 1) (mkRow z)) z y) x
    where
      mkRow zx j = compose (j + 1) (shift S) zx
      mkCol i    = let zx = compose (i + 1) (shift W) z
                    in Z (fromF (yT - 1) (mkRow zx)) zx (zx ^. to index  ^. _2)
      (xT,yT)    = size z
      (x,y)      = index z
      fromF      = S.fromFunction

zix :: Int -> Lens' (Z a) a
zix k f z@(Z l c i) = maybe
  ((\x -> Z l x i) <$> f c)
  (\n -> (\x -> Z (S.update n x l) c i) <$> f (S.index l n)) (zToLix z k)

zToLix :: Z a -> Int -> Maybe Int
zToLix z@(Z _ _ i) k
  | i == n = Nothing
  | i < n  = Just $ s - (n - i) - 1
  | i > n  = Just $ i - n - 1
  where n = k `mod` s
        s = size z

vix :: V.Storable a => Int -> Lens' (V a) a
vix k f v@(V l c i ) = maybe
  ((\x -> V l x i) <$> f c)
  (\n -> (\x -> V (V.update_ l (V.singleton n) (V.singleton x)) c i) <$> f (l V.! n)) (vToLix v k)

vToLix :: V.Storable a => V a -> Int -> Maybe Int
vToLix z@(V l _ i) k
  | i == n = Nothing
  | i < n  = Just $ s - (n - i) - 1
  | i > n  = Just $ i - n - 1
  where n = k `mod` s
        s = V.length l + 1

compose :: Int -> (a -> a) -> (a -> a)
compose = (foldr (.) id .) . replicate

board :: Int -- ^ Length
      -> Int -- ^ Height
      -> [Cell] -- ^ List of cells initially alive
      -> Board
board l h = fromMap Off . ins (l-1,h-1) . map (,On)
  where ins m cs = if (m, On) `elem` cs
                   then cs
                   else ((m,Off):cs)

-- | Class for a modular bounded container
--
-- Examples of functions provided for a simple one dimensional list, where appropriate
class Zipper z where
  type Index z
  data Direction z

  -- | Shift in a direction
  shift :: Direction z -> z a -> z a

  -- | Retrieve current cursor value
  cursor :: z a -> a

  -- | Retrieve current index value
  index :: z a -> Index z

  -- | Retrieve neighborhood of current cursor.
  -- TODO consider keeping in Seq instead of []
  neighborhood :: z a -> [a]

  -- | Destruct to list maintaining order of @(Index z)@, e.g. @(Z ls c rs) -> ls ++ [c] ++ rs@.
  toList :: z a -> [a]

  -- | Destruct a list into a mapping with indices
  toMap :: (Comonad z) => z a -> [(Index z, a)]
  toMap = toList . extend ((,) <$> index <*> cursor)

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

  -- | Normalize @Index z@ value with respect to modular boundaries
  normalize :: z a -> (Index z) -> (Index z)

  -- | Get size (maximum of @Index z@).
  size :: z a -> (Index z)

class ZipperS z where
  type IndexS z
  data DirectionS z

  -- | Shift in a direction
  shiftS :: V.Storable a => DirectionS z -> z a -> z a

  -- | Retrieve current cursor value
  cursorS :: V.Storable a => z a -> a

  -- | Retrieve current index value
  indexS :: V.Storable a => z a -> IndexS z

  -- | Retrieve neighborhood of current cursor.
  -- TODO consider keeping in Seq instead of []
  neighborhoodS :: V.Storable a => z a -> [a]

  -- | Destruct to list maintaining order of @(Index z)@, e.g. @(Z ls c rs) -> ls ++ [c] ++ rs@.
  toListS :: V.Storable a => z a -> [a]

  -- | Destruct a list into a mapping with indices
  toMapS :: (V.Storable (IndexS z, a), V.Storable a, Comonad z) => z a -> [(IndexS z, a)]
  toMapS = toListS . extend ((,) <$> indexS <*> cursorS)

  -- | Construct zipper from mapping (provide default value so this is always safe, no bottoms)
  fromMapS :: (V.Storable a, Ord (Index z)) => a -> [(IndexS z, a)] -> z a

  -- | Lookup by possibly denormalised index (still safe from modularity).
  --
  -- e.g. [1,2] ! 2 == 1
  (!>) :: V.Storable a => z a -> (IndexS z) -> a

  -- | Adjust value at specified index
  adjustS :: V.Storable a => (a -> a) -> IndexS z -> z a -> z a

  -- | Update value at specified index
  updateS :: V.Storable a => a -> IndexS z -> z a -> z a
  updateS = adjustS . const

  -- | Normalize @Index z@ value with respect to modular boundaries
  normalizeS :: V.Storable a =>  z a -> (IndexS z) -> (IndexS z)

  -- | Get size (maximum of @Index z@).
  sizeS :: V.Storable a => z a -> (IndexS z)
