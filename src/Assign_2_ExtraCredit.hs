{- Assignment 1 Extra Credit
 - Name: Omar Alkersh
 - Date: TODO add of completion
 -}
module Assign_2_ExtraCredit where
import Data.List
import Data.Maybe

macid = "alkersho"

newtype Vector2 a = Vector2 (a,a)
  deriving (Show,Eq)
newtype Vector3 a = Vector3 (a,a,a)
  deriving (Show,Eq)
newtype Vector4 a = Vector4 (a,a,a,a)
  deriving (Show,Eq)

class VectorSpace v where
  vecZero       :: (Num a) => v a
  vecSum        :: (Num a) => v a -> v a -> v a
  vecScalarProd :: (Num a) => a -> v a -> v a
  vecMagnitude  :: (Floating a) => v a -> a
  vecInnerProd  :: (Num a) => v a -> v a -> a
  dim           :: (Num a) => Int -> v a -> a   --used to get individual values from vector
  dist          :: (Floating a) => v a -> v a -> a

vecF :: (Floating a, Ord a, VectorSpace v) => v a -> [v a] -> (v a, v a)
vecF v vs = let
  dists = map (dist v) vs
  in (vs !! fromJust (elemIndex (minimum dists) dists), vs !! fromJust ( elemIndex (maximum dists) dists))

instance VectorSpace Vector2 where

  dim x (Vector2 (a,b)) | x == 0 = a
                        | x == 1 = b

  vecZero = Vector2 (0, 0)

  vecSum v1 v2 = Vector2 (dim 0 v1 + dim 0 v2, dim 1 v1 + dim 1 v2)

  vecScalarProd x v = Vector2 (dim 0 v * x, dim 1 v * x)

  vecMagnitude v = sqrt( dim 0 v ^2 + dim 1 v ^2)

  vecInnerProd v1 v2 = dim 0 v1 * dim 0 v2 + dim 1 v1 * dim 1 v2

  dist v1 v2 = sqrt((dim 0 v1 - dim 0 v2)^2 + (dim 1 v1 - dim 1 v2)^2)

instance VectorSpace Vector3 where
  dim x (Vector3 (a,b,c)) | x == 0 = a
                          | x == 1 = b
                          | x == 2 = c

  vecZero = Vector3 (0, 0, 0)

  vecSum v1 v2 = Vector3 (dim 0 v1 + dim 0 v2, dim 1 v1 + dim 1 v2, dim 2 v1 + dim 2 v2)

  vecScalarProd x v = Vector3 (dim 0 v * x, dim 1 v * x, dim 2 v *x)

  vecMagnitude v = sqrt( dim 0 v ^2 + dim 1 v ^2 + dim 2 v ^2)

  vecInnerProd v1 v2 = dim 0 v1 * dim 0 v2 + dim 1 v1 * dim 1 v2 + dim 2 v1 * dim 2 v2

  dist v1 v2 = sqrt((dim 0 v1 - dim 0 v2)^2 + (dim 1 v1 - dim 1 v2)^2 + (dim 2 v1 - dim 2 v2)^2)


instance VectorSpace Vector4 where
  dim x (Vector4 (a,b,c,d)) | x == 0 = a
                            | x == 1 = b
                            | x == 2 = c
                            | x == 3 = d

  vecZero = Vector4 (0, 0, 0, 0)

  vecSum v1 v2 = Vector4 (dim 0 v1 + dim 0 v2, dim 1 v1 + dim 1 v2, dim 2 v1 + dim 2 v2, dim 3 v1 + dim 3 v2)

  vecScalarProd x v = Vector4 (dim 0 v * x, dim 1 v * x, dim 2 v *x, dim 3 v * x)

  vecMagnitude v = sqrt( dim 0 v ^2 + dim 1 v ^2 + dim 2 v ^2 + dim 3 v ^2)

  vecInnerProd v1 v2 = dim 0 v1 * dim 0 v2 + dim 1 v1 * dim 1 v2 + dim 2 v1 * dim 2 v2 + dim 3 v1 * dim 3 v2

  dist v1 v2 = sqrt((dim 0 v1 - dim 0 v2)^2 + (dim 1 v1 - dim 1 v2)^2 + (dim 2 v1 - dim 2 v2)^2 + (dim 3 v1 - dim 3 v2)^2)
