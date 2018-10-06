{- Assignment 2
 - Name: Omar Alkersh
 - Date: 24/10/18
 -}
module Assign_2 where
import Data.List
import Data.Maybe

macid :: String
macid = "alkersho"

type Vector = (Double,Double,Double)


dim (x1,x2,x3) n
  | n == 0 = x1
  | n == 1 = x2
  | n == 2 = x3
{- -----------------------------------------------------------------
 - vecZero
 - -----------------------------------------------------------------
 - Description: Returns a zero vector, vector with 0 Magnitude
 -}
vecZero :: Vector
vecZero = (0.0,0.0,0.0)

{- -----------------------------------------------------------------
 - vecScalarProd
 - -----------------------------------------------------------------
 - Description: multiplies the vector by scalar quantity
 -}
vecScalarProd :: Double -> Vector -> Vector
vecScalarProd x v0 = (x*dim v0 0, x*dim v0 1, x*dim v0 2)

{- -----------------------------------------------------------------
 - vecSum
 - -----------------------------------------------------------------
 - Description: Adds two vectors together, performs vector addition
 -}
vecSum :: Vector -> Vector -> Vector
vecSum v0 v1 = (dim v0 0 + dim v1 0, dim v0 1 + dim v1 1, dim v0 2 + dim v1 2)

{- -----------------------------------------------------------------
 - vecMagnitude
 - -----------------------------------------------------------------
 - Description: Finds the magnitude of a vector
 -}
vecMagnitude :: Vector -> Double
vecMagnitude v0 = sqrt( dim v0 0 ^2+ dim v0 1 ^2+ dim v0 2 ^2)

{- -----------------------------------------------------------------
 - vecInnerProd
 - -----------------------------------------------------------------
 - Description: Finds dot product of two vectors
 -}
vecInnerProd :: Vector -> Vector -> Double
vecInnerProd v0 v1 = dim v0 0 * dim v1 0 + dim v0 1 * dim v1 1 + dim v0 2 * dim v1 2

{- -----------------------------------------------------------------
 - vecF
 - -----------------------------------------------------------------
 - Description: Finds the closest vector in a list of vecto,vs, and the furthest from vector v0
 -}
vecF :: Vector -> [Vector] -> (Vector,Vector)
vecF v0 vs = let
  dists = map (\ v1 -> sqrt((dim v0 0 - dim v1 0)^2+(dim v0 0 - dim v1 0)^2+(dim v0 0 - dim v1 0)^2)) vs
  in (vs !! fromJust (elemIndex (minimum dists) dists), vs !! fromJust ( elemIndex (maximum dists) dists))

{- -----------------------------------------------------------------
 - Test Cases - vecScalarProd
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 1-A
 - - Input: 5 (1.0,2.0,3.0)
 - - Expected Output: (5.0,10.0,15.0)
 - - Acutal Output: (5.0,10.0,15.0)
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 1-B
 - - Input: -5 (1.0,2.0,3.0)
 - - Expected Output: (-5.0,-10.0,-15.0)
 - - Acutal Output: (-5.0,-10.0,-15.0)
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 1-C
 - - Input: -5 (-1.0,-2.0,3.0)
 - - Expected Output: (5.0,10.0,-15.0)
 - - Acutal Output: (5.0,10.0,-15.0)
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
  - Test Cases - vecSum
  - -----------------------------------------------------------------
  -
  - -----------------------------------------------------------------
  - - Function: vecSum
  - - Test Case Number: 2-A
  - - Input: (1.0,2.0,3.0) (1.0,2.0,3.0)
  - - Expected Output: (2.0,4.0,6.0)
  - - Acutal Output: (2.0,4.0,6.0)
  - -----------------------------------------------------------------
  -
  - -----------------------------------------------------------------
  - - Function: vecSum
  - - Test Case Number: 2-B
  - - Input: (1.0,-2.0,3.0) (1.0,2.0,-3.0)
  - - Expected Output: (2.0,0.0,0.0)
  - - Acutal Output: (2.0,0.0,0.0)
  - -----------------------------------------------------------------
  -
  - -----------------------------------------------------------------
  - - Function: vecSum
  - - Test Case Number: 2-C
  - - Input: (-1.0,-2.0,-3.0) (-1.0,-2.0,-3.0)
  - - Expected Output: (-2.0,-4.0,-6.0)
  - - Acutal Output: (-2.0,-4.0,-6.0)
  - -----------------------------------------------------------------
  -}

{- -----------------------------------------------------------------
   - Test Cases - vecMagnitude
   - -----------------------------------------------------------------
   -
   - -----------------------------------------------------------------
   - - Function: vecMagnitude
   - - Test Case Number: 3-A
   - - Input: (1.0,2.0,3.0)
   - - Expected Output: 3.74165739
   - - Acutal Output: 3.7416573897736413
   - -----------------------------------------------------------------
   -
   - -----------------------------------------------------------------
   - - Function: vecMagnitude
   - - Test Case Number: 3-B
   - - Input: (-1.0,-2.0,0.0)
   - - Expected Output: 2.23606798
   - - Acutal Output: 2.23606797749979
   - -----------------------------------------------------------------
   -
   - -----------------------------------------------------------------
   - - Function: vecMagnitude
   - - Test Case Number: 3-C
   - - Input: (-1.0,-2.0,-3.0)
   - - Expected Output: 3.74165739
   - - Acutal Output: 3.7416573897736413
   - -----------------------------------------------------------------
   -}

{- -----------------------------------------------------------------
    - Test Cases - vecInnerProd
    - -----------------------------------------------------------------
    -
    - -----------------------------------------------------------------
    - - Function: vecInnerProd
    - - Test Case Number: 4-A
    - - Input: (1.0,2.0,3.0) (1.0,2.0,3.0)
    - - Expected Output: 14
    - - Acutal Output: 14.0
    - -----------------------------------------------------------------
    -
    - -----------------------------------------------------------------
    - - Function: vecInnerProd
    - - Test Case Number: 4-B
    - - Input: (-1.0,-2.0,-3.0) (1.0,2.0,3.0)
    - - Expected Output: -14
    - - Acutal Output: -14
    - -----------------------------------------------------------------
    -
    - -----------------------------------------------------------------
    - - Function: vecInnerProd
    - - Test Case Number: 4-C
    - - Input: (-1.0,-2.0,-3.0) (-1.0,-2.0,-3.0)
    - - Expected Output: 14
    - - Acutal Output: 14
    - -----------------------------------------------------------------
    -}

{- -----------------------------------------------------------------
     - Test Cases - vecF
     - -----------------------------------------------------------------
     -
     - -----------------------------------------------------------------
     - - Function: vecF
     - - Test Case Number: 5-A
     - - Input: vecF (1,2,3) [(1,2,3), (5,6,7), (3,2,1)]
     - - Expected Output: ((1.0,2.0,3.0),(5.0,6.0,7.0))
     - - Acutal Output: ((1.0,2.0,3.0),(5.0,6.0,7.0))s
     - -----------------------------------------------------------------
     -
     - -----------------------------------------------------------------
     - - Function: vecF
     - - Test Case Number: 5-B
     - - Input: (1,2,3) [(-1,-2,-3), (-5,-6,-7), (-3,-2,-1)]
     - - Expected Output: ((-1.0,-2.0,-3.0), (-5.0,-6.0,-7.0))
     - - Acutal Output: ((-1.0,-2.0,-3.0), (-5.0,-6.0,-7.0))
     - -----------------------------------------------------------------
     -
     - -----------------------------------------------------------------
     - - Function: vecF
     - - Test Case Number: 5-C
     - - Input: (-1,-2,-3) [(1,2,3), (-5,-6,-7), (-3,-2,-1)]
     - - Expected Output: ((1.0,2.0,3.0),(-5.0,-6.0,-7.0))
     - - Acutal Output: ((1.0,2.0,3.0),(-5.0,-6.0,-7.0))
     - -----------------------------------------------------------------
     -}