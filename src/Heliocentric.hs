-- |

module Heliocentric where

import Julian
import LTerms
import BTerms
import RTerms
import Utilities

type EarthTerm = Int -> (Double, Double, Double)

-- Lx_i
earthPeriodicTerm :: Double -> (Double, Double, Double) -> Double
earthPeriodicTerm jme (a, b, c) = a * cos(b + c * jme)

-- Lx
earthPeriodicTermSummation :: Double -> EarthTerm -> Double
earthPeriodicTermSummation jme term = sum $ map (earthPeriodicTerm jme) $ map term [0..63]

earthValues :: [EarthTerm] -> Double -> Double
earthValues terms jme =
  (sum $ map earthValue [0..5]) / 1e8
  where earthValue i =
          let term = if i < length terms then terms !! i else const (0, 0, 0) in
            earthPeriodicTermSummation jme term * jme ^^ i

-- L
earthLongitude :: Double -> Double
earthLongitude = limitDegrees . radianToDegree . earthValues [l0, l1, l2, l3, l4, l5]

-- B
earthLatitude :: Double -> Double
earthLatitude = radianToDegree . earthValues [b0, b1]

-- R
earthRadiusVector :: Double -> Double
earthRadiusVector = earthValues [r0, r1, r2, r3, r4]
