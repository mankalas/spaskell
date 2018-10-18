-- |

module Heliocentric where

import Julian
import LTerms
import BTerms
import Utilities

-- Lx_i
earthPeriodicTerm :: GregorianDate -> (Double, Double, Double) -> Double
earthPeriodicTerm d terms =
  let (a, b, c) = terms in
    a * cos(b + c * julianEphemerisMillenium d)

-- Lx
earthPeriodicTermSummation :: GregorianDate -> (Int -> (Double, Double, Double)) -> Double
earthPeriodicTermSummation d term = sum $ map (earthPeriodicTerm d) $ map term [0..63]

earthCoordinate :: [Int -> (Double, Double, Double)] -> GregorianDate -> Double
earthCoordinate terms d =
  (sum $ map earthValue [0..5]) / 1e8
  where earthValue i =
          let jme = julianEphemerisMillenium d in
            earthPeriodicTermSummation d (terms !! i) * jme ^^ i

-- L_r
earthLongitude :: GregorianDate -> Double
earthLongitude = limitDegrees . radianToDegree . earthCoordinate [l0, l1, l2, l3, l4, l5]

-- B_r
earthLatitude :: GregorianDate -> Double
earthLatitude = radianToDegree . earthCoordinate [b0, b1, b2, b3, b4, b5]
