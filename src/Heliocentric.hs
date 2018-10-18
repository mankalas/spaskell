-- |

module Heliocentric where

import Julian
import LTerms
import BTerms
import RTerms
import Utilities

-- Lx_i
earthPeriodicTerm :: GregorianDate -> (Double, Double, Double) -> Double
earthPeriodicTerm d terms =
  let (a, b, c) = terms in
    a * cos(b + c * julianEphemerisMillenium d)

-- Lx
earthPeriodicTermSummation :: GregorianDate -> (Int -> (Double, Double, Double)) -> Double
earthPeriodicTermSummation d term = sum $ map (earthPeriodicTerm d) $ map term [0..63]

earthValues :: [Int -> (Double, Double, Double)] -> GregorianDate -> Double
earthValues terms d =
  let jme = julianEphemerisMillenium d in
    (sum $ map (earthValue jme) [0..5]) / 1e8
  where earthValue jme i = earthPeriodicTermSummation d (terms !! i) * jme ^^ i

-- L
earthLongitude :: GregorianDate -> Double
earthLongitude = limitDegrees . radianToDegree . earthValues [l0, l1, l2, l3, l4, l5]

-- B
earthLatitude :: GregorianDate -> Double
earthLatitude = radianToDegree . earthValues [b0, b1, b2, b3, b4, b5]

-- R
earthRadiusVector :: GregorianDate -> Double
earthRadiusVector = earthValues [r0, r1, r2, r3, r4, r5]
