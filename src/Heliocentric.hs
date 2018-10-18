-- |

module Heliocentric where

import Julian
import LTerms
import Utilities

-- Lx_i
earthPeriodicTerm :: GregorianDate -> (Double, Double, Double) -> Double
earthPeriodicTerm d terms =
  let (a, b, c) = terms in
    a * cos(b + c * julianEphemerisMillenium d)

-- Lx
earthPeriodicTermSummation :: GregorianDate -> (Int -> (Double, Double, Double)) -> Double
earthPeriodicTermSummation d l_term = sum $ map (earthPeriodicTerm d) $ map l_term [0..63]

-- L_r
earthLongitude :: GregorianDate -> Double
earthLongitude d =
  limitDegrees $ radianToDegree $ (sum $ map earthValue [0..5]) / 1e8
  where earthValue i =
          let l_terms = [l0, l1, l2, l3, l4, l5]
              jme = julianEphemerisMillenium d in
            earthPeriodicTermSummation d (l_terms !! i) * jme ^^ i
