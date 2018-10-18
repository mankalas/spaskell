-- | 

module Heliocentric where

import Julian
import LTerms

-- Lx_i
earth_periodic_term :: GregorianDate -> (Double, Double, Double) -> Double
earth_periodic_term d terms =
  let (a, b, c) = terms in
    a * cos(b + c * julianEphemerisMillenium d)

-- Lx
earth_periodic_term_summation :: GregorianDate -> (Int -> (Double, Double, Double)) -> Double
earth_periodic_term_summation d l_term = sum $ map (earth_periodic_term d) $ map l_term [0..63]

-- L_r
earth_longitude :: GregorianDate -> Double
earth_longitude d =
  (sum $ map earth_value [0..5]) / 1e8
  where earth_value i =
          let l_terms = [l0, l1, l2, l3, l4, l5]
              jme = julianEphemerisMillenium d in
            earth_periodic_term_summation d (l_terms !! i) * jme ^^ i
