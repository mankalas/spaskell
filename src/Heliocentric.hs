-- |

module Heliocentric where

import Julian
import LTerms
import BTerms
import RTerms
import Utilities

type EarthTerm = (Int -> (Double, Double, Double), Int)

lTerms_ = [(l0, 64), (l1, 34), (l2, 20), (l3, 7), (l4, 3), (l5, 1)]
bTerms_ = [(b0, 5), (b1, 2)]
rTerms_ = [(r0, 40), (r1, 10), (r2, 6), (r3, 2), (r4, 1)]

earthPeriodicTerm_ :: (Double, Double, Double) -> Double -> Double
earthPeriodicTerm_ (a, b, c) jme = a * cos(b + c * jme)

earthPeriodicTermSummation_ :: EarthTerm -> Double -> Double
earthPeriodicTermSummation_ (term, max) jme = sum $ map (flip earthPeriodicTerm_ jme) $ map term [0..max - 1]

earthValues_ :: [EarthTerm] -> Double -> Double
earthValues_ terms jme =
  (sum $ map earthValue_ [0..5]) / 1e8
  where earthValue_ i =
          let term = if i < length terms then terms !! i else (const (0, 0, 0), 1) in
            earthPeriodicTermSummation_ term jme * jme ^^ i

-- export

-- L
earthLongitude :: Double -> Double
earthLongitude = limitDegrees . radianToDegree . earthValues_ lTerms_

-- B
earthLatitude :: Double -> Double
earthLatitude = radianToDegree . earthValues_ bTerms_

-- R
earthRadiusVector :: Double -> Double
earthRadiusVector = earthValues_ rTerms_
