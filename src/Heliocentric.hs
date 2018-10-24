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

earthPeriodicTerm_ :: (Double, Double, Double) -- ^ A_i, B_i and C_i terms
                   -> Double -- ^ Julian Ephemeris Millenium
                   -> Double -- ^ L0_i
earthPeriodicTerm_ (a, b, c) jme = a * cos(b + c * jme)

earthPeriodicTermSummation_ :: EarthTerm -- ^ One of Lx, Bx or Rx term
                               -> Double -- ^ Julian Ephemeris Millenium
                               -> Double -- ^
earthPeriodicTermSummation_ (term, max) jme = sum $ map ((`earthPeriodicTerm_` jme) . term) [0..max - 1]

earthValues_ :: [EarthTerm] -- ^ Array of Lx, Bx or Rx terms
             -> Double -- ^ Julian Ephemeris Millenium
             -> Double -- ^ Either L, B or R, according to the given terms
earthValues_ terms jme =
  sum (map earthValue_ [0..5]) / 1e8
  where earthValue_ i =
          let term = if i < length terms then terms !! i else (const (0, 0, 0), 1) in
            earthPeriodicTermSummation_ term jme * jme ^^ i

-- export

earthLongitude :: Double -- ^ Julian Ephemeris Millenium
               -> Double -- ^ L
earthLongitude = limitDegrees . radianToDegree . earthValues_ lTerms_

earthLatitude :: Double -- ^ Julian Ephemeris Millenium
              -> Double -- ^ B
earthLatitude = radianToDegree . earthValues_ bTerms_

earthRadiusVector :: Double -- ^ Julian Ephemeris Millenium
                  -> Double -- ^ R
earthRadiusVector = earthValues_ rTerms_
