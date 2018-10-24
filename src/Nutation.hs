module Nutation where

import Utilities
import YTerms
import NutationTerms

meanElongationMoonSun_ :: Double -- ^ Julian Ephemeris Millenium
                       -> Double
meanElongationMoonSun_ = thirdOrderPolynomial (1 / 189474) (-1.9142e-3) 445267.11148 297.85036

meanAnomalySun_ :: Double -- ^ Julian Ephemeris Millenium
                -> Double
meanAnomalySun_ = thirdOrderPolynomial (-1 / 300000) (-1.603e-4) 35999.050340 357.52772

meanAnomalyMoon_ :: Double -- ^ Julian Ephemeris Millenium
                 -> Double
meanAnomalyMoon_ = thirdOrderPolynomial (1 / 56250) 8.6972e-3 477198.867398 134.96298

argumentLatitudeMoon_ :: Double -- ^ Julian Ephemeris Millenium
                      -> Double
argumentLatitudeMoon_ = thirdOrderPolynomial (1 / 327270) (-3.6825e-3) 483202.017538 93.27191

ascendingLongitudeMoon_ :: Double -- ^ Julian Ephemeris Millenium
                        -> Double
ascendingLongitudeMoon_ = thirdOrderPolynomial (1 / 450000) 2.0708e-3 (-1934.136261) 125.04452

x 0 = meanElongationMoonSun_
x 1 = meanAnomalySun_
x 2 = meanAnomalyMoon_
x 3 = argumentLatitudeMoon_
x 4 = ascendingLongitudeMoon_

xyProduct_ :: Double -> Int -> Int -> Double
xyProduct_ jce i j = x j jce * fromIntegral(y i j)

xyProductSummation_ :: Double -> Int -> Double
xyProductSummation_ jce i = sum $ map (xyProduct_ jce i) [0..4]

xyTermSummation_ :: (Double, Double) -- ^ Pair of either (a, b) or (c, d)
                 -> (Double -> Double) -- ^ Either sin or cos
                 -> Double -- ^ JCE
                 -> Int -- ^ Index
                 -> Double
xyTermSummation_ (m, n) f jce i =
  let sum_xy =  xyProductSummation_ jce i in
    (m + n * jce) * f sum_xy

nutation_ :: (Int -> (Double, Double)) -- ^ Coefficient function
          -> (Double -> Double) -- ^ Either sin or cos
          -> Double -- ^ Julien Ephemeris Century
          -> Double -- ^ Either Delta Phi or Delta Epsilon
nutation_ coef trig jce =
  sum (map (f coef jce) [0..62]) / 36000000
  where f coef jce i = xyTermSummation_ (coef i) trig jce i

longitude :: Double -- ^ Julian Ephemeris Century
          -> Double
longitude = nutation_ longCoef sin

obliquity :: Double -- ^ Julian Ephemeris Century
          -> Double
obliquity = nutation_ obliCoef cos
