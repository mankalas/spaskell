-- |

module Nutation where

import Utilities
import YTerms
import NutationTerms

meanElongationMoonSun_ :: Double -> Double
meanElongationMoonSun_ = thirdOrderPolynomial (1 / 189474) (-1.9142e-3) 445267.11148 297.85036

meanAnomalySun_ :: Double -> Double
meanAnomalySun_ = thirdOrderPolynomial (-3e-6) (-1.603e-4) 35999.050340 357.52772

meanAnomalyMoon_ :: Double -> Double
meanAnomalyMoon_ = thirdOrderPolynomial (1 / 56250) 8.6972e-3 477198.867398 134.96298

argumentLatitudeMoon_ :: Double -> Double
argumentLatitudeMoon_ = thirdOrderPolynomial (1 / 327270) (-3.6825e-3) 483202.017538 93.27191

ascendingLongitudeMoon_ :: Double -> Double
ascendingLongitudeMoon_ = thirdOrderPolynomial (4.5e-5) 2.0708e-3 (-1934.136261) 125.04452

x 0 = meanElongationMoonSun_
x 1 = meanAnomalySun_
x 2 = meanAnomalyMoon_
x 3 = argumentLatitudeMoon_
x 4 = ascendingLongitudeMoon_

xyProduct_ :: Double -> Int -> Int -> Double
xyProduct_ jce i j = x j jce * fromIntegral(y i j)

xyProductSummation_ :: Double -> Int -> Double
xyProductSummation_ jce i = sum $ map (xyProduct_ jce i) [0..4]

xyTermSummation_ :: (Double, Double) -> (Double -> Double) -> Double -> Int -> Double
xyTermSummation_ (m, n) f jce i =
  let sum_xy =  xyProductSummation_ jce i in
    (m + n * jce) * f sum_xy

longitude_ :: Double -> Int -> Double
longitude_ jce i =
  let (a, b) = longCoef i in
    xyTermSummation_ (a, b) sin jce i

obliquity_ :: Double -> Int -> Double
obliquity_ jce i =
  let (c, d) = obliCoef i in
    xyTermSummation_ (c, d) cos jce i

nutation_ :: (Double -> Int -> Double) -> Double -> Double
nutation_ f jce = (sum $ map (f jce) [0..62]) / 36000000

longitude :: Double -> Double
longitude jce = nutation_ longitude_ jce

obliquity :: Double -> Double
obliquity jce = nutation_ obliquity_ jce
