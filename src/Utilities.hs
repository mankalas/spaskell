-- | 

module Utilities where

radianToDegree :: Double -> Double
radianToDegree = (*) (180 / pi)

degreeToRadian :: Double -> Double
degreeToRadian = (*) (pi / 180)

limitDegrees :: Double -> Double
limitDegrees d =
  let degrees = d / 360
      limited = 360 * (degrees - fromIntegral(floor degrees)) in
    if limited < 0
    then limited + 360
    else limited

thirdOrderPolynomial :: Double -> Double -> Double -> Double -> Double -> Double
thirdOrderPolynomial a b c d jce = ((a * jce + b) * jce + c) * jce + d
