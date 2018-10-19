-- | 

module Geocentric where

import Utilities

longitude :: Double -> Double
longitude l = limitDegrees $ l + 180

latitude :: Double -> Double
latitude = negate
