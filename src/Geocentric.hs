module Geocentric where

import Utilities

longitude :: Double -- ^ Earth heliocentric longitude
          -> Double
longitude l = limitDegrees $ l + 180

latitude :: Double -- ^ Earth heliocentric latitude
         -> Double
latitude = negate
