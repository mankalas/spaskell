module Geocentric where

import Utilities

longitude :: Double -- ^ Earth heliocentric longitude
          -> Double
longitude l = limitDegrees $ l + 180

latitude :: Double -- ^ Earth heliocentric latitude
         -> Double
latitude = negate

sunRightAscension :: Double -- ^ Apparent Sun Longitude
                  -> Double -- ^ True Ecliptic Obliquity
                  -> Double -- ^ Geocentric Latitude
                  -> Double
sunRightAscension l e b =
  let lr = degreeToRadian l
      er = degreeToRadian e
      br = degreeToRadian b in
    limitDegrees $ radianToDegree $ atan2 (sin lr * cos er - tan br * sin er) (cos lr)
