module Geocentric where

import Utilities

longitude :: Double -- ^ Earth heliocentric longitude
          -> Double
longitude l = limitDegrees $ l + 180

latitude :: Double -- ^ Earth heliocentric latitude
         -> Double
latitude = negate

dToR3 a b c = (degreeToRadian a, degreeToRadian b, degreeToRadian c)

sunRightAscension :: Double -- ^ Apparent Sun Longitude
                  -> Double -- ^ True Ecliptic Obliquity
                  -> Double -- ^ Geocentric Latitude
                  -> Double
sunRightAscension l e b =
  let (lr, er, br) = dToR3 l e b in
    limitDegrees $ radianToDegree $ atan2 (sin lr * cos er - tan br * sin er) (cos lr)

sunDeclination :: Double -- ^ Apparent Sun Longitude
               -> Double -- ^ True Ecliptic Obliquity
               -> Double -- ^ Geocentric Latitude
               -> Double
sunDeclination l e b =
  let (lr, er, br) = dToR3 l e b in
    radianToDegree $ asin (sin br * cos er + cos br * sin er * sin lr)
