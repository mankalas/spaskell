module ApparentSunLongitude where

apparentSunLongitude :: Double -- ^ Geocentric longitude
                     -> Double -- ^ Nutation longitude
                     -> Double -- ^ Aberration correction
                     -> Double
apparentSunLongitude theta delta_phi delta_tau = theta + delta_phi + delta_tau
