module ApparentSideralTime where

import Utilities

meanSideralTime :: Double -- ^ Julian Day
                -> Double -- ^ Julian Century
                -> Double
meanSideralTime jd jc = 280.46061837 + 360.98564736629 * (jd - 2451545) + jc * jc * (0.000387933 - jc / 38710000)

apparentSideralTime :: Double -- ^ Mean Sideral Time
                    -> Double -- ^ Nutation Longitude
                    -> Double -- ^ True Ecliptic Obliquity
                    -> Double
apparentSideralTime nu_0 delta_phi eps = limitDegrees nu_0 + delta_phi * cos (degreeToRadian eps)
