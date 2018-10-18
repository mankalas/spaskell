-- | 

module Heliocentric where

import Julian
import LTerms

l0i :: Int -> GregorianDate -> Double
l0i i d = l0a(i) * cos(l0b(i) + l0c(i) * julianEphemerisMillenium d)
