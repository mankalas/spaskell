-- | 

module AberrationCorrection where

import Heliocentric

aberrationCorrection :: Double -- Earth radius vector
                     -> Double -- Aberration correction
aberrationCorrection r = (-20.4898) / (3600 * r)
