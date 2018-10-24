module AberrationCorrection where

aberrationCorrection :: Double -- ^ Earth radius vector
                     -> Double
aberrationCorrection r = (-20.4898) / (3600 * r)
