module ObserverLocalHourAngle where

import Utilities

angle :: Double -- ^ Apparent Sideral Time
      -> Double -- ^ Observer geographical longitude
      -> Double -- ^ Sun Right Ascension
      -> Double
angle nu longitude alpha = limitDegrees $ nu + longitude - alpha
