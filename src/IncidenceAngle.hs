module IncidenceAngle where

import Utilities

angle :: Double -- ^ Topocentric zenith angle
      -> Double -- ^ Slope of the surface
      -> Double -- ^ Topocentric astronomer azimuth angle
      -> Double -- ^ Surface azimuth rotation
      -> Double
angle theta omega gamma g =
  let thetar = degreeToRadian theta
      omegar = degreeToRadian omega
      gammar = degreeToRadian(gamma - g) in
    radianToDegree $ acos (cos thetar * cos omegar + sin omegar * sin thetar * cos gammar)
