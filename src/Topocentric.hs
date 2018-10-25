module Topocentric where

import Utilities

sunEquatorialHorizontalParallax :: Double -- ^ Earth Radius Vector
                                -> Double
sunEquatorialHorizontalParallax r = 8.794 / 3600 * r

u :: Double -- ^ Observer latitude
  -> Double
u l =
  let lr = degreeToRadian l in
    atan (0.99664719 * tan lr)

x :: Double -- ^ u
  -> Double -- ^ Observer elevation
  -> Double -- ^ Observer geographic latitude
  -> Double
x u e lat =
  let lr = degreeToRadian lat in
    cos u + e / 6378140 * cos lr

y :: Double -- ^ u
  -> Double -- ^ Observer elevation
  -> Double -- ^ Observer geographic latitude
  -> Double
y u e l =
  let lr = degreeToRadian l in
    0.99664719 * sin u  + e / 6378140 * sin lr

sunRightAscensionParallax :: Double -- ^ x
                          -> Double -- ^ Sun Equatorial Horizontal Parallax
                          -> Double -- ^ Observer Local Hour Angle
                          -> Double -- ^ Geocentric Sun declination
                          -> Double
sunRightAscensionParallax x xi h delta =
  let xir = degreeToRadian xi
      hr = degreeToRadian h
      deltar = degreeToRadian delta in
    radianToDegree $ atan2 (-x * sin xir * sin hr) (cos deltar - x * sin xir * cos hr)

sunRightAscension :: Double -- ^ Geocentric Sun Right Ascension
                  -> Double -- ^ Sun Right Ascension Parallax
                  -> Double
sunRightAscension alpha delta_alpha = alpha + delta_alpha

sunDeclination :: Double -- ^ Geocentric Sun declination
               -> Double -- ^ y
               -> Double -- ^ Sun Equatorial Horizontal Parallax
               -> Double -- ^ Sun Right Ascension Parallax
               -> Double -- ^ x
               -> Double -- ^ Observer Local Hour Angle
               -> Double
sunDeclination delta y xi delta_alpha x h =
    let xir = degreeToRadian xi
        hr = degreeToRadian h
        deltar = degreeToRadian delta
        delta_alphar = degreeToRadian delta_alpha in
  radianToDegree $ atan2 ((sin deltar - y * sin xir) * cos delta_alphar) (cos deltar - x * sin xir * cos hr)

localHourAngle :: Double -- ^ Observer Local Hour Angle
               -> Double -- ^ Sun Right Ascension Parallax
               -> Double
localHourAngle h delta_alpha = h - delta_alpha

rawElevationAngle :: Double -- ^ Observer latitude
                  -> Double -- ^ Topocentric Sun declination
                  -> Double -- ^ Topocentric Local Hour Angle
                  -> Double
rawElevationAngle l delta h =
  let lr = degreeToRadian l
      deltar = degreeToRadian delta
      hr = degreeToRadian h in
  radianToDegree $ asin (sin lr * sin deltar + cos lr * cos deltar * cos hr)

atmosphericRefractionCorrection :: Double -- ^ Annual average local pressure
                                -> Double -- ^ Annual average local temperature
                                -> Double -- ^ Elevation angle without atmospheric refraction correction
                                -> Double
atmosphericRefractionCorrection p t e_0 =
  p / 1010 * 283 / (273 + t) * 1.02 / (60 * tan (degreeToRadian(e_0 + 10.3 / (e_0 + 5.11))))

elevationAngle :: Double -- ^ Elevation angle without atmospheric refraction correction
               -> Double -- ^ Atmospheric refraction correction
               -> Double
elevationAngle e_0 delta_e = e_0 + delta_e

zenithAngle :: Double -- ^ Elevation Angle
            -> Double
zenithAngle e = 90 - e
