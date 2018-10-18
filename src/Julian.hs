-- |

module Julian where

type GregorianDate = (Int, Int, Double)

deltaT = 70 :: Double

julianDay :: GregorianDate -> Double
julianDay (y, m, d) =
  let yi = fromIntegral(if m > 2 then y else y - 1)
      mi = fromIntegral(if m > 2 then m else m + 12)
      julian_cal_day = julianDay_ yi mi 0
      a = floor(yi / 100)
      b = 2 - a + floor((fromIntegral a) / 4) in
    if julian_cal_day < 2299160
    then julian_cal_day
    else julianDay_ yi mi b
  where julianDay_ y m b =
          fromIntegral(floor(365.25 * (y + 4716)) +
                       floor(30.6001 * (m + 1)) +
                       b) +
          d - 1524.5

julianEphemerisDay :: GregorianDate -> Double
julianEphemerisDay d = julianDay d + deltaT / 86400

julianCentury :: GregorianDate -> Double
julianCentury d = (julianDay d - 2451545) / 36525

julianEphemerisCentury :: GregorianDate -> Double
julianEphemerisCentury d = (julianEphemerisDay d - 2451545) / 36525

julianEphemerisMillenium :: GregorianDate -> Double
julianEphemerisMillenium d = julianEphemerisCentury d / 10
