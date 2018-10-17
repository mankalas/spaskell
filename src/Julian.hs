-- |

module Julian where

type GregorianToJulian = Int -> Int -> Double -> Double

deltaT = 70

julianDay :: GregorianToJulian
julianDay y m d =
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

julianEphemerisDay :: GregorianToJulian
julianEphemerisDay y m d = julianDay y m d + (deltaT / 86400)

julianCentury :: GregorianToJulian
julianCentury y m d = (julianDay y m d - 2451545) / 36525

julianEphemerisCentury :: GregorianToJulian
julianEphemerisCentury y m d = (julianEphemerisDay y m d - 2451545) / 36525
