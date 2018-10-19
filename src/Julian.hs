-- |

module Julian where

-- Year, Month, Day, Hour, Minute, Second, Time Zone
type GregorianDate = (Int, Int, Int, Int, Int, Int, Double)

deltaT = 70 :: Double
deltaUT1 = 0.036687 :: Double

julianDay_ :: Int -> Int -> Double -> Int -> Double
julianDay_ y m d b =
  let y_i = fromIntegral(y)
      m_i = fromIntegral(m) in
  fromIntegral(floor(365.25 * (y_i + 4716)) +
                floor(30.6001 * (m_i + 1)) +
                b) +
  d -
  1524.5

julianDay :: GregorianDate -> Double
julianDay (y, m, d, h, min, s, tz) =
  let new_y = if m > 2 then y else y - 1
      new_m = if m > 2 then m else m + 12
      y_i = fromIntegral(new_y)
      m_i = fromIntegral(new_m)
      s_i = ((fromIntegral s) + deltaUT1) / 60
      min_i = ((fromIntegral min) + s_i) / 60
      h_i = (fromIntegral h) - tz + min_i / 60
      d_i = (fromIntegral d) + h_i / 24
      julian_cal_day = julianDay_ new_y new_m d_i 0
      a = floor(y_i / 100)
      b = 2 - a + floor((fromIntegral a) / 4) in
    if julian_cal_day < 2299160
    then julian_cal_day
    else julianDay_ new_y new_m d_i b

julianEphemerisDay :: Double -> Double
julianEphemerisDay jd = jd + (deltaT / 86400)

julianCentury :: Double -> Double
julianCentury jd = (jd - 2451545) / 36525

julianEphemerisCentury :: Double -> Double
julianEphemerisCentury jde = (jde - 2451545) / 36525

julianEphemerisMillenium :: Double -> Double
julianEphemerisMillenium jce = jce / 10
