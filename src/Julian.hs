-- | 

module Julian where

julianDay :: Int -> Int -> Double -> Double
julianDay y m d =
  let (y1, m1) = if m > 2
               then (fromIntegral y, fromIntegral m)
               else (fromIntegral(y - 1), fromIntegral(m + 12))
      a = floor(y1 / 100.0)
      b = 2 - a + floor((fromIntegral a) / 4) in
    fromIntegral(floor(365.25 * (y1 + 4716))) +
    fromIntegral(floor(30.6001 * (m1 + 1))) +
    d +
    (fromIntegral b) -
    1524.5
