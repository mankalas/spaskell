-- | 

module TrueObliquity where

meanEclipticObliquity :: Double -- Julian Ephemeris Millenimum
                       -> Double -- Mean Ecliptic Obliquity
meanEclipticObliquity jme =
  let u = jme / 10 in
    84381.448 - u * (4680.93 - u * (1.55 + u * (1999.25 - u * (51.38 - u * (249.67 - u *(39.05 + u * (7.12 + u * (27.87 + u * (5.79 + u * 2.45)))))))))

trueEclipticObliquity :: Double -- Mean Ecliptic Obliquity
                      -> Double -- Nutation Obliquity
                      -> Double -- True Ecliptic Olbiquity
trueEclipticObliquity eps_0 delta_eps = eps_0 / 3600 + delta_eps
