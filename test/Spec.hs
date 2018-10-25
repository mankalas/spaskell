module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Julian
import Heliocentric
import Geocentric
import Nutation
import TrueObliquity
import AberrationCorrection
import ApparentSunLongitude
import ApparentSideralTime

observerLatitude = 39.743 -- degrees
observerLongitude = -105.178 -- degrees
observerTimeZone = -7.0 -- hours
observerElevation = 1829 -- meters
annualAverageLocalPressure = 835 -- millibars
annualAverageLocalTemperature = 10 -- Celsius
deltaUT1 = 0.0 -- seconds
deltaT = 64.797 -- seconds
surfaceAzimuthRotation = 180 -- degrees
surfaceSlope = 0 -- degrees
atmosphericRefractionAtSunriseAndSunset = 0.5667 -- degrees

tests = [
--  jan012000
  oct172003123030tsm7
  ]

jd = julianDay
jc  = julianCentury . jd
jde = julianEphemerisDay . jd
jce = julianEphemerisCentury . jde
jme = julianEphemerisMillenium . jce

ehlong = earthLongitude . jme
ehlat = earthLatitude . jme
ehrv = earthRadiusVector . jme

glong = Geocentric.longitude . ehlong
glat = Geocentric.latitude . ehlat

x0 = meanElongationMoonSun_ . jce
x1 = meanAnomalySun_ . jce
x2 = meanAnomalyMoon_ . jce
x3 = argumentLatitudeMoon_ . jce
x4 = ascendingLongitudeMoon_ . jce
nlong = Nutation.longitude . jce
nobli = obliquity . jce

teobli d = trueEclipticObliquity (meanEclipticObliquity $ jme d) (nobli d)
ac = aberrationCorrection . ehrv
asl d = apparentSunLongitude (glong d) (nlong d) (ac d)
ast d = apparentSideralTime (meanSideralTime (jd d) (jc d)) (nlong d) (teobli d)
sra d = sunRightAscension (asl d) (teobli d) (glat d)
sd d = sunDeclination (asl d) (teobli d) (glat d)

-- julianDayTest :: TestTree
-- julianDayTest = testCase "Julian Day" $ do
--   assertEqual "January 1, 2000 12:00:00 UT"    2451545.291667 $ julianDay (2000, 1, 1, 12, 0, 0, 0)
--   assertEqual "January 1, 1999 00:00:00 UT"    2451179.5 $ julianDay (1999, 1, 1, 0, 0, 0, 0)
--   assertEqual "January 27, 1987 00:00:00 UT"   2446822.5 $ julianDay (1987, 1, 27, 0, 0, 0, 0)
--   assertEqual "June 19, 1987 12:00:00 UT"      2446966.0 $ julianDay (1987, 6, 19, 12, 0, 0, 0)
--   assertEqual "January 27, 1988 00:00:00 UT"   2447187.5 $ julianDay (1988, 1, 27, 0, 0, 0, 0)
--   assertEqual "June 19, 1988 12:00:00 UT"      2447332.0 $ julianDay (1988, 6, 19, 12, 0, 0, 0)
--   assertEqual "January 1, 1900 00:00:00 UT"    2415020.5 $ julianDay (1900, 1, 1, 0, 0, 0, 0)
--   assertEqual "January 1, 1600 00:00:00 UT"    2305447.5 $ julianDay (1600, 1, 1, 0, 0, 0, 0)
--   assertEqual "December 31, 1600 00:00:00 UT"  2305812.5 $ julianDay (1600, 12, 31, 0, 0, 0, 0)
--   assertEqual "April 10, 837 07:12:00 UT"      2026871.8 $ julianDay (837, 4, 10, 7, 12, 0, 0)
--   assertEqual "December 31, -123 00:00:00 UT"  1676496.5 $ julianDay (-123, 12, 31, 0, 0, 0, 0)
--   assertEqual "January 1, -122 00:00:00 UT"    1676497.5 $ julianDay (-122, 1, 1, 0, 0, 0, 0)
--   assertEqual "July 12, -1000 12:00:00 UT"     1356001.0 $ julianDay (-1000, 7, 12, 12, 0, 0, 0)
--   assertEqual "February 29, -1000 00:00:00 UT" 1355866.5 $ julianDay (-1000, 2, 29, 12, 0, 0, 0)
--   assertEqual "August 17, -1001 21:36:00 UT"   1355671.4 $ julianDay (-1001, 8, 17, 21, 36, 0, 0)
--   assertEqual "January 1, -4712 12:00:00 UT"   0.0       $ julianDay (-4712, 1, 1, 12, 0, 0, 0)

jan012000 :: TestTree
jan012000 =
  let d = (2000, 1, 1, 12, 0, 0, 0) in
    testCase "January 1, 2000 12:00:00 UT" $ do
    assertEqual "Julian day" 2451545 $ julianDay d
    assertEqual "Julian century" 0 $ jc d
    assertEqual "Julian ephemeris day" 2451545.000749965 $ jde d
    assertEqual "Julian ephemeris century" 2.0532928085914627e-8 $ jce d -- Differ from spa.c from here
    assertEqual "Julian ephemeris millennium" 2.0532928085914627e-9 $ jme d
    assertEqual "Earth heliocentric longitude" 100.37854123103729 $ ehlong d
    assertEqual "Earth heliocentric latitude" (-1.8934747170755355e-4) $ ehlat d
    assertEqual "Earth radius vector" 0.9833275767119815 $ ehrv d
    assertEqual "Geocentric longitude" 280.3785412310373 $ glong d
    assertEqual "Geocentric latitude" 1.8934747170755355e-4 $ glat d

    assertEqual "Mean elongation (moon-sun)" 297.85950263757906 $ x0 d
    assertEqual "Mean anomaly (sun)" 357.5284591659118 $ x1 d
    assertEqual "Mean anomaly (moon)" 134.97277829002695 $ x2 d
    assertEqual "Argument latitude (moon)" 93.28183155227708 $ x3 d
    assertEqual "Ascending longitude (moon)" 125.04448028651925 $ x4 d

    assertEqual "Nutation Longitude" (-0.003867) $ nlong d
    assertEqual "Nutation Obliquity" (-0.001606) $ nobli d

oct172003123030tsm7 :: TestTree
oct172003123030tsm7 =
  let d = (2003, 10, 17, 12, 30, 30, -7) in
    testCase "October 17, 2003 12:30:30 UT TZ -7" $ do
    assertEqual "Julian day" 2452930.292361111 $ julianDay d -- SPA: 2452930.312847
    assertEqual "Julian century" 0.037927237812756046 $ jc d
    assertEqual "Julian ephemeris day" 2452930.293111076 $ jde d
    assertEqual "Julian ephemeris century" 0.03792725834568413 $ jce d -- Differ from spa.c from here
    assertEqual "Julian ephemeris millennium" 0.0037927258345684133 $ jme d

    assertEqual "Earth heliocentric longitude" 23.99790778438188 $ ehlong d -- SPA: 24.0182616917
    assertEqual "Earth heliocentric latitude" (-1.0073878657373277e-4) $ ehlat d -- SPA: (-1.011219e-4)
    assertEqual "Earth radius vector" 0.9965479540978688 $ ehrv d -- SPA: 0.99654229745
    assertEqual "Geocentric longitude" 203.99790778438188 $ glong d -- SPA: 204.0182616917
    assertEqual "Geocentric latitude" 1.0073878657373277e-4 $ glat d -- SPA: 0.0001011219

    assertEqual "Mean elongation (moon-sun)" 17185.611127185253 $ x0 d -- Manual: 17185.6111271853
    assertEqual "Mean anomaly (sun)" 1722.8730022136986 $ x1 d -- Manual: 1722.8730022137
    assertEqual "Mean anomaly (moon)" 18233.807718583503 $ x2 d -- Manual: 18233.8077185835
    assertEqual "Argument latitude (moon)" 18419.799657022497 $ x3 d -- Manual: 18419.7996570225
    assertEqual "Ascending longitude (moon)" 51.68803733221671 $ x4 d -- Manual: 51.6880373322167

    assertEqual "Nutation Longitude" (-3.998766179133264e-3) $ nlong d -- SPA: -0.00399840
    assertEqual "Nutation Obliquity" 1.6665147765963365e-3 $ nobli d -- SPA: 0.00166657

    assertEqual "True Ecliptic Obliquity" 23.440464473642695 $ teobli d -- SPA: 23.440465

    assertEqual "Aberration Correction" (-5.711326873640995e-3) $ ac d -- Manual: -0.00567193121257403
    assertEqual "Apparent Sun Longitude" 203.9881976913291 $ asl d -- SPA: 204.0085519281, Manual: 203.988237453169

    assertEqual "Apparent sideral time" 311.1208707057544 $ ast d -- Manual: 311.120870661161
    assertEqual "Sun Right Ascension" 202.20823126506485 $ sra d -- SPA: 202.22741
    assertEqual "Sun Declination" (-9.306844762179702) $ sd d -- SPA: -9.31434

main :: IO ()
main = defaultMain (testGroup " SPA " tests)
