module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Julian
import Heliocentric

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
  jan012000
  ]

jc  = julianCentury . julianDay
jde = julianEphemerisDay . julianDay
jce = julianEphemerisCentury . jde
jme = julianEphemerisMillenium . jce

ehlong = earthLongitude . jme
ehlat = earthLatitude . jme
ehrv = earthRadiusVector . jme

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

main :: IO ()
main = do
  defaultMain (testGroup " SPA " tests)
