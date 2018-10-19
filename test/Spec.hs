module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Julian

tests = [
  julianDayTest,
  julianCenturyTest,
  julianEphemerisDayTest,
  julianEphemerisCenturyTest
  ]

jc  = julianCentury . julianDay
jde = julianEphemerisDay . julianDay
jce = julianEphemerisCentury . jde
jme = julianEphemerisMillenium . jce

julianEphemerisMilleniumTest :: TestTree
julianEphemerisMilleniumTest = testCase "Julian Ephemeris Millenium" $ do
  assertEqual "January 1, 2000 12:00:00 UT"    0.000001 $ jme (2000, 1, 1, 12, 0, 0, 0)

julianEphemerisCenturyTest :: TestTree
julianEphemerisCenturyTest = testCase "Julian Ephemeris Century" $ do
  assertEqual "January 1, 2000 12:00:00 UT"    0.000008 $ jce (2000, 1, 1, 12, 0, 0, 0)

julianEphemerisDayTest :: TestTree
julianEphemerisDayTest = testCase "Julian Ephemeris Day" $ do
  assertEqual "January 1, 2000 12:00:00 UT"    2451545.292417 $ jde (2000, 1, 1, 12, 0, 0, 0)

julianCenturyTest :: TestTree
julianCenturyTest = testCase "Julian Century" $ do
  assertEqual "January 1, 2000 12:00:00 UT"    0.000008 $ jc (2000, 1, 1, 12, 0, 0, 0)

julianDayTest :: TestTree
julianDayTest = testCase "Julian Day" $ do
  assertEqual "January 1, 2000 12:00:00 UT"    2451545.291667 $ julianDay (2000, 1, 1, 12, 0, 0, 0)
  assertEqual "January 1, 1999 00:00:00 UT"    2451179.5 $ julianDay (1999, 1, 1, 0, 0, 0, 0)
  assertEqual "January 27, 1987 00:00:00 UT"   2446822.5 $ julianDay (1987, 1, 27, 0, 0, 0, 0)
  assertEqual "June 19, 1987 12:00:00 UT"      2446966.0 $ julianDay (1987, 6, 19, 12, 0, 0, 0)
  assertEqual "January 27, 1988 00:00:00 UT"   2447187.5 $ julianDay (1988, 1, 27, 0, 0, 0, 0)
  assertEqual "June 19, 1988 12:00:00 UT"      2447332.0 $ julianDay (1988, 6, 19, 12, 0, 0, 0)
  assertEqual "January 1, 1900 00:00:00 UT"    2415020.5 $ julianDay (1900, 1, 1, 0, 0, 0, 0)
  assertEqual "January 1, 1600 00:00:00 UT"    2305447.5 $ julianDay (1600, 1, 1, 0, 0, 0, 0)
  assertEqual "December 31, 1600 00:00:00 UT"  2305812.5 $ julianDay (1600, 12, 31, 0, 0, 0, 0)
  assertEqual "April 10, 837 07:12:00 UT"      2026871.8 $ julianDay (837, 4, 10, 7, 12, 0, 0)
  assertEqual "December 31, -123 00:00:00 UT"  1676496.5 $ julianDay (-123, 12, 31, 0, 0, 0, 0)
  assertEqual "January 1, -122 00:00:00 UT"    1676497.5 $ julianDay (-122, 1, 1, 0, 0, 0, 0)
  assertEqual "July 12, -1000 12:00:00 UT"     1356001.0 $ julianDay (-1000, 7, 12, 12, 0, 0, 0)
  assertEqual "February 29, -1000 00:00:00 UT" 1355866.5 $ julianDay (-1000, 2, 29, 12, 0, 0, 0)
  assertEqual "August 17, -1001 21:36:00 UT"   1355671.4 $ julianDay (-1001, 8, 17, 21, 36, 0, 0)
  assertEqual "January 1, -4712 12:00:00 UT"   0.0       $ julianDay (-4712, 1, 1, 12, 0, 0, 0)

main :: IO ()
main = do
  defaultMain (testGroup " Julian functions " tests)
