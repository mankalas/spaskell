module Main where

import Lib
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Calendar.Julian

test = toJulianYearAndDay $ fromGregorian (-4712) 1 1

main :: IO ()
main = putStrLn $ show test
