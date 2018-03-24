module Lib
    ( someFunc
    ) where

import Data.Time.Clock

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data City = City
  {
  a :: Int,
  b :: Double,
  c :: Float,
  d :: Float
  }

data World = World
  {
  e :: Int,
  f :: Season,
  time :: UTCTime,
  price :: Float,
  fond :: Double,
  cities :: [City]
  }

data Season =
  January | February | March | April | May |
  June | July | August | September | October | November | December
