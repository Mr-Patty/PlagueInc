module State where


  -- | Miso framework import
import Miso
import Miso.String

import Data.Time.Clock
import System.Random

workable :: Float
workable = 0.65

duty :: Float
duty = 500

instance Eq StdGen where
  g1 == g2 = (show g1) == (show g2)

data State = State
  { number :: Int
  , season :: Season
  , time :: Int
  , startTime :: Int
  , timeDiffWeek :: Int
  , timeDiffMonth :: Int
  , timeDiffYear :: Int
  , startWeek :: Bool
  , price :: Float
  , fond :: Float
  , isFocus :: Maybe Int
  , cities :: [City]
  , gen :: StdGen
  }
  deriving (Eq, Show)

data City = City
  { population :: Float
  , sick :: Float
  , immune :: Float
  , vaccine :: Maybe Int
  , vaccinated :: [(Int, Int)] -- число вакцинированых, показывает сколько недель назад произошла вакцинация
  , trafficIn :: Float
  , trafficOut :: Float
  }
  deriving (Eq, Show)

data Season =
  January | February | March | April | May |
  June | July | August | September | October | November | December
  deriving (Eq, Show)

class World state where
  -- check :: state -> Bool
  recalculation :: state -> state
  nextDay :: state -> state

class Town town where
  -- check :: town -> Bool
  -- recalculation :: town -> town
  tax :: Float -> town -> Float

class Department state where
  taxPayments :: state -> state

instance World State where
  recalculation state = state

  nextDay state =
    let
      newCities = (cities state)
      -- newTime = (time state) - 1
      newTime = ((startTime state) + 1)
      newTimeDiffWeek = (timeDiffWeek state) + 1
      newStartWeek = if newTimeDiffWeek `div` 7 /= 0 then False else (startWeek state)
      -- newSeason =
    in
      (recalculation . taxPayments $ state)
      { cities = newCities
      , startTime = newTime
      , timeDiffWeek = newTimeDiffWeek `mod` 7
      , startWeek = newStartWeek
      }

instance Town City where
  tax price city =
    workable * healthy * duty - price * (fromIntegral vac)
    where
      healthy = (1 - (sick city)) * (population city)
      vac =
        case (vaccine city) of
          (Just n) -> n
          Nothing -> 0

  -- recalculation town =
  --   let
  --

instance Department State where
  taxPayments state =
    state {fond = (fond state) + (func state)}
    where
      func s =
        Prelude.foldl (\b a -> b + tax (price s) a) 0.0 (cities s)

nexMonth :: Season -> Season
nexMonth season =
  case season of
    January -> February
    February -> March
    March -> April
    April -> May
    May -> June
    June -> July
    July -> August
    August -> September
    September -> October
    October -> November
    November -> December
    December -> January

seasonToInt :: Season -> Int
seasonToInt s =
  case s of
    January -> 0
    February -> 30
    March -> 60
    April -> 90
    May -> 120
    June -> 150
    July -> 180
    August -> 210
    September -> 240
    October -> 270
    November -> 300
    December -> 330

intToSeason :: Int -> Season
intToSeason n =
  case n `div` 30 of
     0 -> January
     1 -> February
     2 -> March
     3 -> April
     4 -> May
     5 -> June
     6 -> July
     7 -> August
     8 -> September
     9 -> October
     10 -> November
     11 -> December


readSeason :: Int -> Season
readSeason n =
  case n of
    1 -> January
    2 -> February
    3 -> March
    4 -> April
    5 -> May
    6 -> June
    7 -> July
    8 -> August
    9 -> September
    10 -> October
    11 -> November
    12 -> December
    _ -> January

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx

initDefaultCity :: City
initDefaultCity = City
  { population = 0.1
  , sick = 0
  , immune = 0
  , vaccine = Nothing
  , vaccinated = []
  , trafficIn = 0.3
  , trafficOut = 0.3
  }

initDefault :: StdGen -> State
initDefault g = State
  { number = 0
  , season = January
  , time = 0
  , startTime = 0
  , timeDiffWeek = 0
  , timeDiffMonth = 0
  , timeDiffYear = 0
  , startWeek = False
  , price = 0.0
  , fond = 0.0
  , isFocus = Nothing
  , cities = []
  , gen = g
  }
