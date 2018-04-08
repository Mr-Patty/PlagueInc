module State where


  -- | Miso framework import
import Miso
import Miso.String

import Data.Time.Clock
import System.Random

workable :: Float
workable = 0.65

duty :: Float
duty = 1500

instance Eq StdGen where
  g1 == g2 = (show g1) == (show g2)

data State = State
  { number :: Int
  , season :: Season
  , time :: Int
  , timeDiff :: Int
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
      newTime = (time state) - 1
      newTimeDiff = (timeDiff state) + 1
      -- newSeason =
    in
      (recalculation . taxPayments $ state)

instance Town City where
  tax price city =
    workable * healthy * duty - price * (fromIntegral vac)
    where
      healthy = (1 - (sick city)) * (population city)
      vac =
        case (vaccine city) of
          (Just n) -> n
          Nothing -> 0

instance Department State where
  taxPayments state =
    state {fond = (fond state) + (func state)}
    where
      func s =
        Prelude.foldl (\b a -> b + tax (price s) a) 0.0 (cities s)


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
  , timeDiff = 0
  , price = 0.0
  , fond = 0.0
  , isFocus = Nothing
  , cities = []
  , gen = g
  }
