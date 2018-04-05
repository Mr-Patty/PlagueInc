module State where


  -- | Miso framework import
import Miso
import Miso.String

import Data.Time.Clock


data State = State
  {
  number :: Int,
  season :: Season,
  time :: Int,
  price :: Float,
  fond :: Double
  , cities :: [City]
  }
  deriving (Eq, Show)

data City = City
  {
  population :: Int,
  seek :: Double,
  priv :: Float,
  trans :: Float
  }
  deriving (Eq, Show)

data Season =
  January | February | March | April | May |
  June | July | August | September | October | November | December
  deriving (Eq, Show)

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

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

initDefaultCity :: City
initDefaultCity = City
  { population = 0
  ,  seek = 0
  ,  priv = 0
  ,  trans = 0
  }

initDefault :: UTCTime -> State
initDefault time = State
  { number = 0
  , season = January
  , time = 0
  , price = 0.0
  , fond = 0.0
  , cities = []
  }
