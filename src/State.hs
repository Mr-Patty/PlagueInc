module State where


  -- | Miso framework import
import Miso
import Miso.String

import Data.Time.Clock
import System.Random
import Data.List

workable :: Double
workable = 0.65

duty :: Double
duty = 0.05

instance Eq StdGen where
  g1 == g2 = (show g1) == (show g2)

-- data Maybe a = Nothing | Just a

-- * состояние эксперимента
data State = State
  { number :: Int -- ^ количество городов
  , season :: Season -- ^ сезон/месяц года
  , time :: Int -- ^ период моделирования
  , startTime :: Int -- ^ время прошедшее сначала
  , timeDiffWeek :: Int -- ^ прошедшие недели
  , timeDiffMonth :: Int -- ^ прошедшие месяцы
  , timeDiffYear :: Int -- ^ количество года
  , startWeek :: Bool -- ^ началась ли новая неделя
  , price :: Double -- ^ стоимость прививки
  , fond :: Double -- ^ денежный фонд
  , isFocus :: Maybe Int -- ^ какой город сейчас в фокусе
  , cities :: [City] -- ^ города
  , gen :: StdGen -- ^ генератор для случайных чисел
  } deriving (Eq, Show)

-- * Город
data City = City
  { population :: Double -- ^ население города
  , sick :: Double -- ^ процент больных
  , immune :: Double -- ^ процент здоровых
  , vaccine :: Maybe Int -- ^ количество прививок
  , vaccinated :: [(Int, Int)] -- ^ число вакцинированых, показывает сколько недель назад произошла вакцинация
  , ill :: [(Int, Int)] -- ^ число больных, показывает сколько недель болеют
  , trafficIn :: Double -- ^ трафик города
  , trafficOut :: Double -- ^ трафик между городами
  , isEpidemic :: Bool -- ^ началась ли эпидемия
  } deriving (Eq, Show)

data Season =
  January | February | March | April | May |
  June | July | August | September | October | November | December
  deriving (Eq, Show)


class World state where
  -- | Пересчитать актальное состояние
  recalculation :: state -> state
  -- | Обновить состояние и начать новый день
  nextDay :: state -> state
  -- | Инициализировать эксперимент
  initDefault :: StdGen -> state

class Town town where
  -- | Расчиать актальное состояние
  calculation :: Season ->  StdGen -> town -> (StdGen, town)
  -- | Посчитать налоги
  tax :: Double -> town -> Double
  -- | Инициализировать город
  initDefaultCity :: town

class Department state where
  -- | Расчитать налоги во всех городах
  taxPayments :: state -> state

instance World State where
  recalculation state =
    let (g, newCities) = Data.List.mapAccumL (calculation $ season state) (gen state) (cities state)
    in state {gen = g, cities = newCities}

  nextDay state =
    let
      -- newCities = (cities state)
      -- newTime = (time state) - 1
      newTime = ((startTime state) + 1)
      newTimeDiffWeek = (timeDiffWeek state) + 1
      newTimeDiffMonth = (timeDiffMonth state) + 1
      newTimeDiffYear = (timeDiffYear state) + 1
      newStartWeek = if newTimeDiffWeek `div` 7 /= 0 then False else (startWeek state)
      newSeason = if newTimeDiffMonth `div` 30 /= 0 then nextMonth (season state) else (season state)
      -- newSeason =
    in
      (recalculation $ taxPayments $ state)
      { startTime = newTime
      , timeDiffWeek = newTimeDiffWeek `mod` 7
      , startWeek = newStartWeek
      , timeDiffMonth = newTimeDiffMonth `mod` 30
      , timeDiffYear = newTimeDiffYear `mod` 360
      , season = newSeason
      }

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


instance Town City where
  initDefaultCity = City
    { population = 0.1
    , sick = 0
    , immune = 0
    , vaccine = Nothing
    , vaccinated = []
    , ill = []
    , trafficIn = 0.3
    , trafficOut = 0.3
    , isEpidemic = False
    }

  tax price city =
    workable * healthy * duty - price * (fromIntegral vac)
    where
      healthy = (1 - (sick city)) * (population city)
      vac =
        case (vaccine city) of
          (Just n) -> n
          Nothing -> 0

  calculation season gen town = (newGen, newTown)
    where
      numSick = (population town) * (sick town)

      numImm = (population town) * (immune town)

      monthCoef = morbidity season

      numNotImm = (population town) * (1 - (immune town) - (sick town))

      vaccinatedTmp = Data.List.map (\(num,nDay) -> (num, nDay + 1)) (vaccinated town)

      newImm = Data.List.foldl
        (\s (num, nDay) -> if nDay `div` 7 >= 3 then s + (fromIntegral num) else s)
        0.0 vaccinatedTmp

      newVaccinated = (fromMaybe $ vaccine town) ++
        Data.List.filter (\(num, nDay) -> nDay `div` 7 < 3) vaccinatedTmp

      globalCoef = (trafficIn town) * monthCoef * numSick

      (localCoef, newGen) = randomR (0 :: Double, globalCoef) gen --

      newCountSick = round (numNotImm * localCoef / 6000) --

      healTmp = if (isEpidemic town) then (ill town)
        else Data.List.map (\(num,nDay) -> (num, nDay - 1)) (ill town)

      heal = Data.List.foldl
        (\s (num, nDay) -> if nDay <= 0 then s + num else s) 0 healTmp

      newIll = (sickToIll $ fromIntegral newCountSick) ++
        Data.List.filter (\(num, nDay) -> nDay > 0) healTmp

      newSick = clamp 0 (population town) (fromIntegral $ illToDouble newIll)

      newTown =
        town
        { immune = (clamp 0 (population town) (newImm + numImm + (fromIntegral heal))) / (population town)
        , vaccinated = minBound newVaccinated
        , sick = newSick / (population town)
        , ill = minBound newIll
        , vaccine = Nothing
        , isEpidemic = if newSick / (population town) > 0.45 then True else False
        }

      minBound :: [(Int, Int)] -> [(Int, Int)]
      minBound asocList
        | Prelude.length asocList > 0 = asocList
        | otherwise = []

      illToDouble :: [(Int, Int)] -> Int
      illToDouble [] = 0
      illToDouble ((n,_):xs) = n + illToDouble xs

      fromMaybe :: Maybe Int -> [(Int, Int)]
      fromMaybe (Just n) = [(n, 0)]
      fromMaybe Nothing = []

instance Department State where
  taxPayments state =
    state {fond = (fond state) + (func state)}
    where
      func s =
        Prelude.foldl (\b a ->
        if (isEpidemic a) then 0.0 else b + tax (price s) a) 0.0 (cities s)

sickToIll :: Double -> [(Int, Int)]
sickToIll count =
  let
    w2 = ceiling $ count * 0.6
    w3 = ceiling $ count * 0.15
    w1 = ceiling $ count * 0.25
  in [(w1, 7), (w2, 14), (w3, 21)]

morbidity :: Season -> Double
morbidity season =
  case season of
    January -> 0.7
    February -> 0.85
    March -> 1.0
    April -> 0.85
    May -> 0.7
    June -> 0.55
    July -> 0.4
    August -> 0.25
    September -> 0.1
    October -> 0.25
    November -> 0.4
    December -> 0.55

nextMonth :: Season -> Season
nextMonth season =
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

roundTo2 :: (RealFrac a) => a -> a
roundTo2 x = fromIntegral f / 100
                 where f = round (x * 100)
