

module Settings.Update where

import Prelude
import Miso
import Miso.String
import Data.List
import Data.Time.Clock
import Data.List.Index
import System.Random

import State

data Action
  = SetNumber Int
  | SetSeason Season
  | SetTime Int
  | SetPrice Double
  | SetFond Double
  | SetPopulation Int Double
  | SetSick Int Double
  | SetImmune Int Double
  | SetTraffic Int Double
  | Random
  | Check
  | Next
  | NoOp
  deriving (Show, Eq)

update :: Action -> State -> Effect Action State
update Check state = state <# do
  putStrLn "Hello World" >> pure NoOp
update (SetNumber n) state = state {number = n, cities = setCities (cities state) (clamp 1 10 n)} <# do
  putStrLn (show n) >> pure NoOp
update (SetSeason s) state = noEff $ state {season = s}
update (SetTime t) state = noEff $ state {time = t}
update (SetPrice p) state = noEff $ state {price = p}
update (SetFond f) state = noEff $ state {fond = f * 1000}
update (SetPopulation n p) state =
    noEff $ state {cities = (imap (\i c -> if i == n then (c {population = (clamp 0 15 p) * 1000}) else c) (cities state))}
update (SetSick n s) state =
    noEff $ state {cities = (imap (\i c ->
    let
      setSick = clamp 0 1 s
      setIll = sickToIll $ setSick * (population c)
    in
      if i == n then (c {sick = setSick, ill = setIll}) else c
      ) (cities state))}
update (SetImmune n p) state =
    noEff $ state {cities = (imap (\i c -> if i == n then (c {immune = clamp 0 1 p}) else c) (cities state))}
update (SetTraffic n t) state =
    noEff $ state {cities = (imap (\i c -> if i == n then (c {trafficIn = clamp 0 1 t}) else c) (cities state))}
update Next state = noEff state
update NoOp state = noEff state
update Random state = noEff $
  state {cities = newCities, gen = g}
  where
    (g, newCities) = Data.List.mapAccumL setR (gen state) (cities state)
    setR g0 city = (g4, newC)
      where
        (pop, g1) = randomR (5 :: Int, 15) g0
        (s, g2) = randomR (0 :: Double, 0.45) g1
        (imm, g3) = randomR (0 :: Double, (1 - s)) g2
        (traff, g4) = randomR (0 :: Double, 1) g3
        setIll = sickToIll $ s * ((fromIntegral pop) * 1000)
        newC = city {population = (fromIntegral pop) * 1000, sick = s, immune = imm, trafficIn = traff, ill = setIll}

setCities :: [City] -> Int -> [City]
setCities cities n =
  if m <= n then
    cities ++ (fmap (\_ -> initDefaultCity) [m..(n - m - 1)])
  else
    Prelude.take n cities
  where
    m = Prelude.length cities
