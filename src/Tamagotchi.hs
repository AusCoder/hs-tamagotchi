module Tamagotchi where

data Tamagotchi = Tamagotchi
  { name :: String
  , age :: Int
  , hunger :: Int
  } deriving (Show, Eq)

newTamagotchi :: String -> Tamagotchi
newTamagotchi s = Tamagotchi s 0 25

defaultFeed :: Tamagotchi -> Tamagotchi
defaultFeed = feed 5

feed :: Int -> Tamagotchi -> Tamagotchi
feed food (Tamagotchi name age hunger) =
  Tamagotchi name age (hunger + food)

defaultTick :: Tamagotchi -> Tamagotchi
defaultTick = tickWithOpts 1 1

tickWithOpts :: Int -> Int -> Tamagotchi -> Tamagotchi
tickWithOpts growthRate hungerRate (Tamagotchi name age hunger) =
  Tamagotchi name (age + growthRate) (hunger - hungerRate)

isDead :: Tamagotchi -> Bool
isDead (Tamagotchi name age hunger) =
  age > 100 || hunger < 0