module Main where

import Game
import Tamagotchi

main :: IO ()
main = game $ newTamagotchi "Cam Brady"
