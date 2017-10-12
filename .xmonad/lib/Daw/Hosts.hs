module Daw.Hosts where

import Data.List.Split (splitOn)

isVera :: String -> Bool
isVera = (== "vera")

isGladys :: String -> Bool
isGladys = (== "gladys")

isLaptop :: String -> Bool
isLaptop h = isGladys h || h == "winona"

isHomeMachine :: String -> Bool
isHomeMachine h = isVera h || isLaptop h

isWork :: String -> Bool
isWork = (== "com") . last . splitHostName

splitHostName :: String -> [String]
splitHostName = splitOn "."

chromeName :: String -> String
chromeName h
  | isWork h  = "google-chrome"
  | otherwise = "chromium"

