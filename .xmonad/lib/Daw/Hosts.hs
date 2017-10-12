module Daw.Hosts where

import Data.List.Split (splitOn)

isVera :: String -> Bool
isVera = (== "vera")

isHomeLaptop :: String -> Bool
isHomeLaptop = (`elem` ["gladys", "winona"])

isHomeMachine :: String -> Bool
isHomeMachine h = isVera h || isHomeLaptop h

isLaptop :: String -> Bool
isLaptop h = isHomeLaptop h || isWorkLaptop h

isWork :: String -> Bool
isWork = (== "com") . last . splitHostName

-- TODO this by pattern matching?
isWorkLaptop :: String -> Bool
isWorkLaptop h = isWork h && ((== "roam") . (!! 3) . reverse . splitHostName) h

isWorkDesktop :: String -> Bool
isWorkDesktop h = isWork h && not (isWorkLaptop h)

splitHostName :: String -> [String]
splitHostName = splitOn "."

chromeName :: String -> String
chromeName h
  | isWork h  = "google-chrome"
  | otherwise = "chromium"

hasMpd :: String -> Bool
hasMpd = isVera

needsXScreensaver :: String -> Bool
needsXScreensaver = isHomeMachine

