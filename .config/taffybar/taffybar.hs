{-# OPTIONS_GHC -O2 -tmpdir /tmp -optc -O2 #-}

import Control.Applicative ((<$>))
import Control.Monad (guard)

import Daw.Hosts

import System.Information.CPU
import System.Information.Memory

import System.Posix.Unistd (getSystemID, nodeName)

import System.Taffybar

import System.Taffybar.Battery
import System.Taffybar.CommandRunner
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.Pager (colorize, escape)
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Weather

import System.Taffybar.Widgets.PollingGraph

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main :: IO ()
main = do
  host <- nodeName <$> getSystemID
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
      weatherCfg = (defaultWeatherConfig "EGLC") { weatherTemplate = "$tempC$ °C" }
      pagerCfg = defaultPagerConfig { emptyWorkspace = colorize "grey" "" . escape }
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M:%S</span>" 1
      pager = taffyPagerNew pagerCfg
      note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew weatherCfg 60
      mpris = mprisNew defaultMPRISConfig
      mem = pollingGraphNew memCfg 2 memCallback
      cpu = pollingGraphNew cpuCfg 1 cpuCallback
      tray = systrayNew
      dropbox = commandRunnerNew 5 "dstatline" [] "error calling dstatline" "blue"
      cputemp = commandRunnerNew 5 "cputemp" [] "error calling cputemp" "red"
      batt = textBatteryNew ("$percentage$%" ++ (guard (not $ isGladys host) >> " ($time$)")) 10
  defaultTaffybar defaultTaffybarConfig
      { startWidgets =
          (guard (isLaptop host) >> [batt])
          ++ [pager]
          ++ (guard (not $ isWork host) >> [dropbox])
          ++ [note]
      , endWidgets = [ tray, wea, clock, mem, cputemp, cpu, mpris ]
      }
