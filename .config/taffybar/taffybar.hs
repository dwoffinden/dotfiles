{-# OPTIONS_GHC -O2 -tmpdir /tmp -optc -O2 #-}
import Control.Applicative
import Control.Monad

import System.Information.CPU
import System.Information.Memory
import System.Posix.Unistd (getSystemID, nodeName)

import System.Taffybar

import System.Taffybar.Battery
import System.Taffybar.CommandRunner
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Weather

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main = do
  host <- nodeName <$> getSystemID
  let getTld = reverse . takeWhile (/= '.') . reverse
      isWork h = getTld h == "com" || h == "daw-glaptop"
      notWork = not $ isWork host
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
      weatherCfg = (defaultWeatherConfig "EGLC") { weatherTemplate = "$tempC$ °C" }
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M:%S</span>" 1
      log = taffyPagerNew defaultPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew weatherCfg 10
      mpris = mprisNew
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      dropbox = commandRunnerNew 2 "dstatline" [] "error calling dstatline" "blue"
      cputemp = commandRunnerNew 5 "cputemp" [] "error calling cputemp" "red"
      batt = textBatteryNew "$percentage$% ($time$)" 2
  defaultTaffybar defaultTaffybarConfig
      { startWidgets = [ batt, log] ++ (guard notWork >> [ dropbox ]) ++ [ note ]
      , endWidgets = [ tray, wea, clock, mem, cputemp, cpu, mpris ]
      }
