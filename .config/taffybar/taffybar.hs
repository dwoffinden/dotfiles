{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Daw.Hosts
import System.Posix.Unistd (getSystemID, nodeName)
import System.Taffybar
import System.Taffybar.Hooks
import System.Taffybar.SimpleConfig
import System.Taffybar.Information.Battery
import System.Taffybar.Information.Memory
import System.Taffybar.Widget
import System.Taffybar.Widget.CPUMonitor
import System.Taffybar.Widget.CommandRunner
import System.Taffybar.Widget.FreedesktopNotifications
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.SNITray
import System.Taffybar.Widget.Weather
import System.Taffybar.Widget.Workspaces

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

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
      myWorkspacesConfig =
        defaultWorkspacesConfig
        { minIcons = 1
        , widgetGap = 0
        , showWorkspaceFn = hideEmpty
        }
      workspaces = workspacesNew myWorkspacesConfig
      layout = layoutNew defaultLayoutConfig
      windows = windowsNew defaultWindowsConfig
      weatherCfg = (defaultWeatherConfig "EGLC") { weatherTemplate = "$tempC$ °C" }
      clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M:%S</span>" 1
      note = notifyAreaNew defaultNotificationConfig
      wea = liftIO $ weatherNew weatherCfg 60
      mem = pollingGraphNew memCfg 2 memCallback
      cpu = cpuMonitorNew cpuCfg 1 "cpu"
      tray = sniTrayNew
      dropbox = commandRunnerNew 5 "dstatline" [] "error calling dstatline"
      cputemp = commandRunnerNew 5 "cputemp" [] "error calling cputemp"
      batt = textBatteryNew ("$percentage$%" ++ (guard (not $ isGladys host) >> " ($time$)"))
      myConfig = defaultSimpleTaffyConfig
        { startWidgets =
          (guard (isLaptop host) >> [batt])
          ++ (workspaces : map (>>= buildContentsBox) [ layout, windows ])
          ++ (guard (not $ isWork host) >> [dropbox])
          ++ [note]
        , endWidgets = [ tray, wea, clock, mem, cputemp, cpu ]
        }
  dyreTaffybar $ withBatteryRefresh $
    toTaffyConfig myConfig
