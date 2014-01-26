{-# OPTIONS_GHC -O2 -tmpdir /tmp -optc -O2 #-}
{-# LANGUAGE GADTs, OverloadedStrings #-}
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           Network.MPD (withMPD, pause, previous, next, stop)
import           Network.MPD.Commands.Extensions (toggle)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.Posix.Unistd hiding (sleep)
import           Text.Read
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run hiding (safeRunInTerm)

{-
http://hackage.haskell.org/packages/archive/xmonad-contrib/0.8/doc/html/XMonad-Hooks-DynamicLog.html
TODO: xmobar options on the command line? no?
-}

data LocalConfig m i =
  (MonadIO m, Integral i, Num i, Show i) => LocalConfig
    { hasMpd :: Bool
    , hasWifi :: Bool
    , needsXScreensaver :: Bool
    , trayerWidth :: i
    , suspendAction :: m ()
    , warnAction :: String -> m ()
    , homeDir :: FilePath
    , wallpaper :: FilePath
    , chromiumName :: String
    }

myBar = "~" </> ".cabal" </> "bin" </> "xmobar"

myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

toggleStrutsKey XConfig { XMonad.modMask = myMask } = (myMask, xK_b)

myNormalColour = "#202020"

myFocusedColour = "#ff0000"

myModMask = mod4Mask

{- If urxvt is not installed, use xterm. -}
myTerminal = fromMaybe "xterm" <$> findExecutable "urxvtc"

getConfiguration = do
  home <- getHomeDirectory
  host <- nodeName <$> getSystemID
  warn <- maybe
    (\msg -> safeSpawn "xmessage" [msg])
    (\zty msg -> safeSpawn zty ["--warning", "--text", msg])
    <$> findExecutable "zenity"
  return LocalConfig
    { hasMpd  = isVera host
    , hasWifi = isLaptop host
    , needsXScreensaver = isHomeMachine host
    , trayerWidth = tw host
    , suspendAction = suspend host
    , warnAction = warn
    , homeDir = home
    , wallpaper = wp home
    , chromiumName = chromium host
    }
  where
    wp = (</> "Dropbox" </> "wallpaper" </> "wallpaper-2473668.jpg")
    isVera = (=="vera")
    isLaptop = flip elem ["gladys", "winona"]
    isHomeMachine h = isVera h || isLaptop h
    -- TODO explicit isLabs / isWork
    tw = (\w -> w - (w * 95 `div` 100) ) -- width, minus a 95% xmobar
         . (fromMaybe 1920) -- sane default for labs
         . (flip lookup [ ("winona", 1024)
                        , ("gladys", 1366)
                        , ("vera",   1920)
                        ])
    suspend h =
      if (isHomeMachine h)
        then safeSpawn "systemctl" ["suspend"]
        else screenOff
    chromium h =
      if (isHomeMachine h) then "chromium" else "chromium-browser"

lock :: MonadIO m => m ()
lock = safeSpawn "xdg-screensaver" ["lock"]

screenOff :: MonadIO m => m()
screenOff = safeSpawn "xset" ["dpms", "force", "off"]

sleep :: MonadIO m => Rational -> m ()
sleep = io . threadDelay . seconds

myStartupHook LocalConfig { homeDir = home
                          , wallpaper = wp
                          , trayerWidth = tw
                          , needsXScreensaver = xScreensaver
                          } = do
  setWMName "LG3D" --fuck java
  safeSpawn "setxkbmap" ["-layout", "gb"]
  safeSpawn "xsetroot" ["-cursor_name", "left_ptr"]
  safeSpawn "xrdb" ["-merge", (home </> ".Xresources")]
  safeSpawn "feh" ["--no-fehbg", "--bg-fill", wp]
  when xScreensaver $ safeSpawn "xscreensaver" ["-no-splash"]
  ifNotRunning "urxvtd" $ safeSpawn "urxvtd" ["-q", "-o"]
  ifNotRunning "trayer" $ safeSpawn "trayer"
    [ "--edge", "top"
    , "--align", "right"
    , "--margin", "0"
    , "--SetDockType", "true"
    , "--SetPartialStrut", "true"
    , "--heighttype", "pixel"
    , "--height", "16"
    , "--widthtype", "pixel"
    , "--width", show tw
    , "--transparent", "true"
    , "--tint", "0"
    , "--alpha", "0"
    , "--expand", "true"
    , "--padding", "0"
    ]
  safeSpawn "xcompmgr" ["-c"]

ifNotRunning prog hook = io $ void $ forkIO $ do -- TODO bench forkIO vs xfork?
  noPids <- null <$> pidof prog
  when noPids (void hook)

-- TODO: A Bool version that can stop as soon as it finds one PID.
--       Alternatively: Use lazy IO/conduit to delay reading the later files?
pidof :: (MonadIO m, Integral i, Read i, Show i) => String -> m [i]
pidof comm = io $
  mapMaybe readMaybe <$> getDirectoryContents "/proc" >>= filterM matchesComm
  where
    matchesComm pid =
      either (const False) (== comm) <$> tryReadLine ("/proc" </> show pid </> "comm")
    tryReadLine f =
      tryJust (guard . isDoesNotExistError) (readFirstLine f)
    readFirstLine f = do
      h <- openFile f ReadMode
      str <- hGetLine h
      hClose h
      return str

myManageHook = composeAll
  [ className =? "MPlayer"        --> doFloat
  , className =? "Zenity"         --> doFloat
  , className =? "Xmessage"       --> doFloat
  , resource  =? "desktop_window" --> doIgnore
  , resource  =? "kdesktop"       --> doIgnore
--, isFullscreen                  --> (doF W.focusDown <+> doFullFloat)
  , isFullscreen                  --> doFullFloat
  ]

myHandleEventHook = fullscreenEventHook

myLogHook = return ()

myLayout = smartBorders $ avoidStruts $
--layoutHintsToCenter $
      (Tall 1 (2/100) (1/2))
  ||| (ThreeColMid 1 (2/100) (1/2))
  ||| (wide 1 (2/100) (1/2))
  ||| Grid
  where
    wide a b c =
      renamed [Replace "Wide"] $ Mirror $ Tall a b c

{- Workspace Identifiers. Must correspond to keys in mkKeyMap format. -}
myWorkspaces = map pure "`1234567890-="

myKeys LocalConfig { warnAction = warn
                   , hasMpd = mpd
                   , hasWifi = wifi
                   , suspendAction = suspend
                   , chromiumName = chromium
                   } c =
  mkKeymap c $
    [ ("M-S-<Return>",     safeSpawnProg $ XMonad.terminal c)
    , ("M-p",              safeSpawnProg "dmenu_run")
--  , ("M-S-p",            spawn "gmrun")
    , ("M-S-c",            kill)
    , ("M-<Space>",        sendMessage NextLayout)
    , ("M-S-<Space>",      setLayout $ XMonad.layoutHook c)
    , ("M-n",              refresh)
    , ("M-<Tab>",          windows W.focusDown)
    , ("M-j",              windows W.focusDown)
    , ("M-S-<Tab>",        windows W.focusUp)
    , ("M-k",              windows W.focusUp)
    , ("M-m",              windows W.focusMaster)
    , ("M-<Return>",       windows W.swapMaster)
    , ("M-S-j",            windows W.swapDown)
    , ("M-S-k",            windows W.swapUp)
    , ("M-h",              sendMessage Shrink)
    , ("M-l",              sendMessage Expand)
    , ("M-t",              withFocused $ windows . W.sink)
    , ("M-,",              sendMessage (IncMasterN 1))
    , ("M-.",              sendMessage (IncMasterN (-1)))
    , ("M-S-q",            io exitSuccess)
    , ("M-q",              recompile False >>= (flip when) (safeSpawn "xmonad" ["--restart"]))
    , ("M-a",              safeRunInTerm "alsamixer" [])
    {- Power off screen -}
    , ("M-S-s",            sleep 1 >> screenOff)
    , ("M-s",              (when mpd $ doMpd $ pause True) >> sleep 1 >> screenOff)
    {- Take a screenshot, save as 'screenshot.png' -}
    , ("<Print>",          safeSpawn "import" [ "-window", "root", "screenshot.png" ])
    , ("<XF86Eject>",      safeSpawn "eject" ["-T"])
    , ("<XF86Calculator>", safeSpawnProg "speedcrunch")
    , ("<XF86Search>",     warn "search")
    , ("<XF86Mail>",       warn "mail")
    , ("<XF86WebCam>",     warn "smile")
    , ("<XF86Eject>",      safeSpawnProg "eject")
    ]
    {- Workspace Switching -}
    ++ [ (m ++ k, windows $ f k)
       | k <- XMonad.workspaces c, (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-")]
    ]
    {- Screen Switching -}
    ++ [ (m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip ["w", "e", "r"] [0..], (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
    ]
    {- Volume Controls -}
    ++ [ (k, safeSpawn "amixer" ["set", "Master", a])
       | (k, a) <- [ ("<XF86AudioMute>", "toggle")
                   , ("<XF86AudioRaiseVolume>", "1+")
                   , ("<XF86AudioLowerVolume>", "1-")
                   ]
    ]
    {- Spawning Browsers -}
    ++ [ ( k, runOrRaise "firefox" (className =? "Firefox"))
       | k <- ["M-f"]
    ]
    ++ [ ( k, runOrRaise chromium (className =? chromium))
       | k <- ["M-c", "<XF86HomePage>"]
    ]
    {- Screen Locking -}
    ++ [ (k , lock >> sleep 1 >> screenOff)
       | k <- ["M-S-x", "<XF86ScreenSaver>"]
    ]
    ++ [ (k , lock >> sleep 1 >> suspend)
       | k <- ["M-x", "<XF86Sleep>"]
    ]
{-
    {- WiFi manager -}
    ++ ( guard wifi >>
      [ ("M-S-n", safeRunProgInTerm "wicd-curses") ]
    )
-}
    {- MPC keys, media player UI -}
    ++ ( guard mpd >>
      [ (k, doMpd comm)
      | (k, comm) <- [ ("<XF86AudioPlay>", toggle)
                     , ("<XF86AudioPrev>", previous)
                     , ("<XF86AudioNext>", next)
                     , ("<XF86AudioStop>", stop)
                     ]
      ] ++
      [ (k, safeRunProgInTerm "ncmpcpp")
      | k <- ["M-S-m", "<XF86AudioMedia>"]
      ]
    )
  where
    doMpd =
      io . void . withMPD
    safeRunInTerm c o =
      asks (terminal . config) >>= flip safeSpawn (["-e", c] ++ o)
    safeRunProgInTerm =
      flip safeRunInTerm []

myConfig = do
  conf <- getConfiguration
  term <- myTerminal
  return defaultConfig
    { normalBorderColor  = myNormalColour
    , focusedBorderColor = myFocusedColour
    , modMask            = myModMask
    , terminal           = term
    , startupHook        = myStartupHook conf
    , manageHook         = myManageHook
    , handleEventHook    = myHandleEventHook
    , logHook            = myLogHook
    , layoutHook         = myLayout
    , workspaces         = myWorkspaces
    , keys               = myKeys conf
    }

main = myConfig >>= statusBar myBar myPP toggleStrutsKey >>= xmonad

