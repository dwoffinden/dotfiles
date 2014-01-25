{-# OPTIONS_GHC -O2 -tmpdir /tmp -optc -O2 -optl -O #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import           Network.MPD (withMPD, pause, previous, next, stop)
import           Network.MPD.Commands.Extensions (toggle)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.Info
import           System.Posix.Unistd hiding (sleep)
import           System.Process
import           Text.Read
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run hiding (safeRunInTerm)

{-
http://hackage.haskell.org/packages/archive/xmonad-contrib/0.8/doc/html/XMonad-Hooks-DynamicLog.html
TODO: xmobar options on the command line? no?
-}

 -- TODO unfix the type of suspend
 -- TODO parameterise by type of trayerWidth (some Integral)? suspend/warn?
data LocalConfig = LocalConfig { hasMpd :: Bool
                               , hasWifi :: Bool
                               , needsXScreensaver :: Bool
                               , trayerWidth :: Int
                               , suspend :: X ()
                               , warn :: String -> X()
                               , homeDir :: FilePath
                               , wallpaper :: FilePath
                               }

myBar = "~" </> ".cabal" </> "bin" </> "xmobar"

myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

--toggleStrutsKey XConfig { XMonad.modMask = mask } = (mask, xK_b)
--toggleStrutsKey conf = ( XMonad.modMask conf, xK_b )
toggleStrutsKey = flip ( (,) . XMonad.modMask ) xK_b

myNormalColour = "#202020"

myFocusedColour = "#ff0000"

myModMask = mod4Mask

{- If urxvt is not installed, use xterm. -}
myTerminal = io $ fromMaybe "xterm" <$> findExecutable "urxvtc"

myWp = (</> "Dropbox" </> "wallpaper" </> "wallpaper-2473668.jpg")

getConfiguration :: (MonadIO m) => m LocalConfig
getConfiguration = do
  home <- io getHomeDirectory
  host <- io $ nodeName <$> getSystemID
  warn' <- io $ maybe (\msg -> safeSpawn "xmessage" [msg])
    (\zty msg -> safeSpawn zty ["--warning", "--text", msg]) <$> findExecutable "zenity"
  return LocalConfig
    { hasMpd  = isVera host
    , hasWifi = isLaptop host
    , needsXScreensaver = isHomeMachine host
    , trayerWidth = tw host
    , suspend = suspend' host
    , warn = warn'
    , homeDir = home
    , wallpaper = myWp home
    }
  where
    isVera = (=="vera")
    isLaptop = flip elem ["gladys", "winona"]
    isHomeMachine h = isVera h || isLaptop h
    tw = (\w -> w - (w * 95 `div` 100) ) -- width, minus a 95% xmobar
         . (fromMaybe 1920) -- sane default for labs
         . (flip lookup [ ("winona", 1024)
                        , ("gladys", 1366)
                        , ("vera",   1920)
                        ])
    suspend' h =
      if (isHomeMachine h)
        then safeSpawn "systemctl" ["suspend"]
        else screenOff

lock :: MonadIO m => m ()
lock = safeSpawn "xdg-screensaver" ["lock"]

screenOff :: MonadIO m => m ()
screenOff = safeSpawn "xset" ["dpms", "force", "off"]

sleep :: MonadIO m => Rational -> m ()
sleep = io . threadDelay . seconds

-- TODO pattern match on conf record?
myStartupHook conf = do
  setWMName "LG3D" --fuck java
  safeSpawn "setxkbmap" ["-layout", "gb"]
  safeSpawn "xsetroot" ["-cursor_name", "left_ptr"]
  safeSpawn "xrdb" ["-merge", ( (homeDir conf) </> ".Xresources" )]
  safeSpawn "feh" ["--no-fehbg", "--bg-fill", wallpaper conf]
  if needsXScreensaver conf
    then safeSpawn "xscreensaver" ["-no-splash"]
    else return ()
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
                     , "--width", show $ trayerWidth conf
                     , "--transparent", "true"
                     , "--tint", "0"
                     , "--alpha", "0"
                     , "--expand", "true"
                     , "--padding", "0"
                     ]
  safeSpawn "xcompmgr" ["-cC"]

ifNotRunning :: MonadIO m => FilePath -> IO a -> m ()
ifNotRunning prog hook = io . void . forkIO $ do -- TODO bench forkIO vs xfork?
  noPids <- null <$> pgrep prog
  if noPids
    then void hook
    else return ()

-- TODO: A Bool version that can stop as soon as it finds a PID.
--       Alternatively: Use lazy IO/conduit to delay reading the later files?
pgrep :: (MonadIO m, Integral i, Read i, Show i) => String -> m [i]
pgrep comm = io $ do
  allPids <- mapMaybe readMaybe <$> getDirectoryContents "/proc"
  filterM (\p -> maybe False (==comm) <$>
    readFileMaybe ("/proc" </> show p </> "comm")) allPids
  where
    readFileMaybe f =
       either (const Nothing) Just <$>
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
  onWorkspace "=" imLayout $
--layoutHintsToCenter $
      (Tall 1 (2/100) (1/2))
  ||| (ThreeColMid 1 (2/100) (1/2))
  ||| (wide 1 (2/100) (1/2))
  ||| Grid
  where
    wide a b c =
      renamed [Replace "Wide"] $
        Mirror $ Tall a b c
    doubleIM n i l lp r rp =
      renamed [Replace n] $
        withIM l lp $ reflectHoriz $ withIM r rp $ layoutHints i
    imLayout   = doubleIM "IM" Grid 0.15 (Role "buddy_list")
                                    0.2  (ClassName "Skype")

{- Workspace Identifiers. Must correspond to keys in mkKeyMap format. -}
--myWorkspaces = ["`"] ++ map show [1..9] ++ ["0", "-", "="]
myWorkspaces = map (:"") "`1234567890-="

myKeys conf c = mkKeymap c $
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
    , ("M-s",              (when (hasMpd conf) $ mpd_ $ pause True) >> sleep 1 >> screenOff)
    {- Take a screenshot, save as 'screenshot.png' -}
    , ("<Print>",          safeSpawn "import" [ "-window", "root"
                                              , "screenshot.png" ])
    , ("<XF86Eject>",      safeSpawn "eject" ["-T"])
    , ("<XF86Calculator>", safeSpawnProg "speedcrunch")
    , ("<XF86Search>",     (warn conf) "search")
    , ("<XF86Mail>",       (warn conf) "mail")
    , ("<XF86WebCam>",     (warn conf) "smile")
    , ("<XF86Eject>",      safeSpawnProg "eject")
    ]
    {- Workspace Switching -}
    ++ [ (m ++ k, windows $ f k)
       | k <- XMonad.workspaces c
         , (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-")]
    ]
    {- Screen Switching -}
    ++ [ (m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip ["w", "e", "r"] [0..]
         , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
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
    ++ [ ( k, runOrRaise "chromium" (className =? "Chromium"))
       | k <- ["M-c", "<XF86HomePage>"]
    ]
    {- Screen Locking -}
    ++ [ (k , lock >> sleep 1 >> screenOff)
       | k <- ["M-S-x", "<XF86ScreenSaver>"]
    ]
    ++ [ (k , lock >> sleep 1 >> (suspend conf))
       | k <- ["M-x", "<XF86Sleep>"]
    ]
{-
    {- WiFi manager -}
    ++ ( guard (hasWifi conf) >>
      [ ("M-S-n", safeRunProgInTerm "wicd-curses") ]
    )
-}
    {- MPC keys, media player UI -}
    ++ ( guard (hasMpd conf) >>
      [ (k, mpd_ c)
      | (k, c) <- [ ("<XF86AudioPlay>", toggle)
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
    mpd_ =
      io . void . withMPD
    safeRunInTerm c o =
      asks (terminal . config) >>= flip safeSpawn (["-e", c] ++ o)
    safeRunProgInTerm =
      flip safeRunInTerm []

myConfig = do
  c <- getConfiguration
  t <- myTerminal
  return defaultConfig
    { normalBorderColor  = myNormalColour
    , focusedBorderColor = myFocusedColour
    , modMask            = myModMask
    , terminal           = t
    , startupHook        = myStartupHook c
    , manageHook         = myManageHook
    , handleEventHook    = myHandleEventHook
    , logHook            = myLogHook
    , layoutHook         = myLayout
    , workspaces         = myWorkspaces
    , keys               = myKeys c
    }

main = myConfig >>= statusBar myBar myPP toggleStrutsKey >>= xmonad
