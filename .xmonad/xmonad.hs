{-# OPTIONS_GHC -O2 -tmpdir /tmp -optc -O2 #-}
{-# LANGUAGE GADTs, OverloadedStrings #-}
import           Control.Applicative ((<$>),pure)
import           Control.Concurrent (forkIO,threadDelay)
import           Control.Exception (tryJust)
import           Control.Monad (filterM,guard,void,when)
import           Data.Map (Map)
import           Data.Maybe (fromMaybe,isNothing,mapMaybe)
import qualified Network.MPD as MPD (withMPD, pause, previous, next, stop)
import qualified Network.MPD.Commands.Extensions as MPD (toggle)
import           System.Directory (doesFileExist,findExecutable,getDirectoryContents,getHomeDirectory)
import           System.Exit (exitSuccess)
import           System.FilePath ((</>))
import           System.IO (hClose,hGetLine,IOMode(ReadMode),openFile)
import           System.IO.Error (isDoesNotExistError)
import           System.Posix.Types (ProcessID)
import           System.Posix.Unistd (getSystemID,nodeName)
import           System.Random (randomRIO)
import           Text.Read (readMaybe)
import           XMonad
import           XMonad.Actions.WindowGo (runOrRaise)
import           XMonad.Hooks.DynamicLog (PP,ppCurrent,statusBar,wrap,xmobarColor,xmobarPP)
import           XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import           XMonad.Hooks.ManageDocks (avoidStruts)
import           XMonad.Hooks.ManageHelpers (doFullFloat,isFullscreen)
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Layout.Grid
{-
import           XMonad.Layout.LayoutHints
-}
import           XMonad.Layout.NoBorders (smartBorders)
import           XMonad.Layout.Renamed
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (mkKeymap)
import           XMonad.Util.Run (safeSpawn,safeSpawnProg,seconds)

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

myBar :: String
myBar = "exec " ++ "~" </> ".cabal" </> "bin" </> "xmobar"

myPP :: PP
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

toggleStrutsKey :: XConfig l -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = myMask } = (myMask, xK_b)

myNormalColour :: String
myNormalColour = "#202020"

myFocusedColour :: String
myFocusedColour = "#ff0000"

myModMask :: KeyMask
myModMask = mod4Mask

{- If urxvt is not installed, use xterm. -}
myTerminal :: IO FilePath
myTerminal = fromMaybe "xterm" <$> findExecutable "urxvtc"

getConfiguration :: IO (LocalConfig X Int)
getConfiguration = do
  home <- getHomeDirectory
  host <- nodeName <$> getSystemID
  warn <- maybe
    (\msg -> safeSpawn "xmessage" [msg])
    (\zty msg -> safeSpawn zty ["--warning", "--text", msg])
    <$> findExecutable "zenity"
  wp <- pickRandomWallpaper home
  return LocalConfig
    { hasMpd  = isVera host
    , hasWifi = isLaptop host
    , needsXScreensaver = isHomeMachine host
    , trayerWidth = tw host
    , suspendAction = suspend host
    , warnAction = warn
    , homeDir = home
    , wallpaper = wp
    , chromiumName = chromium host
    }
  where
    isVera =
      (== "vera")
    isLaptop =
      flip elem ["gladys", "winona"]
    isHomeMachine h =
      isVera h || isLaptop h
    isLabs =
      (== "doc.ic.ac.uk") . getDomain
    getDomain =
      dropWhile (== '.') . dropWhile (/= '.')
    tw =
      (\w -> w - (w * 95 `div` 100) ) -- width, minus a 95% xmobar
      . (fromMaybe 1920) -- sane default for labs
      . (flip lookup [ ("winona", 1024)
                     , ("gladys", 1366)
                     , ("vera",   1920)
                     ])
    suspend h
      | isLaptop h = safeSpawn "systemctl" ["hibernate"] -- Stupid radeon drivers
      | isVera h   = safeSpawn "systemctl" ["suspend"]
      | otherwise  = screenOff
    chromium h
      | isLabs h  = "chromium-browser"
      | otherwise = "chromium"
    pickRandomWallpaper home = do
      let wps = home </> "Dropbox" </> "wallpaper"
      candidates <- getDirectoryContents wps >>= filterM doesFileExist . map (wps </>)
      index <- randomRIO (0, length candidates - 1)
      return $ candidates !! index

lock :: MonadIO m => m ()
lock = safeSpawn "xdg-screensaver" ["lock"]

screenOff :: MonadIO m => m()
screenOff = safeSpawn "xset" ["dpms", "force", "off"]

sleep :: MonadIO m => Rational -> m ()
sleep = io . threadDelay . seconds

myStartupHook :: LocalConfig m t -> X ()
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

ifNotRunning :: MonadIO m => String -> IO () -> m ()
ifNotRunning prog hook = io $ void $ forkIO $ do
  notRunning <- isNothing <$> findPid prog
  when notRunning hook

-- | Find the first PID of the first process with given `comm', if one exists.
-- We could find all of them by replacing `findM' with `filterM'...
findPid :: String -> IO (Maybe ProcessID)
findPid comm =
  getDirectoryContents "/proc" >>= findM matchesComm . mapMaybe readMaybe
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

-- | This generalizes the list-based 'find' function.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = do
  y <- f x
  if y then return (Just x) else findM f xs

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

myLogHook :: X ()
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
myWorkspaces :: [String]
myWorkspaces = map pure "`1234567890-="

myKeys :: LocalConfig X t -> XConfig Layout -> Map (KeyMask, KeySym) (X())
myKeys LocalConfig { warnAction = warn
                   , hasMpd = mpd
--                   , hasWifi = wifi
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
    ++ [ (k , (when mpd $ doMpd $ MPD.pause True) >> lock >> sleep 4 >> screenOff)
       | k <- ["M-s", "<XF86ScreenSaver>"]
    ]
    ++ [ (k , lock >> sleep 4 >> screenOff)
       | k <- ["M-S-s"]
    ]
    ++ [ (k , lock >> sleep 4 >> suspend)
       | k <- ["M-C-S-s", "<XF86Sleep>"]
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
      | (k, comm) <- [ ("<XF86AudioPlay>", MPD.toggle)
                     , ("<XF86AudioPrev>", MPD.previous)
                     , ("<XF86AudioNext>", MPD.next)
                     , ("<XF86AudioStop>", MPD.stop)
                     ]
      ] ++
      [ (k, safeRunProgInTerm "ncmpcpp")
      | k <- ["M-S-m", "<XF86AudioMedia>"]
      ]
    )
  where
    doMpd =
      io . void . MPD.withMPD
    safeRunInTerm comm args =
      asks (terminal . config) >>= flip safeSpawn (["-e", comm] ++ args)
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

main :: IO ()
main = myConfig >>= statusBar myBar myPP toggleStrutsKey >>= xmonad

