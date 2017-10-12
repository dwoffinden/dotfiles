{-# OPTIONS_GHC -O2 -tmpdir /tmp -optc -O2 #-}
{-# LANGUAGE GADTs, OverloadedStrings #-}

import           Control.Applicative ((<$>),pure)
import           Control.Concurrent (forkIO,threadDelay)
import           Control.Exception (tryJust)
import           Control.Monad (filterM,guard,void,when)
import           Data.Map (Map)
import           Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import           Data.List (stripPrefix)
import qualified Network.MPD as MPD (withMPD, pause, previous, next, stop)
import qualified Network.MPD.Commands.Extensions as MPD (toggle)
import           System.Directory (doesDirectoryExist,doesFileExist,findExecutable,getDirectoryContents,getHomeDirectory)
import           System.Exit (exitSuccess)
import           System.FilePath ((</>), takeFileName)
import           System.IO (hClose,hGetLine,IOMode(ReadMode),openFile)
import           System.IO.Error (isDoesNotExistError, isEOFError)
import           System.Posix.Types (ProcessID)
import           System.Posix.Unistd (getSystemID,nodeName)
import           System.Random (randomRIO)
import           System.Taffybar.Hooks.PagerHints (pagerHints)
import           Text.Read (readMaybe)
import           XMonad
import           XMonad.Actions.CopyWindow (copyToAll,killAllOtherCopies)
import           XMonad.Actions.WindowGo (runOrRaise)
import           XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docksEventHook)
import           XMonad.Hooks.ManageHelpers (doFullFloat,isFullscreen)
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders (smartBorders)
import           XMonad.Layout.Renamed
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (mkKeymap)
import           XMonad.Util.Run (safeSpawn,safeSpawnProg,seconds)

data LocalConfig m =
  (MonadIO m) => LocalConfig
    { hostName :: String
    , warnAction :: String -> m ()
    , homeDir :: FilePath
    , wallpaper :: Wallpaper
    }

data Wallpaper = Fill FilePath | Tile FilePath | None
  deriving (Eq, Show)

myNormalColour :: String
myNormalColour = "#202020"

myFocusedColour :: String
myFocusedColour = "#ff0000"

myModMask :: KeyMask
myModMask = mod4Mask

{- If gnome-terminal nor urxvt are not installed, use xterm. -}
myTerminal :: IO FilePath
myTerminal =
  (fromMaybe "xterm") . listToMaybe . catMaybes <$> (mapM findExecutable ["gnome-terminal", "urxvtc"])

getConfiguration :: IO (LocalConfig X)
getConfiguration = do
  home <- getHomeDirectory
  host <- nodeName <$> getSystemID
  warn <- maybe
    (\msg -> safeSpawn "xmessage" [msg])
    (\ntfy msg -> safeSpawn ntfy [msg])
    <$> findExecutable "notify-send"
  wp <- pickRandomWallpaper home
  return LocalConfig
    { hostName = host
    , warnAction = warn
    , homeDir = home
    , wallpaper = wp
    }
  where
    pickRandomWallpaper home = do
      wps <- filterM (doesDirectoryExist . snd)
        [ (Tile, home </> "Dropbox" </> "wallpaper" </> "tile")
        , (Fill, home </> "Dropbox" </> "wallpaper" </> "simple")
        , (Fill, home </> "Dropbox" </> "wallpaper")
        , (Fill, "/usr/share/backgrounds")
        ]
      if null wps
        then return None
        else do
          candidates <- concat <$> mapM (\(m, dir) -> getDirectoryContents dir >>= filterM doesFileExist . map (dir </>) >>= return . map m) wps
          index <- randomRIO (0, length candidates - 1)
          return $ candidates !! index

isVera :: String -> Bool
isVera = (== "vera")

isHomeMachine :: String -> Bool
isHomeMachine h = isVera h || h `elem` ["gladys", "winona"]

-- TODO: isLaptop that works for home and work

isWork :: String -> Bool
isWork = (== "com") . getTld

getTld :: String -> String
getTld = reverse . takeWhile (/= '.') . reverse

suspend :: MonadIO m => String -> m ()
suspend h
  | isWork h = safeSpawn "dbus-send" [ "--system"
                                     , "--print-reply"
                                     , "--dest=org.freedesktop.UPower"
                                     , "/org/freedesktop/UPower"
                                     , "org.freedesktop.UPower.Suspend"
                                     ]
  | isHomeMachine h = safeSpawn "systemctl" ["suspend"]
  | otherwise  = screenOff

chromeName :: String -> String
chromeName h
  | isWork h  = "google-chrome"
  | otherwise = "chromium"

hasMpd :: String -> Bool
hasMpd = isVera

needsXScreensaver :: String -> Bool
needsXScreensaver = isHomeMachine

lock :: MonadIO m => m ()
lock = safeSpawn "xdg-screensaver" ["lock"]

screenOff :: MonadIO m => m()
screenOff = safeSpawn "xset" ["dpms", "force", "off"]

sleep :: MonadIO m => Rational -> m ()
sleep = io . threadDelay . seconds

myStartupHook :: LocalConfig X -> X ()
myStartupHook LocalConfig { homeDir = home
                          , hostName = host
                          , wallpaper = wp
                          , warnAction = warn
                          } = do
  setWMName "LG3D" --fuck java
  safeSpawn "setxkbmap" ["-layout", "gb"]
  safeSpawn "xsetroot" ["-cursor_name", "left_ptr"]
  safeSpawn "xrdb" ["-merge", (home </> ".Xresources")]
  setWallpaper wp
  when (needsXScreensaver host) $ safeSpawn "xscreensaver" ["-no-splash"]
  ifNotRunning "urxvtd" $ safeSpawn "urxvtd" ["-q", "-o"]
  ifNotRunning "taffybar-linux-x86_64" $ safeSpawnProg "taffybar"
  ifNotRunning "compton" $ safeSpawn "compton" [ "--backend=glx"
                                               , "--paint-on-overlay"]
  -- TODO: spawn this iff isWork, spawn connman-ui-gtk when on a *home* laptop
  ifNotRunning "nm-applet" $ safeSpawnProg "nm-applet"
  where
    setWallpaper None =
      warn "couldn't set wallpaper!"
    setWallpaper (Fill fp) =
      safeSpawn "feh" ["--no-fehbg", "--bg-fill", fp]
    setWallpaper (Tile fp) =
      safeSpawn "feh" ["--no-fehbg", "--bg-tile", fp]

ifNotRunning :: MonadIO m => String -> IO () -> m ()
ifNotRunning prog hook = io $ void $ forkIO $ do
  notRunning <- isNothing <$> findPid prog
  when notRunning hook

-- | Find the first PID of the first process with given comm, if one exists.
-- We could find all of them by replacing `findM' with `filterM'...
-- TODO: this ignores processes with no cmdline, like kernel threads. We could
--       fallback to comm, but I don't really care for those cases.
-- TODO: handle scripts, where the script name is the second field
-- TODO: seperate this into computing a name => pid map, then consulting it
findPid :: String -> IO (Maybe ProcessID)
findPid comm =
  getDirectoryContents "/proc" >>= findM matchesComm . mapMaybe readMaybe
  where
    matchesComm :: ProcessID -> IO Bool
    matchesComm pid =
      either (const False) ((== comm) . takeFileName . takeWhile (/= '\0')) <$> tryReadLine ("/proc" </> show pid </> "cmdline")
    tryReadLine :: FilePath -> IO (Either () String)
    tryReadLine f =
      tryJust (guard . (\e -> isDoesNotExistError e || isEOFError e)) (readFirstLine f)
    readFirstLine :: FilePath -> IO String
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
  , stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat
  , stringProperty "WM_WINDOW_ROLE" =? "app" --> doFloat
  ] <+> manageDocks


myHandleEventHook = fullscreenEventHook <+> docksEventHook

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

myKeys :: LocalConfig X -> XConfig Layout -> Map (KeyMask, KeySym) (X())
myKeys LocalConfig { warnAction = warn
                   , hostName = host
                   } c =
  mkKeymap c $
    [ ("M-S-<Return>",            safeSpawnProg $ XMonad.terminal c)
    , ("M-p",                     safeSpawnProg "dmenu_run")
--  , ("M-S-p",                   spawn "gmrun")
    , ("M-S-c",                   kill)
    , ("M-<Space>",               sendMessage NextLayout)
    , ("M-S-<Space>",             setLayout $ XMonad.layoutHook c)
    , ("M-n",                     refresh)
    , ("M-<Tab>",                 windows W.focusDown)
    , ("M-j",                     windows W.focusDown)
    , ("M-S-<Tab>",               windows W.focusUp)
    , ("M-k",                     windows W.focusUp)
    , ("M-m",                     windows W.focusMaster)
    , ("M-<Return>",              windows W.swapMaster)
    , ("M-S-j",                   windows W.swapDown)
    , ("M-S-k",                   windows W.swapUp)
    , ("M-h",                     sendMessage Shrink)
    , ("M-l",                     sendMessage Expand)
    , ("M-t",                     withFocused $ windows . W.sink)
    , ("M-,",                     sendMessage (IncMasterN 1))
    , ("M-.",                     sendMessage (IncMasterN (-1)))
    , ("M-S-q",                   io exitSuccess)
    , ("M-q",                     recompile False >>=
                                    (`when` (safeSpawn "xmonad" ["--restart"])))
    , ("M-v",                     windows copyToAll)
    , ("M-S-v",                   killAllOtherCopies)
    , ("M-a",                     safeRunProgInTerm "alsamixer")
    {- Take a screenshot, save as 'screenshot.png' -}
    -- TODO notify-send screenshot saved
    , ("<Print>",                 safeSpawn "import" [ "-window", "root"
                                                     , "screenshot.png" ])
    , ("<XF86Eject>",             safeSpawn "eject" ["-T"])
    , ("<XF86Calculator>",        safeSpawnProg "speedcrunch")
    , ("<XF86Search>",            warn "search")
    , ("<XF86Mail>",              warn "mail")
    , ("<XF86WebCam>",            warn "smile")
    , ("<XF86Eject>",             safeSpawnProg "eject")
    -- TODO >> notify-send `xbacklight`
    , ("<XF86MonBrightnessUp>",   safeSpawn "xbacklight" ["-inc", "10"])
    , ("<XF86MonBrightnessDown>", safeSpawn "xbacklight" ["-dec", "10"])
    ]
    {- Workspace Switching -}
    ++ [ (m ++ k, windows $ f k)
       | k <- XMonad.workspaces c, (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-")]
    ]
    {- Screen Switching -}
    ++ [ (m ++ key, screenWorkspace sc >>= (`whenJust` (windows . f)))
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
    ++ [ ( k, runOrRaise chrome (className =? chrome))
       | k <- ["M-c", "<XF86HomePage>"]
    ]
    {- Screen Locking -}
    ++ [ (k , (when mpd $ doMpd $ MPD.pause True) >> lock >> sleep 4 >> screenOff)
       | k <- ["M-s", "<XF86ScreenSaver>"]
    ]
    ++ [ (k , lock >> sleep 4 >> screenOff)
       | k <- ["M-S-s"]
    ]
    ++ [ (k , lock >> sleep 4 >> (suspend host))
       | k <- ["M-C-S-s", "<XF86Sleep>"]
    ]
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
      asks (terminal . config) >>= (`safeSpawn` (["-e", comm] ++ args))
    safeRunProgInTerm =
      (`safeRunInTerm` [])
    chrome = chromeName host
    mpd = hasMpd host

myConfig = do
  conf <- getConfiguration
  term <- myTerminal
  return $ ewmh $ pagerHints $ defaultConfig
    { normalBorderColor  = myNormalColour
    , focusedBorderColor = myFocusedColour
    , modMask            = myModMask
    , terminal           = term
    , startupHook        = myStartupHook conf
    , manageHook         = myManageHook
    , handleEventHook    = myHandleEventHook
    , layoutHook         = myLayout
    , workspaces         = myWorkspaces
    , keys               = myKeys conf
    }

main :: IO ()
main = myConfig >>= xmonad

