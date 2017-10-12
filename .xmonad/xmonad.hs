{-# OPTIONS_GHC -O2 -optc -O2 #-}
{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables #-}

import           Control.Applicative ((<$>),pure)
import           Control.Concurrent (forkIO,threadDelay)
import           Control.Exception (tryJust)
import           Control.Monad (filterM,foldM,guard,void,when)
import           Data.Map (Map)
import           Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import           Data.List.Split (splitOn,split,dropFinalBlank,dropDelims,onSublist)
import           Data.Set as Set (Set, empty, insert, member)
import           Daw.Hosts
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
import           XMonad.Actions.CopyWindow (copyToAll,copyWindow,killAllOtherCopies)
import           XMonad.Actions.WindowGo (runOrRaise)
import           XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docksEventHook, ToggleStruts(..))
import           XMonad.Hooks.ManageHelpers (doFullFloat,isFullscreen)
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders (smartBorders)
import           XMonad.Layout.Renamed
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (mkKeymap)
import           XMonad.Util.Run (safeSpawn,safeSpawnProg,seconds,unsafeSpawn)

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
      case wps of
        [] -> return None
        _  -> do
          candidates <-
            concat <$>
               mapM
                (\(m, dir) ->
                  getDirectoryContents dir
                    >>= filterM doesFileExist . map (dir </>)
                    >>= return . map m)
                wps
          index <- randomRIO (0, length candidates - 1)
          return $ candidates !! index

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
  progs <- getRunningProcesses
  let ifNotRunning prog hook = io $ void $ when (not $ Set.member prog progs) hook
  safeSpawn "xscreensaver" ["-no-splash"]
  ifNotRunning "urxvtd" $ safeSpawn "urxvtd" ["-q", "-o"]
  ifNotRunning "taffybar-linux-x86_64" $ safeSpawn "stack" ["exec", "taffybar"]
  ifNotRunning "compton" $ safeSpawn "compton" [ "--backend=glx"
                                               , "--paint-on-overlay"]
  when (isWork host) $ ifNotRunning "nm-applet" $ safeSpawnProg "nm-applet"
  when (isLaptop host) $ ifNotRunning "connman-ui-gtk" $ safeSpawnProg "connman-ui-gtk"
  where
    setWallpaper None =
      warn "couldn't set wallpaper!"
    setWallpaper (Fill fp) =
      safeSpawn "feh" ["--no-fehbg", "--bg-fill", fp]
    setWallpaper (Tile fp) =
      safeSpawn "feh" ["--no-fehbg", "--bg-tile", fp]

-- | Get a set containing the file name and first argument (if it has one, in case it's a script)
-- | of every running process.
-- Pretty hacky.
getRunningProcesses :: MonadIO m => m (Set String)
getRunningProcesses = io $ do
  -- get all of the files/dirs in /proc
  dirs <- getDirectoryContents "/proc"
  -- read those that we can as numbers
  let pids :: [ProcessID] = mapMaybe readMaybe dirs
  foldM
    (\set pid ->
      -- for each process, try and get the command line
      tryReadLine ("/proc" </> show pid </> "cmdline") >>= \errorOrCmdline ->
        return $ either
          -- if reading failed (e.g. the process is already dead), leave the set as-is
          (const set)
          -- else take the set and add to it the filename components of up to the first 2 args
          (\str -> foldl (flip Set.insert) set (map takeFileName $ take 2 $ splitz "\0" str))
          errorOrCmdline)
    Set.empty
    pids
  where
    splitz = split . dropFinalBlank . dropDelims . onSublist
    tryReadLine :: FilePath -> IO (Either () String)
    tryReadLine f =
      tryJust (guard . (\e -> isDoesNotExistError e || isEOFError e)) (readFirstLine f)
    readFirstLine :: FilePath -> IO String
    readFirstLine f = do
      h <- openFile f ReadMode
      str <- hGetLine h
      hClose h
      return str

myManageHook = composeAll
  [ className =? "MPlayer"                      --> doFloat
  , className =? "Zenity"                       --> doFloat
  , className =? "Xmessage"                     --> doFloat
  , resource  =? "desktop_window"               --> doIgnore
  , resource  =? "kdesktop"                     --> doIgnore
  , isFullscreen                                --> (doF W.focusDown <+> doFullFloat)
  , stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat <+> doCopyToAll
  ] <+> manageDocks
  where
    doCopyToAll = ask >>= doF . \w -> (\ws -> foldr($) ws (map (copyWindow w) myWorkspaces))

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
    , ("M-q",                     unsafeSpawn "stack exec -- xmonad --recompile; xmonad --restart")
    , ("M-b",                     sendMessage ToggleStruts)
    , ("M-v",                     windows copyToAll)
    , ("M-S-v",                   killAllOtherCopies)
    , ("M-a",                     safeRunProgInTerm "alsamixer")
    {- Take a screenshot, save as 'screenshot.png' -}
    -- TODO notify-send screenshot saved
    -- TODO include the timestamp?
    , ("<Print>",                 safeSpawn "import" [ "-window", "root"
                                                     , "/tmp/screenshot.png" ])
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
    ++ [ (k , (safeSpawn "mpc" ["pause"]) >> lock >> sleep 4 >> screenOff)
       | k <- ["M-s", "<XF86ScreenSaver>"]
    ]
    ++ [ (k , lock >> sleep 4 >> screenOff)
       | k <- ["M-S-s"]
    ]
    ++ [ (k , lock >> sleep 4 >> (suspend host))
       | k <- ["M-C-S-s", "<XF86Sleep>"]
    ]
    {- MPC keys, media player UI -}
    ++ [ (k, safeSpawn "mpc" [comm])
       | (k, comm) <- [ ("<XF86AudioPlay>", "toggle")
                      , ("<XF86AudioPrev>", "prev")
                      , ("<XF86AudioNext>", "next")
                      , ("<XF86AudioStop>", "stop")
                      ]
       ] ++
       [ (k, safeRunProgInTerm "ncmpcpp")
       | k <- ["M-S-m", "<XF86AudioMedia>"]
       ]
  where
    safeRunInTerm :: String -> [String] -> X()
    safeRunInTerm comm args =
      asks (terminal . config) >>= (`safeSpawn` (["-e", comm] ++ args))
    safeRunProgInTerm =
      (`safeRunInTerm` [])
    chrome = chromeName host

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

