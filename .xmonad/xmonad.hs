{-# OPTIONS_GHC -O2 -tmpdir /tmp -optc -O2 -optl -O #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import           Network.MPD (withMPD, pause, previous, next, stop)
import           Network.MPD.Commands.Extensions (toggle)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Info
import           System.Posix.Unistd hiding (sleep)
import           System.Process
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
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

-- TODO bring urxvtcd functionality in here?

{-
http://hackage.haskell.org/packages/archive/xmonad-contrib/0.8/doc/html/XMonad-Hooks-DynamicLog.html
TODO: xmobar options on the command line? no?
 -}

myBar = "xmobar"

myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

--toggleStrutsKey XConfig { XMonad.modMask = mask } = (mask, xK_b)
--toggleStrutsKey conf = ( XMonad.modMask conf, xK_b )
toggleStrutsKey = flip ( (,) . XMonad.modMask ) xK_b

myNormalColour = "#202020"

myFocusedColour = "#ff0000"

myModMask = mod4Mask

{- If urxvt is not installed, use xterm. If it is, prefer urxvtcd. -}
myTerminal = io $
  findExecutable "urxvt"
  >>= maybe (return "xterm") (\a -> fromMaybe a <$> findExecutable "urxvtcd")

getHomes :: MonadIO m => m (FilePath, FilePath)
getHomes = io $ do
  h <- getHomeDirectory
  return (h, myWP h)
  where
    myWP = (</> "Dropbox/wallpaper/wallpaper-2473668.jpg")

getConfiguration :: (MonadIO m, Integral n) => m (Bool, Bool, n)
getConfiguration = io $ do
  h <- nodeName <$> getSystemID
  return (hasMpd h, hasWifi h, trayerWidth h)
  where
    hasMpd      = (== "vera")
    hasWifi     = flip elem ["gladys", "winona"]
    --trayerWidth = (\x -> (x `div` 320) * 16)
    trayerWidth = (* 16) . (flip div 320) -- round 5% down to a multiple of 16
                . (fromMaybe 1280) -- sane default for labs
                . (flip lookup [ ("winona", 1024)
                               , ("gladys", 1440)
                               , ("vera",   1920)
                               ])

myStartupHook = do
  safeSpawn "killall" ["trayer"]
  setWMName "LG3D" --fuck java
  safeSpawn "setxkbmap" ["-layout", "gb"]
  safeSpawn "xsetroot" ["-cursor_name", "left_ptr"]
  (home, wp) <- getHomes
  (_, _, tw) <- getConfiguration
  safeSpawn "trayer" [ "--edge", "top"
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

myManageHook = composeAll
  [ className =? "MPlayer"        --> doFloat
  , resource  =? "desktop_window" --> doIgnore
  , resource  =? "kdesktop"       --> doIgnore
--, isFullscreen                  --> (doF W.focusDown <+> doFullFloat)
  , isFullscreen                  --> doFullFloat
  ]

myHandleEventHook = mempty
--myHandleEventHook = fullscreenEventHook

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
    imLayout   = doubleIM "IM"   Grid 0.15 (Role "buddy_list")
                                      0.2  (ClassName "Skype")

{- Workspace Identifiers. Must correspond to keys in mkKeyMap format. -}
--myWorkspaces = ["`"] ++ map show [1..9] ++ ["0", "-", "="]
myWorkspaces = map (:"") "`1234567890-="

myKeys = do
  (mpd, wifi, _) <- getConfiguration
  lock <- safeSpawnProg . fromMaybe "xlock" <$> findExecutable "slock"
  return $ \c -> mkKeymap c $
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
    , ("M-q",              recompile False >>= (flip when)
                             (safeSpawn "xmonad" ["--restart"]))
    , ("M-a",              safeRunInTerm "alsamixer" [])
    {- Power off screen -}
    , ("M-S-s",            screenOff)
    , ("M-s",              screenOff >> (when mpd $ mpd_ $ pause True))
    {- Take a screenshot, save as 'screenshot.png' -}
    , ("M-<Print>",        safeSpawn "import" [ "-window", "root"
                                              , "screenshot.png" ])
    , ("<XF86Eject>",      safeSpawn "eject" ["-T"])
    , ("<XF86Sleep>",      lock >> sleep 1
                           >> safeSpawn "sudo" ["pm-hibernate"])
    , ("<XF86Calculator>", safeSpawnProg "speedcrunch")
    , ("<XF86Search>",     xmessage "search")
    , ("<XF86Mail>",       xmessage "mail")
    , ("<XF86WebCam>",     xmessage "smile")
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
    {- Spawning Firefox -}
    ++ [ ( k, runOrRaise "firefox" (className =? "Firefox"))
       | k <- ["M-f", "<XF86HomePage>"]
    ]
    {- Screen Locking -}
    ++ [ (k , lock >> screenOff)
       | k <- ["M-x", "<XF86ScreenSaver>"]
    ]
    {- WiFi manager -}
    ++ ( guard wifi >>
      [ ("M-S-n", safeRunProgInTerm "wicd-curses") ]
    )
    {- MPC keys, media player UI -}
    ++ ( guard mpd >>
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
    safeRunInTerm c o =
      asks (terminal . config) >>= flip safeSpawn (["-e", c] ++ o)
    safeRunProgInTerm c =
      safeRunInTerm c []
    sleep =
      io . threadDelay . seconds
    screenOff =
      sleep 1 >> safeSpawn "xset" ["dpms", "force", "off"]
    mpd_ =
      void . io . withMPD
    xmessage m =
      void $ safeSpawn "xmessage" [m]

myConfig = do
  t <- myTerminal
  k <- myKeys
  return defaultConfig
    { normalBorderColor  = myNormalColour
    , focusedBorderColor = myFocusedColour
    , modMask            = myModMask
    , terminal           = t
    , startupHook        = myStartupHook
    , manageHook         = myManageHook
    , handleEventHook    = myHandleEventHook
    , logHook            = myLogHook
    , layoutHook         = myLayout
    , workspaces         = myWorkspaces
    , keys               = k
    }

main = myConfig >>= statusBar myBar myPP toggleStrutsKey >>= xmonad
