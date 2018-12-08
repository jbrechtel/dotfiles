import qualified Data.Map as M
import           Data.List (intercalate)
import           Data.List.Ordered (isSorted)
import           Data.Maybe (isJust)
import           System.Posix.Unistd          (getSystemID, nodeName)
import qualified Text.Regex as R
import           GHC.IO.Handle.Types          (Handle)
import           XMonad
import           XMonad.Actions.Search (promptSearch, duckduckgo)
import           XMonad.Actions.OnScreen (viewOnScreen, toggleOnScreen, Focus(..))
import           XMonad.Actions.CycleWS (toggleWS, swapNextScreen)
import qualified XMonad.StackSet as W
import           XMonad.Hooks.DynamicLog (PP(..), defaultPP, dynamicLogWithPP, dzenColor, pad, dzenEscape)
import           XMonad.Hooks.ManageDocks (avoidStruts, docks, ToggleStruts(..))
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import           XMonad.Util.Run (spawnPipe, hPutStrLn)
import           XMonad.Layout.PerWorkspace (onWorkspace)
import           XMonad.Layout.PerScreen (ifWider)
import           XMonad.Layout.Named (named)
import           XMonad.Layout.GridVariants (TallGrid (..))
import           XMonad.Layout.Accordion (Accordion(..))
import           XMonad.Layout.NoBorders (noBorders, withBorder)
import           XMonad.Layout.SimpleFloat
import           XMonad.Actions.CycleWindows (rotFocusedUp)
import           XMonad.Hooks.ScreenCorners (ScreenCorner(..), addScreenCorner, screenCornerLayoutHook, screenCornerEventHook)
import           XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import           XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Prompt.Pass (passPrompt)
import           XMonad.Prompt.Workspace (workspacePrompt)
import           XMonad.Prompt.Window (windowPrompt, allWindows, WindowPrompt(..))
import           XMonad.Prompt.XMonad (xmonadPrompt)
import qualified XMonad.Prompt as Prompt
import           System.Exit (exitWith, ExitCode(ExitSuccess))

myXmonadBar Laptop = "dzen2 -xs 1 -dock -fn " <> myFont <> " -x '0' -y '-1' -w '920' -ta 'l' -fg '#FFFFFF' -bg '#000000'"
myXmonadBar Desktop = "dzen2 -xs 1 -dock -fn " <> myFont <> " -x '0' -y '-1' -w '820' -ta 'l' -fg '#FFFFFF' -bg '#000000'"

myStatusBar Laptop = "conky -c ~/.conkyrc | dzen2 -xs 1 -dock -fn " <> myFont <> " -y '-1' -w '1000' -x -1000 -ta 'r' -fg '#FFFFFF' -bg '#000000'"
myStatusBar Desktop = "conky -c ~/.conkyrc | dzen2 -xs 1 -dock -fn " <> myFont <> " -y '-1' -w '1740' -x -1740 -ta 'r' -fg '#FFFFFF' -bg '#000000'"

data MachineType
  = Desktop
  | Laptop

machineType :: String -> MachineType
machineType "dev" = Desktop
machineType "cecil" = Laptop
machineType "bojack" = Laptop
machineType _ = Desktop

main = do
  mType <- fmap (machineType . nodeName) getSystemID
  dzenLeftBar <- spawnPipe $ myXmonadBar mType
  dzenRightBar <- spawnPipe $ myStatusBar mType
  xmonad $ docks $ ewmh $ myConfig mType dzenLeftBar

myConfig machineType leftBar =
  defaultConfig { terminal           = "alacritty"
                , modMask            = mod4Mask
                , borderWidth        = 1
                , focusedBorderColor = "#f8f9d1"
                , normalBorderColor  = "#333333"
                , focusFollowsMouse  = False
                , mouseBindings      = (const M.empty)
                , startupHook        = (myStartup machineType)
                , layoutHook         = myLayoutHook
                , workspaces         = myWorkspaces
                , keys               = myKeys
                , handleEventHook    = myEventHook
                , logHook            = myLogHook leftBar >> workspaceHistoryHook >> fadeInactiveLogHook 0xf0000000
                , manageHook         = myManageHook
                }

myRestartCmd = "xmonad --recompile; killall dzen2; xmonad --restart; notify-send -t 500 'XMonad' '~/.xmonad/xmonad.hs reloaded'"

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["web", "term", "editor", "scratch", "steam", "signal", "work", "chat", "music"]

myKeys conf = M.union (M.fromList (newKeys conf)) (keys def conf)

newKeys conf@(XConfig {XMonad.modMask = modMask}) =
  [ ((modMask, xK_p), passPrompt xPrompt)
--  , ((modMask, xK_w), toggleWS)
  , ((modMask, xK_w), windows $ toggleOnScreen 0 "web")
  , ((modMask .|. controlMask, xK_w), windows $ toggleOnScreen 1 "web")
  , ((modMask, xK_d), spawn "/home/jbrechtel/bin/mydmenu")
  , ((modMask .|. shiftMask, xK_j), spawn "/home/jbrechtel/bin/audio jabra")
  , ((modMask .|. shiftMask, xK_l), spawn "/home/jbrechtel/bin/audio logitech")
--  , ((modMask .|. controlMask, xK_j), swapTo Next)
  , ((modMask, xK_Return ), windows $ W.focusMaster . W.swapUp)
  , ((modMask .|. shiftMask, xK_r ), spawn myRestartCmd)
  , ((modMask .|. shiftMask, xK_e ), refresh)
  , ((modMask .|. shiftMask, xK_q ), kill)
  , ((modMask .|. shiftMask .|. controlMask, xK_q ), io (exitWith ExitSuccess))
  , ((modMask .|. shiftMask, xK_x ), xmonadPrompt xPrompt)
  , ((modMask, xK_x ), swapNextScreen)
  , ((modMask, xK_s ), promptSearch xPrompt duckduckgo)
  , ((modMask, xK_f), sendMessage ToggleStruts)
  , ((modMask .|. shiftMask, xK_t ), withFocused $ windows . W.sink)
--  , ((modMask .|. controlMask, xK_1), setFocus $ FocusTag "web")
  ] ++ (perScreenKeys conf modMask)

perScreenKeys conf modMask =
   [ ((m .|. modMask, k), windows (f i))
      | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
      , (f, m) <- [ (viewOnScreen 0, 0)
                  , (viewOnScreen 1, controlMask)
                  , (W.greedyView, controlMask .|. shiftMask) ]
    ]

myLayoutHook = avoidStruts $ screenCornerLayoutHook
  $ onWorkspace "web" (noBorders Full)
  $ onWorkspace "term" normalGrid
  $ onWorkspace "editor" normalGrid
  $ onWorkspace "music" (Tall 1 (3/100) (3/4))
--  $ onWorkspace "steam" (fullscreenFloat $ fullscreenFull $ Full)
  $ normalGrid

  where
    normalGrid = ifWider 1280 wideMonitorGrid tallMonitorGrid
    wideMonitorGrid = Tall 1 (3/100) (1/2) ||| Full
    tallMonitorGrid = (Mirror (Tall 1 (3/100) (1/2)))

myStartup :: MachineType -> X ()
myStartup machineType = do
  spawn "dunst"
  spawn "gnome-keyring-daemon --replace --daemonize --components=secrets,ssh,pcks11"
  spawn "xrandr --output DP-4 --rotate left --output DP-2 --primary --right-of DP-4 --auto"
  spawn "feh --randomize --bg-fill ~/.wallpapers/*"

  case machineType of
    Laptop -> spawn "/home/jbrechtel/bin/start_compton"
    Desktop -> pure ()

  addScreenCorner SCLowerRight $ spawn "sflock"

myEventHook e = do
  screenCornerEventHook e
  handleEventHook def e

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ebac54" "#000000" . pad
      , ppVisible           =   dzenColor "white" "#000000" . pad
      , ppHidden            =   dzenColor "white" "#000000" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#000000" . pad
      , ppUrgent            =   dzenColor "#ff0000" "#000000" . pad
      , ppWsSep             =   ""
      , ppSep               =   ""
      , ppLayout            =   const ""
      , ppTitle             =   const ""
      , ppOutput            =   hPutStrLn h
    }

-- Move some programs to certain workspaces and float some too
myManageHook = composeAll . concat $
    [ [className =? x --> doF (W.shift "web") | x <- myWebShift]
    , [className =? x --> doF (W.shift "chat") | x <- myImShift]
    , [className =? x --> doF (W.shift "editor") | x <- myEditorShift]
    , [className =? x --> doF (W.shift "music") | x <- myMediaShift]
    , [className =? x --> doF (W.shift "work") | x <- myWorkShift]
    , [className =? x --> doFloat | x <- myFloats]
    ]

  where
    myWebShift = ["Firefox"]
    myImShift = ["Gajim"]
    myWorkShift = ["Slack", "Thunderbird"]
    myEditorShift = ["Emacs", "nvim-gtk"]
    myReadShift = ["Calibre","calibre"]
    myMediaShift = ["Spotify","spotify"]
    myFloats = ["Gimp"]

-- makeFullscreen :: X ()
makeFullscreen = do
  doFullFloat

myFont :: String
myFont = "xft:AwesomeFont:size=18:antialias=true "

xPrompt :: Prompt.XPConfig
xPrompt =
  Prompt.amberXPConfig { Prompt.font = myFont
                       , Prompt.height = 32
                       , Prompt.maxComplRows = Just 5
                       , Prompt.position = Prompt.Top
                       , Prompt.searchPredicate = fuzzyPredicate
                       , Prompt.bgHLight = "black"
                       , Prompt.bgColor = "black"
                       , Prompt.fgHLight = "white"
                       , Prompt.fgColor = "#33ff00"
                       }

fuzzyPredicate :: String -> String -> Bool
fuzzyPredicate compl cand =
  let singleton a = [a]
      regex = R.mkRegex $ ".*" <> compl <> ".*"
   in isJust $ R.matchRegex regex cand
