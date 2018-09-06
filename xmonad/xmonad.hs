import qualified Data.Map as M
import           Data.List (intercalate)
import           Data.List.Ordered (isSorted)
import           Data.Maybe (isJust)
import qualified Text.Regex as R
import           GHC.IO.Handle.Types          (Handle)
import           XMonad
import           XMonad.Actions.Search (promptSearch, duckduckgo)
import qualified XMonad.StackSet as W
import           XMonad.Hooks.DynamicLog (PP(..), defaultPP, dynamicLogWithPP, dzenColor, pad, dzenEscape)
import           XMonad.Hooks.ManageDocks (avoidStruts, docks, ToggleStruts(..))
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import           XMonad.Util.Run (spawnPipe, hPutStrLn)
import           XMonad.Layout.PerWorkspace (onWorkspace)
import           XMonad.Layout.Named (named)
import           XMonad.Layout.GridVariants (TallGrid (..))
import           XMonad.Layout.Accordion (Accordion(..))
import           XMonad.Layout.NoBorders (noBorders, smartBorders)
import           XMonad.Actions.CycleWS (shiftNextScreen)
import           XMonad.Actions.CycleWindows (rotFocusedUp)
import           XMonad.Hooks.ScreenCorners (ScreenCorner(..), addScreenCorner, screenCornerLayoutHook, screenCornerEventHook)
import           XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import           XMonad.Prompt.Pass (passPrompt)
import           XMonad.Prompt.Workspace (workspacePrompt)
import           XMonad.Prompt.Window (windowPrompt, allWindows, WindowPrompt(..))
import           XMonad.Prompt.XMonad (xmonadPrompt)
import qualified XMonad.Prompt as Prompt

myXmonadBar = "dzen2 -dock -fn " <> myFont <> " -x '0' -y '-1' -h '24' -w '745' -ta 'l' -fg '#FFFFFF' -bg '#000000'"
myStatusBar = "conky -c ~/.conkyrc | dzen2 -dock -fn " <> myFont <> " -y '-1' -w '850' -x -850 -ta 'r' -fg '#FFFFFF' -bg '#000000'"

main = do
  dzenLeftBar <- spawnPipe myXmonadBar
  dzenRightBar <- spawnPipe myStatusBar
  xmonad $ docks $ myConfig dzenLeftBar

myConfig leftBar =
  defaultConfig { terminal           = "kitty"
                , modMask            = mod4Mask
                , borderWidth        = 1
                , focusedBorderColor = "red"
                , focusFollowsMouse  = False
                , startupHook        = myStartup
                , layoutHook         = myLayoutHook
                , workspaces         = myWorkspaces
                , keys               = myKeys
                , handleEventHook    = myEventHook
                , logHook            = myLogHook leftBar >> fadeInactiveLogHook 0xdddddddd
                , manageHook         = myManageHook
                }

myRestartCmd = "xmonad --recompile; killall dzen2; xmonad --restart; notify-send -t 500 'XMonad' '~/.xmonad/xmonad.hs reloaded'"

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["web", "term", "editor", "scratch", "steam", "signal", "work", "chat", "music"]

myKeys conf = M.union (M.fromList (newKeys conf)) (keys def conf)

newKeys conf@(XConfig {XMonad.modMask = modMask}) =
  [ ((modMask, xK_p), passPrompt xPrompt)
  , ((modMask, xK_w), workspacePrompt xPrompt (windows . W.shift))
  , ((modMask .|. shiftMask, xK_w), windowPrompt xPrompt Goto allWindows)
  , ((modMask, xK_d), spawn "/home/jbrechtel/bin/mydmenu")
  , ((modMask .|. shiftMask, xK_j), spawn "/home/jbrechtel/bin/audio jabra")
  , ((modMask .|. shiftMask, xK_l), spawn "/home/jbrechtel/bin/audio logitech")
  , ((modMask .|. controlMask, xK_j), shiftNextScreen)
  , ((modMask, xK_Return ), windows $ W.focusMaster . W.swapUp)
  , ((modMask .|. shiftMask, xK_r ), spawn myRestartCmd)
  , ((modMask .|. shiftMask, xK_e ), refresh)
  , ((modMask .|. shiftMask, xK_q ), kill)
  , ((modMask .|. shiftMask, xK_x ), xmonadPrompt xPrompt)
  , ((modMask, xK_s ), promptSearch xPrompt duckduckgo)
  , ((modMask, xK_f), sendMessage ToggleStruts)
  ]

myLayoutHook = avoidStruts $ screenCornerLayoutHook $ smartBorders
  $ onWorkspace "web" (noBorders Full)
  $ onWorkspace "term" (Tall 1 (3/100) (2/3))
  $ onWorkspace "editor" (Tall 1 (3/100) (3/4))
  $ (named "default" (Tall 1 (3/100) (5/7))) ||| Accordion

myStartup :: X ()
myStartup = do
  spawn "dunst"
  spawn "gnome-keyring-daemon --replace --daemonize --components=secrets,ssh,pcks11"
  spawn "xrandr --output DP-2 --rotate left --output DP-4 --primary --right-of DP-2 --auto"
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
    , [isFullscreen --> makeFullscreen]
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
                       , Prompt.autoComplete = Just 5000
                       , Prompt.bgHLight = "black"
                       , Prompt.bgColor = "black"
                       , Prompt.fgHLight = "white"
                       , Prompt.fgColor = "#33ff00"
                       }

fuzzyPredicate :: String -> String -> Bool
fuzzyPredicate compl cand =
  let singleton a = [a]
      regex = R.mkRegex $ intercalate ".*" $ singleton <$> compl
   in isJust $ R.matchRegex regex cand
