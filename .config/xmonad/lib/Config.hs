{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config where

import XMonad

import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.Navigation2D
import XMonad.Actions.Submap

import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)

import XMonad.Layout.IM
import XMonad.Layout.LayoutModifier (ModifiedLayout(..))
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import qualified XMonad.Layout.BoringWindows as B

import XMonad.Util.NamedScratchpad
import System.Exit
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio ((%))

import qualified DBus as D
import qualified DBus.Client as D

import qualified Codec.Binary.UTF8.String as UTF8

-- solarized colors
bg     = "#002b36"
gray   = "#073642"
fg     = "#839496"
bg1    = "#93a1a1"

red    = "#dc322f"
green  = "#859900"
yellow = "#b58900"
blue   = "#268bd2"
purple = "#6c71c4"
aqua   = "#2aa198"
orange = "#cb4b16"

delta :: Rational
delta = 3 / 100

-- Tailor IM window behavior
myIM :: LayoutClass l a => l a -> ModifiedLayout AddRoster l a
myIM = withIM (1 % 4) (ClassName "wire")

------------------------------------------------------------------------
-- Workspaces
--

myWorkspaces = ["main","text","ide","web","mail","chat","media","read","gimp"]

-- Layouts
myLayouts = renamed [CutWordsLeft 1] .
    avoidStruts . minimize .  B.boringWindows $
    smartBorders
        ( aTiled
        ||| aFullscreen
        ||| aTabbed
        )
  where
    aTabbed = renamed [Replace "Tab"] $ myIM $ tabbedBottom shrinkText defTabbed
    aFullscreen = renamed [Replace "Full"] $ noBorders Full
    aTiled = renamed [Replace "Main"] $ myIM $ Tall 1 (3 / 100) (1 / 2)
    defTabbed = def
        { activeColor = bg
        , urgentColor = red
        , inactiveColor = bg
        , activeBorderColor = bg
        , inactiveBorderColor = bg
        , urgentBorderColor = red
        , inactiveTextColor = gray -- Gray color on dark gray background
        , activeTextColor = green
        , urgentTextColor = "#ffffff"
        , fontName = "xft:Liberation Sans:size=10" }

switchWorkspaceToWindow :: Window -> X ()
switchWorkspaceToWindow w = windows $ do
  tag <- W.currentTag
  W.focusWindow w . W.greedyView tag . W.focusWindow w

-- Default Managers (credit to  jsjolund @ https://gist.github.com/jsjolund/94f6821b248ff79586ba )
--
-- Match a string against anyone of a window's class, title, name, or role
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, cName, role]

-- Match against @WM_NAME@.
cName :: Query String
cName = stringProperty "WM_CLASS"
-- Match against @WM_WINDOW_ROLE@.

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

{-|
# Script to easily find WM_CLASS for adding applications to the list
#! /bin/sh
exec xprop -notype \
  -f WM_NAME        8s ':\n  title =\? $0\n' \
  -f WM_CLASS       8s ':\n  appName =\? $0\n  className =\? $1\n' \
  -f WM_WINDOW_ROLE 8s ':\n  stringProperty "WM_WINDOW_ROLE" =\? $0\n' \
  WM_NAME WM_CLASS WM_WINDOW_ROLE \
  ${1+"$@"}
-}
myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions]
    where myActions =
            [ ("Xfrun4"                         , doFloat)
            , ("Xfce4-notifyd"                  , doIgnore)
            , ("MPlayer"                        , doFloat)
            , ("mpv"                            , doFloat)
            , ("Oracle VM VirtualBox Manager"   , doShift "8")
            , ("VirtualBox"                     , doShift "8")
            , ("animation-SpriteTestWindow"     , doFloat)
            , ("gimp-image-window"              , (ask >>= doF . W.sink))
            , ("gimp-toolbox"                   , (ask >>= doF . W.sink))
            , ("gimp-dock"                      , (ask >>= doF . W.sink))
            , ("gimp-image-new"                 , doFloat)
            , ("gimp-toolbox-color-dialog"      , doFloat)
            , ("gimp-layer-new"                 , doFloat)
            , ("gimp-vectors-edit"              , doFloat)
            , ("gimp-levels-tool"               , doFloat)
            , ("preferences"                    , doFloat)
            , ("gimp-keyboard-shortcuts-dialog" , doFloat)
            , ("gimp-modules"                   , doFloat)
            , ("unit-editor"                    , doFloat)
            , ("screenshot"                     , doFloat)
            , ("gimp-message-dialog"            , doFloat)
            , ("gimp-tip-of-the-day"            , doFloat)
            , ("plugin-browser"                 , doFloat)
            , ("procedure-browser"              , doFloat)
            , ("gimp-display-filters"           , doFloat)
            , ("gimp-color-selector"            , doFloat)
            , ("gimp-file-open-location"        , doFloat)
            , ("gimp-color-balance-tool"        , doFloat)
            , ("gimp-hue-saturation-tool"       , doFloat)
            , ("gimp-colorize-tool"             , doFloat)
            , ("gimp-brightness-contrast-tool"  , doFloat)
            , ("gimp-threshold-tool"            , doFloat)
            , ("gimp-curves-tool"               , doFloat)
            , ("gimp-posterize-tool"            , doFloat)
            , ("gimp-desaturate-tool"           , doFloat)
            , ("gimp-scale-tool"                , doFloat)
            , ("gimp-shear-tool"                , doFloat)
            , ("gimp-perspective-tool"          , doFloat)
            , ("gimp-rotate-tool"               , doFloat)
            , ("gimp-open-location"             , doFloat)
            , ("gimp-file-open"                 , doFloat)
            , ("animation-playbac"              , doFloat)
            , ("gimp-file-save"                 , doFloat)
            , ("file-jpeg"                      , doFloat)
            , ("Pavucontrol"                    , doFloat)
            , ("Slack"                          , doFloat)
            , ("xarchive"                       , doFloat)
            , ("xGtkFileChooserDialog"          , doFloat)
            , ("telegram-desktop"               , doFloat)
            , ("file_properties"                , doFloat)
            ]
-- Helpers --
-- avoidMaster:  Avoid the master window, but otherwise manage new windows normally
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
  W.Stack t [] (r:rs) -> W.Stack t [r] rs
  otherwise           -> c

-- Keyboard
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
  -----------------------------
  -- CUSTOM KEY BINDINGS
  -- --------------------------
  -- Launching and Kill Programs
  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [((modMask,                  xK_Return   ), spawn "terminix")

      -- Lock the screen using a screensaver
  , ((modMask .|. shiftMask,   xK_l        ), spawn "xscreensaver-command -lock")

      -- Launch launcher
  , ((modMask,                 xK_x        ), spawn "$HOME/.config/dmenu/dmenu-bind.sh")

      -- Startup sublime for xmonad configuration
  , ((modMask,                 xK_e        ), spawn "subl $HOME/.config/xmonad/xmonad.hs")

      -- Take a snapshot by selecting a region
  , ((modMask .|. controlMask, xK_5        ), spawn "scrot -s")

      -- Launch firefox
  , ((modMask,                 xK_w        ), spawn "firefox")

      -- Launch nautilus
  , ((modMask,                 xK_f        ), spawn "nautilus")

      -- Launch audio controls
  , ((modMask,                 xK_a        ), spawn "pavucontrol")
  -- Swapping
  -- Swap the focused window and the master window
  , ((modMask .|. shiftMask,   xK_m        ), windows W.swapMaster)

  -- Swap the focused window with the next window
  , ((modMask .|. shiftMask,   xK_j        ), windows W.swapDown  )

  -- Swap the focused window with the previous window
  , ((modMask .|. shiftMask,   xK_k        ), windows W.swapUp    )

  -- Resizing
  -- Shrink the master area
  , ((modMask,                 xK_h        ), sendMessage Shrink)

  -- Expand the master area
  , ((modMask,                 xK_l        ), sendMessage Expand)

  -- toggle the status bar gap
  , ((modMask,                 xK_b        ), sendMessage ToggleStruts)
  -- 2D navigation
  , ((modMask .|. shiftMask, xK_l), screenGo R True)
  , ((modMask .|. shiftMask, xK_h), screenGo L True)
  , ((modMask .|. controlMask, xK_l), screenSwap R True)
  , ((modMask .|. controlMask, xK_h), screenSwap L True)

  -- Float handling (snapping to edges)
  , ((modMask, xK_Right), withFocused $ snapMove R Nothing)
  , ((modMask, xK_Left), withFocused $ snapMove L Nothing)
  , ((modMask, xK_Up), withFocused $ snapMove U Nothing)
  , ((modMask, xK_Down), withFocused $ snapMove D Nothing)

  , ((modMask .|. shiftMask, xK_Right), withFocused $ keysResizeWindow (20, 0) (0, 0))
  , ((modMask .|. shiftMask, xK_Left), withFocused $ keysResizeWindow (-20, 0) (0, 0))
  , ((modMask .|. shiftMask, xK_Up), withFocused $ keysResizeWindow (0, -20) (0, 0))
  , ((modMask .|. shiftMask, xK_Down), withFocused $ keysResizeWindow (0, 20) (0, 0))

    -- Minimize stuff
  , ((modMask, xK_v), withFocused minimizeWindow)
  , ((modMask .|. shiftMask, xK_v), sendMessage RestoreNextMinimizedWin)

  , ((modMask, xK_g), placeFocused $ smart (0.5, 0.5))

  -- Inc or Dec # of windows in master area
  -- Increment the number of windows in the master area
  , ((modMask,                 xK_comma    ), sendMessage (IncMasterN 1))
  -- Deincrement the number of windows in the master area
  , ((modMask,                 xK_period   ), sendMessage (IncMasterN (-1)))

  -- Restart xmonad
  , ((modMask .|. shiftMask,   xK_r        ), broadcastMessage ReleaseResources >> restart "xmonad" True)
  -- Quit XMonad (Default)
  , ((modMask .|. shiftMask,   xK_q        ), io exitSuccess)
 ]
 ++
-- mod-[1..9], Switch to workspace N
-- mod-shift-[1..9], Move client to workspace N
--
  [ ((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Width of the window border in pixels.
myBorderWidth = 2

-- Loghook
myDbusHook :: D.Client -> PP
myDbusHook dbus = def
  { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{B" ++ bg ++ "} ") " %{B-}"
    , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
    , ppUrgent  = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden  = wrap " " " "
    , ppWsSep   = ""
    , ppSep     = " : "
    , ppTitle = shorten 40
    }


-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

-- ManageHook --
pbManageHook :: ManageHook
pbManageHook = composeAll $ concat
    [ [ manageDocks ]
    , [ isDialog --> doCenterFloat ]
    , [ isFullscreen --> doFullFloat ]
    , [ fmap not isDialog --> doF avoidMaster ]
    ]

myConfig = def
  { terminal = "terminix"
  , borderWidth = myBorderWidth
  , modMask = mod4Mask
  , workspaces = myWorkspaces
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , keys = myKeys
  , manageHook = placeHook (smart (0.5, 0.5))
    <+> pbManageHook
    <+> myManageHook
  , handleEventHook = docksEventHook <+> minimizeEventHook <+> fullscreenEventHook
  , layoutHook = myLayouts
  }