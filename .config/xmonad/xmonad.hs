import System.IO
import System.Exit
import XMonad

import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.Place
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Minimize
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- Terminal

myTerminal = "/usr/bin/tilix"

------------------------------------------------------------------------
-- Workspaces

myWorkspaces = map show [1..9]

------------------------------------------------------------------------
-- Window rules

-- Default Managers (credit to  jsjolund @ https://gist.github.com/jsjolund/94f6821b248ff79586ba )
--
-- Match a string against anyone of a window's class, title, name, or role
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, cName, cRole]

-- Match against @WM_NAME@.
cName :: Query String
cName = stringProperty "WM_CLASS"
-- Match against @WM_WINDOW_ROLE@.

cRole :: Query String
cRole = stringProperty "WM_WINDOW_ROLE"

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

-- ManageHook --
pbManageHook :: ManageHook
pbManageHook = composeAll $ concat
    [ [ manageDocks ]
    , [ isDialog --> doCenterFloat ]
    , [ isFullscreen --> doFullFloat ]
    , [ fmap not isDialog --> doF avoidMaster ]
    ]

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
  _                                 -> c
------------------------------------------------------------------------
-- Layouts

myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

------------------------------------------------------------------------
-- Colors and borders

myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.

tabConfig = def {
    activeBorderColor = "#2aa198",
    activeTextColor = "#2aa198",
    activeColor = "#2aa198",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#268bd2"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 2

------------------------------------------------------------------------
-- Key bindings

-- Keyboard
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
  -----------------------------
  -- CUSTOM KEY BINDINGS
  
  -- --------------------------
  -- Launching and Kill Programs
  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [((modMask,                  xK_Return   ), spawn $ XMonad.terminal conf)

      -- Lock the screen using a screensaver
  , ((modMask .|. shiftMask,   xK_l        ), spawn "xscreensaver-command -lock")

      -- Launch launcher
  -- , ((modMask,                 xK_x        ), spawn "$HOME/.config/dmenu/dmenu-bind.sh")
  , ((modMask, 				   xK_x		   ), spawn "rofi -show run")
      -- Startup sublime for xmonad configuration
  , ((modMask,                 xK_e        ), spawn "/usr/bin/subl $HOME/.config/xmonad/xmonad.hs")

      -- Take a snapshot by selecting a region
  , ((modMask .|. controlMask, xK_5        ), spawn "scrot -s")

      -- Launch firefox
  , ((modMask,                 xK_w        ), spawn "firefox")

      -- Launch nautilus
  , ((modMask,                 xK_f        ), spawn "nautilus")

      -- Launch audio controls
  , ((modMask,                 xK_a        ), spawn "pavucontrol")
      -- Kill program
  , ((modMask .|. shiftMask,   xK_c        ), kill)
  
  ---------------------------
  -- Layout
  -- Change Layout
  , ((modMask,                 xK_space    ), sendMessage NextLayout)
  -- Change layout to the default layout of the current workspace
  , ((modMask .|. shiftMask,   xK_space    ), setLayout $ XMonad.layoutHook conf)
  
  ---------------------------
  -- Swapping
  -- Swap the focused window and the master window
  , ((modMask .|. shiftMask,   xK_m        ), windows W.swapMaster)

  -- Swap the focused window with the next window
  , ((modMask .|. shiftMask,   xK_j        ), windows W.swapDown  )

  -- Swap the focused window with the previous window
  , ((modMask .|. shiftMask,   xK_k        ), windows W.swapUp    )
  
  ---------------------------
  -- Resizing
  -- Shrink the master area
  , ((modMask,                 xK_h        ), sendMessage Shrink)

  -- Expand the master area
  , ((modMask,                 xK_l        ), sendMessage Expand)

  -- toggle the status bar gap
  , ((modMask,                 xK_b        ), sendMessage ToggleStruts)

  -- 2D navigation
  -- Move focus to next window
  , ((modMask,                 xK_Tab      ), windows W.focusDown)
  -- Move focus to previous window
  , ((modMask .|. shiftMask,   xK_Tab      ), windows W.focusUp)
  
  , ((modMask .|. shiftMask,   xK_l        ), screenGo R True)
  , ((modMask .|. shiftMask,   xK_h        ), screenGo L True)
  , ((modMask .|. controlMask, xK_l        ), screenSwap R True)
  , ((modMask .|. controlMask, xK_h        ), screenSwap L True)

  -- Minimize stuff
  , ((modMask, xK_v), withFocused minimizeWindow)
  , ((modMask .|. shiftMask,   xK_v        ), sendMessage RestoreNextMinimizedWin)

  , ((modMask,                 xK_g        ), placeFocused $ smart (0.5, 0.5))

  -- Inc or Dec # of windows in master area
  -- Increment the number of windows in the master area
  , ((modMask,                 xK_comma    ), sendMessage (IncMasterN 1))
  -- Deincrement the number of windows in the master area
  , ((modMask,                 xK_period   ), sendMessage (IncMasterN (-1)))

  -- Restart xmonad
  , ((modMask .|. shiftMask,   xK_r        ), broadcastMessage ReleaseResources >> restart "xmonad" True)
  -- Quit XMonad (Default)
  , ((modMask .|. shiftMask,   xK_q        ), io exitSuccess)

  -- Toggle Mute/Unmute Audio
  , ((0, 0x1008ff12                        ), spawn "amixer -q set Master toggle")
  -- Decrease Volume 
  , ((0, 0x1008ff11                        ), spawn "amixer -q set Master %5-")
  -- Increase Volume
  , ((0, 0x1008ff13                        ), spawn "amixer -q set Master %5+")

  -- Increase Screen brightness
  , ((0, 0x1008ff02                        ), spawn "xbacklight +5%")
  -- Decrease Screen brightness
  , ((0, 0x1008ff03                        ), spawn "xbacklight -5%")
  -- Run xmessage with a summary of the default keybindings (useful for beginners)`
  --  , ((modMask .|. shiftMask, 	  xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
 ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList $
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
  ]

------------------------------------------------------------------------
-- Startup hook

myStartupHook = return ()

------------------------------------------------------------------------
-- Main

main = do
  xmproc <- spawnPipe "$HOME/.cabal/bin/xmobar $HOME/.xmonad/xmobar.hs"
  xmonad $ defaults {
      logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = "   "
      }
      , manageHook = manageDocks <+> myManageHook
      , startupHook = setWMName "LG3D"
  }
defaults = def {
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = mod4Mask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    layoutHook = smartSpacing 10 myLayout,
    manageHook = placeHook (smart (0.5, 0.5))
    <+> pbManageHook
    <+> myManageHook,
    handleEventHook = docksEventHook <+> minimizeEventHook,
    startupHook        = myStartupHook
}
