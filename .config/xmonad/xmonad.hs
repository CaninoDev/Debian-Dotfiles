import qualified Data.Map as M

import qualified XMonad.StackSet as W
import Control.Exception
import Control.Monad
import System.Exit

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Config.Xfce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.ComboP
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane

import XMonad.Util.Run

import System.IO


promptExit = do
      response <- runProcessWithInput "dmenu" ["-p", "Really quit?"] "yes\nno\n"
      when (response == "yes") (io (exitWith ExitSuccess))

conf = ewmh xfceConfig
  { terminal = myTerminal,
    borderWidth = myBorderWidth,
                modMask = mod4Mask,
                workspaces = myWorkspaces,
                normalBorderColor = myNormalBorderColor,
                focusedBorderColor = myFocusedBorderColor,
                keys = myKeys,
                mouseBindings = myMouseBindings,
                manageHook = manageDocks <+> myManageHook,
                handleEventHook = ewmhDesktopsEventHook,
                startupHook = ewmhDesktopsStartup,
                layoutHook = myLayoutHook
        }

main :: IO ()
main = xmonad $ conf
        { startupHook   = startupHook conf
                          >> setWMName "LG3D"
        , logHook       = ewmhDesktopsLogHook <+> myLogHook
        }

-- Default Terminal
myTerminal = "terminix"
-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
myTabTheme = def
        {
                activeBorderColor = "#7C7C7C"
                , activeTextColor = "#CEFFAC"
                , activeColor = "#333333"
                , inactiveBorderColor = "#7C7C7C"
                , inactiveTextColor = "#EEEEEE"
                , inactiveColor = "#000000"
                , fontName = "xft:SFNS Display Nerd Font:size=12:antialias=true"
                , decoHeight = 36
        }
-- Layouts
myLayoutHook = avoidStruts $ tile ||| rtile ||| full ||| mtile ||| gimp
        where
                rt          = ResizableTall 1 (2/100) (1/2) []
                -- normal vertical tile
                tile        = named "[]="             $ smartBorders rt
                rtile       = named "=[]"             $ reflectHoriz $ smartBorders rt
                -- normal horizantal tile
                mtile       = named "M[]="            $ reflectHoriz $ Mirror rt
                --fullscreen with tabs
                tab         = named "T"               $ noBorders $ tabbed shrinkText myTabTheme
                -- two tab panes beside eachother, move windows with swapwindow
                tabB        = noBorders               $ tabbed shrinkText myTabTheme
                tabtile     = named "TT"              $ combineTwoP (TwoPane 0.03 0.5)
                                                                  (tabB)
                                                                  (tabB)
                                                                  (ClassName "firefox")
                -- two layouts for gimp, tabs and tiling
                gimp        = named "gimp"            $ combineTwoP (TwoPane 0.03 0.5)
                                                                    (tabB) (reflectHoriz
                                                                            $ combineTwoP (TwoPane 0.03 0.2)
                                                                              tabB        (tabB ||| Grid)
                                                                                          (Role "gimp-dock")
                                                                            )
                                                                    (Role "gimp-toolbox")
                -- fullscreen without tabs
                full        =   named "[]"            $ noBorders Full
-- Default Managers (credit to  jsjolund @ https://gist.github.com/jsjolund/94f6821b248ff79586ba )
--
-- Match a string against anyone of a window's class, title, name, or role
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]

-- Match against @WM_NAME@.
name :: Query String
name = stringProperty "WM_CLASS"
-- Match against @WM_WINDOW_ROLE@.
role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

-- ManageHook --
pbManageHook :: ManageHook
pbManageHook = composeAll $ concat
    [ [ manageDocks ]
    , [ manageHook def]
    , [ isDialog --> doCenterFloat ]
    , [ isFullscreen --> doFullFloat ]
    , [ fmap not isDialog --> doF avoidMaster ]
    ]

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
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
        -----------------------------
        -- CUSTOM KEY BINDINGS
        -- --------------------------
        -- Launching and Kill Programs
        -- Start a terminal.  Terminal to start is specified by myTerminal variable.
                    [((modMask,               xK_Return  ), spawn "terminix")

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

                -- Layouts

                        -- Rotate through the available layout algorithms
                 , ((modMask,                 xK_space    ), sendMessage NextLayout)

                        --  Reset the layouts on the current workspace to defaultdd
                 , ((modMask .|. shiftMask,   xK_space        ), setLayout $ XMonad.layoutHook conf)

                -- Floating layer stuff
                 , ((modMask,                                   xK_t                ), withFocused $ windows . W.sink)

                -- Refresh
                 , ((modMask,                                   xK_r                ), refresh)

                -- Close focused window
                 , ((modMask .|. shiftMask,   xK_c        ), kill)
                        -- Resize viewed windows to the correct size
                 , ((modMask,                 xK_n        ), refresh)

                -- focus
                        -- Move focus to the next window
                 , ((modMask,                 xK_Tab      ), windows W.focusDown)

                        -- Move focus to the next window
                 , ((modMask,                 xK_j        ), windows W.focusDown)

                -- Move focus to the previous window
                 , ((modMask,                 xK_k        ), windows W.focusUp  )

                -- Move focus to the master window
                 , ((modMask,                 xK_m        ), windows W.focusMaster  )
----------------------
                -- Swapping
                        -- Swap the focused window and the master window
                 , ((modMask .|. shiftMask,   xK_m  ), windows W.swapMaster)
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

                -- Inc or Dec # of windows in master area
                        -- Increment the number of windows in the master area
                 , ((modMask,                 xK_comma    ), sendMessage (IncMasterN 1))
                        -- Deincrement the number of windows in the master area
                 , ((modMask,                 xK_period   ), sendMessage (IncMasterN (-1)))

                -- Restart xmonad
                 , ((modMask .|. shiftMask,   xK_r        ), broadcastMessage ReleaseResources >> restart "xmonad" True)
                -- Quit XMonad (Default)
								 , ((modMask .|. shiftMask,   xK_q        ), promptExit)
                 ]
                 ++
            -- mod-[1..9], Switch to workspace N
            -- mod-shift-[1..9], Move client to workspace N
            --
            [ ((m .|. modMask, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
            ]

myWorkspaces = ["1:Term","2:Web","3:Code","4:Spotify","5:Video","6:Chat","7:Art","8:Misc"]
------------------------------------------------------------------------
myStatusBar :: String
myStatusBar = "xmobar"
myLauncher = "$(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')"
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

renameWS :: String -> X ()
renameWS newTag = windows $ \s -> let old = W.tag $ W.workspace $ W.current s
                                  in W.renameTag old newTag s


-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 2

-- Mouse bindings: default actions bound to mouse events
myMouseBindings XConfig{XMonad.modMask = modMask} = M.fromList
--
-- mod-button1, Set the window to floating mode and move by dragging
 [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)

-- mod-button3, Raise the window to the top of the stack
 , ((modMask, button3), \w -> focus w >> windows W.swapMaster)

-- mod-button2, Set the window to floating mode and resize by dragging
 , ((modMask, button2), \w -> focus w >> mouseResizeWindow w)
-- you may also bind events to the mouse scroll wheel (button4 and button5)
 ]
myLogHook =
        dynamicLogWithPP xmobarPP {
                                                            ppCurrent       = xmobarColor "#2aa198" "#002b35" . pad
                                                            , ppVisible     = xmobarColor "#859900" "" . pad
                                                            , ppHidden      = xmobarColor "#fdf6e3" "" . pad
                                                            , ppUrgent      = xmobarColor "#dc322f" "" . pad
                                                            , ppWsSep           = ""
                                                            , ppSep             ="|"
                                                            , ppLayout      = xmobarColor "#859900" "" .
                                                                 (\ x -> case x of
                                                                 "Maximize Tall"                                        -> "[]="
                                                                 "Maximize Mirror Tall"                         -> "TTT"
                                                                 "Maximize Full"                                        -> "<M>"
                                                                 "Maximize Grid"                                        -> "+++"
                                                                 "Maximize Spiral"                                  -> "(@)"
                                                                 "Maximize Accordion"                               -> "Acc"
                                                                 "Maximize Tabbed Simplest"                 -> "Tab"
                                                                 "Maximize Tabbed Bottom Simplest"  -> "TaB"
                                                                 "Maximize SimplestFloat"                       -> "><>"
                                                                 "Maximize IM"                                          -> "IM "
                                                                 "Maximize Dishes 2 (1%6)"                  -> "Dsh"
                                                                 _                                                                  -> pad x
                                                             )
                                                             }
