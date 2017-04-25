import XMonad
import System.Exit

import XMonad.Actions.Submap
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.DynamicWorkspaces

import XMonad.Layout.Circle
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Circle
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed

import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.Input

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import Data.List
import Data.Function

import System.IO

------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "/usr/bin/terminix"
------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:Term","2:Web","3:Code","4:Spotify","6:Video","7:Chat","8:Art","9:Misc"]
------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll [ className =? "Sublime-Text-3"           --> doShift "3:Code"
													, className =? "Gimp"                     --> doShift "8:Art"
													, className =? "Pavucontrol"              --> doFloat
													, manageDocks
													, scratchpadManageHook (W.RationalRect 0.125 0.25 0.75 0.5) ]

------------------------------------------------------------------------
-- The command to use as a launcher, to laucn commands that don't have
--  preset bindings
myLauncher = "$(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')"
------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
{-myLayout = avoidStruts (-}
    {-tabbed shrinkText tabConfig -}
		{-||| Tall 1 (3/100) (1/2)-}
		{-||| Mirror (Tall 1 (3/100) (1/2))-}
		{-||| Full) -}
		{-||| noBorders (fullscreenFull Full)-}
myLayout = avoidStruts $
           tiled
           ||| Mirror tiled
           ||| Full
           ||| tabbed shrinkText defaultTheme
           ||| threeCol
           ||| spiral (4/3)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     threeCol = ThreeCol nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 2/100
------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

myXPConfig = defaultXPConfig
-- sendKeyPress :: KeyMask -> KeySym -> X ()
-- sendKeyPress = userCode $ withDisplay sendKeyPress'
--
-- sendKeyPress' :: Display -> KeyMask -> KeySym -> X ()
-- sendKeyPress' dpy mask key = do
--   root <- asks theRoot
--   time <- currentTime
--   evt  <
renameWS :: String -> X ()
renameWS newTag = windows $ \s -> let old = W.tag $ W.workspace $ W.current s
                                  in W.renameTag old newTag s

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C"
		, activeTextColor = "#CEFFAC"
		, activeColor = "#333333"
		, inactiveBorderColor = "#7C7C7C"
		, inactiveTextColor = "#EEEEEE"
		, inactiveColor = "#000000"
		, fontName = "xft:DejaVu Sans Mono:size=14:bold:antialias=true",
    decoHeight = 36
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 2
------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $

----------------------------------------------------------------------
-- Custom key bindings
--
-- Start a terminal.  Terminal to start is specified by myTerminal variable.
	[((modMask,                  xK_t       ), spawn "/usr/bin/terminix")

--`r ck the screen using a screensaver
 , ((modMask .|. shiftMask,   xK_l        ), spawn "xscreensaver-command -lock")

-- Startup nvim-qt
 , ((modMask,                 xK_e        ), spawn "nvim-qt")

-- Take a snapshot by selecting a region
 , ((modMask .|. controlMask, xK_5        ), spawn "scrot -s")

-- Launch firefox
 , ((modMask,                 xK_w        ), spawn "firefox")

-- Close focused window
 , ((modMask .|. shiftMask,   xK_c        ), kill)

-- Rotate through the available layout algorithms
 , ((modMask,                 xK_space    ), sendMessage NextLayout)

--  Reset the layouts on the current workspace to default
 , ((modMask .|. shiftMask,   xK_space    ), setLayout $ XMonad.layoutHook conf)

-- Resize viewed windows to the correct size
 , ((modMask,                 xK_n        ), refresh)

-- Move focus to the next window
 , ((modMask,                 xK_Tab      ), windows W.focusDown)

-- Move focus to the next window
 , ((modMask,                 xK_j        ), windows W.focusDown)

-- Move focus to the previous window
 , ((modMask,                 xK_k        ), windows W.focusUp  )

-- Move focus to the master window
 , ((modMask,                 xK_m        ), windows W.focusMaster  )

-- Swap the focused window and the master window
 , ((modMask,                 xK_Return   ), windows W.swapMaster)
-- Swap the focused window with the next window
 , ((modMask .|. shiftMask,   xK_j        ), windows W.swapDown  )

-- Swap the focused window with the previous window
 , ((modMask .|. shiftMask,   xK_k        ), windows W.swapUp    )

-- Shrink the master area
 , ((modMask,                 xK_h        ), sendMessage Shrink)

-- Expand the master area
 , ((modMask,                 xK_l        ), sendMessage Expand)

-- Push window back into tiling
 , ((modMask,                 xK_t        ), withFocused $ windows . W.sink)

-- Increment the number of windows in the master area
 , ((modMask,                 xK_comma    ), sendMessage (IncMasterN 1))

-- Deincrement the number of windows in the master area
 , ((modMask,                 xK_period   ), sendMessage (IncMasterN (-1)))

-- toggle the status bar gap
 , ((modMask,                 xK_b        ), sendMessage ToggleStruts)
-- Restart xmonad
 , ((modMask .|. shiftMask,   xK_r        ), broadcastMessage ReleaseResources >> restart "xmonad" True)
 , ((modMask              ,   xK_g        ), workspacePrompt myXPConfig (windows . W.greedyView))
 , ((modMask .|. shiftMask,   xK_g        ), workspacePrompt myXPConfig (windows . W.shift))
-- C-t submap
	 , ((controlMask,   xK_t),   submap . M.fromList $
			[  ((controlMask, xK_t) ,   toggleWS)
	     , ((0,            xK_Tab)    ,   windows W.focusDown) -- @@ Move focus to the next window
       , ((shiftMask,    xK_Tab )   ,   windows W.focusUp) -- @@ Move focus to the previous window
       , ((0,            xK_c)      ,   spawn $ XMonad.terminal conf)
       , ((0,            xK_k)      ,   kill)
       , ((0,            xK_Return) ,   windows W.swapMaster)
       , ((shiftMask,    xK_1)      ,   spawn "dmenu_run")
       , ((shiftMask,    xK_1)      ,   scratchpadSpawnActionTerminal "gterm")
       , ((0,            xK_p)      ,   spawn "/home/nelhage/bin/viewpdf")
       , ((0,            xK_s)      ,   sshPrompt myXPConfig)
       , ((shiftMask,    xK_s)      ,   spawn "/usr/bin/tracker-search-tool")
       , ((0,            xK_x)      ,   spawn "/home/nelhage/bin/rp-hm-complete.sh")
       , ((0,            xK_g)      ,   workspacePrompt  myXPConfig (windows . W.greedyView))
       , ((shiftMask,    xK_g)      ,   workspacePrompt  myXPConfig (windows . W.shift))
       , ((0,            xK_n)      ,   inputPrompt myXPConfig "Workspace name" ?+ addWorkspace)
       , ((shiftMask,    xK_k)      ,   removeWorkspace)
       , ((0,            xK_r)      ,   renameWorkspace myXPConfig)
      ] ++
 [((0, k), windows $ W.greedyView i) | (i, k) <- zip (take 10 (XMonad.workspaces conf)) [xK_1 ..]
  ])
		]
		++
	--------------------------------------------------------------------------------------
	--
	-- mod-[1..9], Switch to workspace N
	-- mod-shift-[1..9], Move client to workspace N
	--
	[((m .|. modMask, k), windows $ f i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9], (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
----------------------------------------------------------------------------------------
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
{-myLogHook = dynamicLogWithPP $ defaultPP { ppOutput = PutStrLn, ppTitle = xmobarColor "green" "" . shorten 50 }-}

main = do
		xmproc <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc"
		xmonad defaults

defaults = defaultConfig {
												 terminal = myTerminal,
												 borderWidth = myBorderWidth,
												 modMask = myModMask,
												 workspaces = myWorkspaces,
												 normalBorderColor = myNormalBorderColor,
												 focusedBorderColor = myFocusedBorderColor,
												 keys = myKeys,
												 mouseBindings = myMouseBindings,
												 layoutHook = myLayout,
												 manageHook = myManageHook
												 {-logHook = logHook-}
												 }
												 `additionalKeysP` [
															  ("<XF86MonBrightnessUp>", spawn "xbacklight +10")
															 ,("<XF86MonBrightnessDown>", spawn "xbacklight -10")]
