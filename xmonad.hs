import XMonad
import Data.Monoid
import System.Exit
import GHC.IO.Handle.Types

import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
--import XMonad.Util.WorkspaceCompare (getWsIndex)
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO 

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier

import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CycleWS

import XMonad.Hooks.InsertPosition
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Colors and defaults:
myTerminal :: String
myTerminal = "st"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth  = 2

myModMask :: KeyMask
myModMask  = mod4Mask

xmobarEscape :: String -> String
xmobarEscape  = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape $
  ["1:www","2:tr1","3:tr2","4:tr3","5:tel","6:irc","7:cmp"]
  where
    clickable l = ["<action=`wmctrl -s " ++ (show n) ++ "`>" ++ ws ++ "</action>"
                  | (i,ws) <- zip [0..6] l, let n = i :: Int]

myColors :: [String]
myColors = [   "#FFFFE8" -- 00 black 
             , "#880000" -- 01 red
             , "#005500" -- 02 green
             , "#8F7634" -- 03 yellow
             , "#1054AF" -- 04 blue
             , "#555599" -- 05 magenta
             , "#007777" -- 06 cyan
             , "#E5E5D0" -- 07 white
             , "#444444" -- 08 brblack
             , "#F8E8E8" -- 09 brred
             , "#E8FCE8" -- 10 brgreen
             , "#F8FCE8" -- 11 bryellow
             , "#E1FAFF" -- 12 brblue
             , "#FFEAFF" -- 13 brmagenta
             , "#9EEEEE" -- 14 brcyan
             , "#CCCCB7" -- 15 brwhite
           ]

myForeground :: String
myForeground = "#5a5a4c"

myBackground :: String
myBackground = "#777777"

myBar :: String
myBar = "xmobar ~/.config/xmobar/config.hs"

myNormalBorderColor :: String
myNormalBorderColor  = myColors !! 15

myFocusedBorderColor :: String
myFocusedBorderColor = myColors !! 14

scratchpads :: [NamedScratchpad]
scratchpads = [
    NS "scratchpad" (myTerminal ++ " -t scratchpad")
      (title =? "scratchpad") doCenterFloat
      
  , NS "music"      (myTerminal ++ " -c cmus music" )
      (className =? "cmus"  ) doCenterFloat
    
  , NS "umpv"       "umpv"
      (resource  =? "umpv"  ) doCenterFloat 
    ]

-- Keybinds
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_e     ), spawn $ myTerminal ++ " lf")
    , ((modm,               xK_f     ), spawn $ myTerminal ++ " tremc")
    , ((modm,               xK_e     ), spawn "emacsclient -a emacs -c")
    , ((modm .|. shiftMask, xK_f     ), spawn "mountpi")
    , ((modm,               xK_g     ), spawn $ myTerminal ++ " -t float htop")
    , ((modm .|. shiftMask, xK_x     ), spawn "powermenu")
    , ((modm .|. shiftMask, xK_n     ), spawn "bookmarksman")
    , ((modm .|. shiftMask, xK_b     ), spawn "$BROWSER")
    , ((modm .|. shiftMask, xK_t     ), spawn "telegram-desktop")
    , ((modm,               xK_Print ), spawn "scrot -q 100 -e 'mv $f ~/pics/screenshots/'")
    , ((modm .|. shiftMask, xK_Print ), spawn "sleep 0.5 && tmpscrot")
    -- Scratchpads
    , ((modm,               xK_o     ), namedScratchpadAction scratchpads "scratchpad")
    , ((modm,               xK_m     ), namedScratchpadAction scratchpads "music")
    , ((modm,               xK_u     ), namedScratchpadAction scratchpads "umpv")
    -- Multimedia keys
    , ((0,   xF86XK_AudioLowerVolume ), spawn "amixer -q sset Master 5%-")
    , ((0,   xF86XK_AudioRaiseVolume ), spawn "amixer -q sset Master 5%+")
    , ((0,   xF86XK_AudioMute        ), spawn "amixer -q sset Master toggle")
    , ((0,   xF86XK_AudioPrev        ), spawn "cmus-remote -r")
    , ((0,   xF86XK_AudioNext        ), spawn "cmus-remote -n")
    , ((0,   xF86XK_AudioPlay        ), spawn "cmus-remote -u")
    , ((0,   xF86XK_Launch1          ), spawn $ myTerminal ++ " -t float -e alsamixer")
    , ((0,   xF86XK_MonBrightnessUp  ), spawn "xbacklight -inc 5")
    , ((0,   xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
    , ((0,   xF86XK_ScreenSaver      ), spawn "slock")
     -- Layouts  
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Window operations
    , ((modm,               xK_w     ), kill)
    , ((modm,               xK_n     ), refresh)
    -- Window movment:
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage $ IncMasterN 1)
    , ((modm              , xK_period), sendMessage $ IncMasterN (-1))
    -- Toggle the status bar gap
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    -- Quit
    , ((modm .|. shiftMask, xK_q     ), io $ exitWith ExitSuccess)
    , ((modm              , xK_q     ), spawn "xmonadcompile")
    -- Workspace movment
    , ((modm .|. shiftMask, xK_h     ), moveTo Prev NonEmptyWS)
    , ((modm .|. shiftMask, xK_l     ), moveTo Next NonEmptyWS)
    ]
    ++
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- Layouts and hooks
type MyLayout = XMonad.Layout.LayoutModifier.ModifiedLayout
       AvoidStruts
       (XMonad.Layout.LayoutModifier.ModifiedLayout
          SmartBorder
          (Choose
             (XMonad.Layout.LayoutModifier.ModifiedLayout Rename Tall)
             (Choose
                (XMonad.Layout.LayoutModifier.ModifiedLayout Rename (Mirror Tall))
                (XMonad.Layout.LayoutModifier.ModifiedLayout Rename Full))))
       
myLayout :: MyLayout Window
myLayout = avoidStruts $ smartBorders (renamed [Replace tiledIcon] tiled
                                        ||| renamed [Replace mirrorIcon] (Mirror tiled)
                                        ||| renamed [Replace fullIcon] Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     -- Icons
     tiledIcon  = "\57525"
     fullIcon   = "\57524"
     mirrorIcon = "\57526"
     
myManageHook :: ManageHook
myManageHook = composeAll
    [ insertPosition Below Newer
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , title     =? "float"          --> doCenterFloat
    , title     =? "Media viewer"   --> doFullFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "umpv"           --> doFloat
    , namedScratchpadManageHook scratchpads
    ] 

myEventHook :: Event -> X All
myEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook


-- See the 'XMonad.Hooks.DynamicLog' extension for examples.

myLogHook :: Handle -> X ()
myLogHook xmproc =  dynamicLogWithPP $ (def PP)
  {   ppCurrent           = xmobarColor (myColors !! 4 ) (myColors !! 12) . wrap "[" "]"
    , ppHidden            = xmobarColor myForeground $ myColors !! 12
    , ppHiddenNoWindows   = const ""
    , ppWsSep             = " "
    , ppTitle             = pad
    , ppSep               = xmobarColor (myColors !! 15) (myColors !! 12) "|"
    --, ppSort   = fmap (namedScratchpadFilterOutWorkspace.) DO.getSortByOrder
    , ppOutput = hPutStrLn xmproc  . fitTitle
  }
  where
    fitTitle :: String -> String
    fitTitle str =
      shorten (((length str - length (xmobarStrip str))+101)) str
                   
myStartupHook :: X()
myStartupHook = do
  spawnOnce "picom --experimental-backends"
  spawnOnce $ "hsetroot -solid '" ++ myBackground ++ "'"
  setDefaultCursor xC_left_ptr

-- Main function
main :: IO()
main = do
    bar <- spawnPipe myBar
    xmonad $ ewmh $ docks $ defaults bar

defaults :: Handle -> XConfig MyLayout
defaults bar = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook ,
        logHook            = myLogHook bar,
        startupHook        = myStartupHook
        }
