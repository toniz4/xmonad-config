import XMonad
import Data.Monoid
import System.Exit
import GHC.IO.Handle.Types
import Graphics.X11.ExtraTypes.XF86

import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare (getSortByTag)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier
import XMonad.Layout.CenteredMaster
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid

import XMonad.Actions.CycleWS

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
myBorderWidth  = 3

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
myColors = [   "#FFFFEA" -- black
             , "#880000" -- red
             , "#005500" -- green
             , "#663311" -- yellow
             , "#00009A" -- blue
             , "#770077" -- magenta
             , "#007777" -- cyan
             , "#EEEECC" -- white
             , "#737373" -- brblack
             , "#FFEAEA" -- brred
             , "#EAFFEA" -- brgreen
             , "#EEEE9E" -- bryellow
             , "#CCEEFF" -- brblue
             , "#FFEAFF" -- brmagenta
             , "#EAFFFF" -- brcyan
             , "#CCCCB7" -- brwhite
           ]

myForeground :: String
myForeground = "#000000"

myBackground :: String
myBackground = myColors !! 8

myBar :: String
myBar = "xmobar ~/.config/xmonad/xmobar.hs"

myNormalBorderColor :: String
myNormalBorderColor  = myColors !! 12

myFocusedBorderColor :: String
myFocusedBorderColor = "#55AAAA"

scratchpads :: [NamedScratchpad]
scratchpads = [
    NS "scratch" (myTerminal ++ " -c scratch"   ) (className =? "scratch") doCenterFloat
  , NS "music"   (myTerminal ++ " -c cmus music") (className =? "cmus"   ) doCenterFloat
  , NS "umpv"    "umpv"                           (resource  =? "umpv"   ) doCenterFloat
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
    , ((modm,               xK_Print ), spawn "capture shot full")
    , ((modm .|. shiftMask, xK_Print ), spawn "capture shot sel tmp")
    -- Scratchpads
    , ((modm,               xK_o     ), namedScratchpadAction scratchpads "scratch")
    , ((modm,               xK_m     ), namedScratchpadAction scratchpads "music")
    , ((modm,               xK_u     ), namedScratchpadAction scratchpads "umpv")
    , ((modm,               xK_r     ), spawn "capture vid sel tmp")
    , ((modm .|. shiftMask, xK_r     ), spawn "capture vid full")
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
       
type MyLayout = ModifiedLayout
       AvoidStruts
       (ModifiedLayout
          SmartBorder
          (Choose
             (ModifiedLayout Rename Tall)
             (Choose
                (ModifiedLayout Rename (Mirror Tall))
                (Choose
                  (ModifiedLayout Rename Full) (ModifiedLayout Rename Grid)))))
       
myLayout :: MyLayout Window
myLayout =
  avoidStruts $
  smartBorders $
      renamed [Replace tiledIcon]  tiled
  ||| renamed [Replace mirrorIcon] (Mirror tiled)
  ||| renamed [Replace fullIcon]   Full
  ||| renamed [Replace gridIcon]   Grid
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100
     -- Icons
     tiledIcon  = "\57525"
     fullIcon   = "\57524"
     mirrorIcon = "\57526"
     gridIcon   = "\57528"
     
myManageHook :: ManageHook
myManageHook = composeAll
    [ insertPosition Below Newer
    , namedScratchpadManageHook scratchpads
    , className =? "Gimp"                 --> doFloat
    , className =? "Dragon-drag-and-drop" --> doCenterFloat
    , title     =? "float"                --> doCenterFloat
    , title     =? "Media viewer"         --> doFullFloat
    , resource  =? "desktop_window"       --> doIgnore
    , isDialog                            --> doCenterFloat
    ]

myEventHook :: Event -> X All
myEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook

myLogHook :: Handle -> X ()
myLogHook xmproc =  dynamicLogWithPP $ (def PP)
  {   ppCurrent         = xmobarColor (myColors !! 4 ) (myColors !! 14) . wrap "[" "]"
    , ppHidden          = xmobarColor myForeground $ myColors !! 14
    , ppHiddenNoWindows = const ""
    , ppWsSep           = " "
    , ppTitle           = pad . xmobarStrip
    , ppSep             = xmobarColor (myColors !! 15) (myColors !! 14) "|"
    , ppSort            = fmap (. namedScratchpadFilterOutWorkspace) getSortByTag
    , ppOutput          = hPutStrLn xmproc . fitTitle
  }
  where
    -- Space between title and calendar, deppends on size of the font
    space :: Int
    space = 101
    fitTitle :: String -> String
    fitTitle str =
      shorten (((length str - length (xmobarStrip str))+space)) str
                   
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
