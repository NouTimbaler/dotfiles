
--------------------------------------------------------------------------------------------------
---      ___      ___ ________        ___  ___      ___ ________  ________  ________           ---
---     |\  \    /  /|\   __  \      |\  \|\  \    /  /|\   __  \|\   ___ \|\   __  \          ---
---     \ \  \  /  / | \  \|\  \     \ \  \ \  \  /  / | \  \|\  \ \  \_|\ \ \  \|\  \         ---
---      \ \  \/  / / \ \  \\\  \  __ \ \  \ \  \/  / / \ \  \\\  \ \  \ \\ \ \   __  \        ---
---       \ \    / /   \ \  \\\  \|\  \\_\  \ \    / /   \ \  \\\  \ \  \_\\ \ \  \ \  \       ---
---        \ \__/ /     \ \_______\ \________\ \__/ /     \ \_______\ \_______\ \__\ \__\      ---
---         \|__|/       \|_______|\|________|\|__|/       \|_______|\|_______|\|__|\|__|      ---
---                                                                                            ---
--------------------------------------------------------------------------------------------------






-- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen, prevWS, nextWS)
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotAllDown)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust, isJust)
import Data.Monoid
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ScreenCorners
import XMonad.Hooks.FadeInactive

    -- Layouts
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce





-- ##################################################################
--- My variables
-- ##################################################################


chosen_font :: String
chosen_font = "xft:FiraCode Nerd Font:pixelsize=15:antialias=true:hinting=true"

mod_mask :: KeyMask
mod_mask = mod4Mask                                       -- Sets modkey to super/windows key

terminal_emulator :: String
terminal_emulator = "alacritty "

web_browser :: String
web_browser = "firefox "

rss_reader :: String
rss_reader = terminal_emulator ++ " -e newsboat"    

matrix_client :: String
matrix_client = "nheko "

text_editor :: String
text_editor = terminal_emulator ++ " -e nvim "
--text_editor = "emacsclient -c -a 'emacs' "

normal_w_color :: String
normal_w_color = "#232323"                                -- Border color of normal windows

focused_w_color :: String
focused_w_color = "#FFFFFF"                               -- Border color of focused windows

file_manager :: String
file_manager = terminal_emulator ++ " -e lf"               

network_tool :: String
network_tool = terminal_emulator ++ " -e sudo nmtui"

power_menu :: String
power_menu = "powermenu-v"  

system_monitor :: String
system_monitor = terminal_emulator ++ " -e htop"            

border_width :: Dimension
border_width = 2                                          -- Sets border width for windows






-- ##################################################################
--- Startup
-- ##################################################################

myStartupHook :: X ()
myStartupHook = do

  --  spawnOnce "picom &"                      -- Transparency and stuff
    spawnOnce "setxkbmap es"                   -- Set es keyboard
    spawnOnce "~/.fehbg"                       -- Set the wallpaper 
    spawnOnce "unclutter &"                    -- Remove mouse when idle
    spawnOnce "xsetroot -cursor_name left_ptr" -- Change cursor outside windows
    spawnOnce "dunst &"                        -- Notifications
    spawnOnce "stalonetray"                    -- System tray
    spawnOnce "usbnotify.sh"
    --spawnOnce "/usr/bin/emacs --daemon &"

 --   setWMName "Devil of Paradis"

    -- addScreenCorners [ (SCUpperLeft,  prevWS), (SCUpperRight, nextWS) ]






-- ##################################################################
--- Scratchpads
-- ##################################################################

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal"  spawnTerm findTerm manageTerm
                , NS "sysinfo"   spawnTop  findTop  manageTop
                , NS "screenrec" spawnRec  findRec  manageRec
                ]
  where

    spawnTerm  = terminal_emulator ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.75
                 w = 0.75
                 t = 0.125 
                 l = 0.125

    spawnTop   = terminal_emulator ++ " -t sysinfo -e gotop"
    findTop    = title =? "sysinfo"
    manageTop  = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w 

    spawnRec   = "simplescreenrecorder" 
    findRec    = title =? "SimpleScreenRecorder"
    manageRec  = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.5
                 t = 0.75 -h
                 l = 0.75 -w 




-- GAPS

gap_width :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
gap_width i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Single window has no gaps.
gap_width' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
gap_width' i = spacingRaw True (Border i i i i) False (Border i i i i) False






-- ##################################################################
--- Layouts
-- ##################################################################

tall     = renamed [Replace "[]="]
           $ smartBorders
           $ subLayout [] (smartBorders Simplest)
           $ gap_width 5
           $ ResizableTall 1 (3/100) (0.55) []

monocle  = renamed [Replace "[M]"]
           $ smartBorders
           $ subLayout [] (smartBorders Simplest)
           $ Full

floats   = renamed [Replace "><>"]
           $ smartBorders
           $ simplestFloat




-- The layout hook

myLayoutHook = screenCornerLayoutHook $ avoidStruts $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder border_width tall
                                 ||| noBorders monocle
                                 ||| floats




-- ##################################################################
--- Workspaces
-- ##################################################################

myWorkspaces = map show [1..10]

clickable :: String -> String -> String
clickable s wsIcon = "<action=xdotool key super+" ++ s ++ ">" ++ wsIcon ++ "</action>"


leftSep :: String -> String
leftSep s = "<fn=2><fc=#555555>\xe0b2</fc></fn>" ++ s

rightSep :: String -> String
rightSep s = s ++ "<fn=2><fc=#555555>\xe0b0</fc></fn>"


-- Show Workspace Name

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:FiraCode Nerd Font:antialias=true:hinting=true:bold:size=75"
    , swn_fade              = 0.5
    , swn_bgcolor           = "#FFFFFF"
    , swn_color             = "#000000"
    }





-- ##################################################################-
--- Managing windows
-- ##################################################################

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "confirm"         --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "firefox"         --> doShift ( "2" )
     , className =? "discord"         --> doShift ( "9" )
     , className =? "Spotify"         --> (className >>= io . appendFile "/home/noutimbaler/xmonad_debug" >> idHook)
     , className =? "Spotify"         --> doShift ( "9" )
     , className =? "mpv"             --> doShift ( "2" )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ] <+> namedScratchpadManageHook myScratchPads






-- ##################################################################
--- Keybindings
-- ##################################################################

myKeys :: [(String, X ())]
myKeys =

--------------------------------------------------------------
    -- Xmonad
        [ ("M-S-r", spawn "restart_xmonad")    -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                                   -- Quits xmonad
        , ("M-x"  , spawn (text_editor ++ " .xmonad/xmonad.hs") ) 
        , ("M-S-x", spawn (text_editor ++ " .config/xmobar/xmobar.hs") ) 
--------------------------------------------------------------

    -- 10th Workspace
        , ("M-0",   windows $ W.greedyView "10")


--------------------------------------------------------------
    -- Dmenu Scripts
        , ("M-p",   spawn (power_menu))   -- power menu
        , ("M-S-w", spawn "web-v")         -- most visited pages
        , ("M-c",   spawn "edit_confs.sh") -- conf editor
--------------------------------------------------------------





---------------------------------------------------------------
   -- Useful programs to have a keybinding for launch
        , ("M-<Return>", spawn (terminal_emulator))
        , ("M-w",        spawn (web_browser))
        , ("M-d",        spawn "dmenu_run -c -l 5 -bw 2 -m 0 -p \"Run command:\"")
        , ("M-m",        spawn (matrix_client))
        , ("M-r",        spawn (file_manager))
        , ("M-n",        spawn (rss_reader))
        , ("M-S-p",      spawn (system_monitor))
--------------------------------------------------------------




---------------------------------------------------------------
    -- Kill windows
        , ("M-q",   kill1)     -- Kill the currently focused client
        , ("M-S-a", killAll)   -- Kill all windows on current workspace
--------------------------------------------------------------




---------------------------------------------------------------
    -- Floating windows
        , ("M-S-f",   sendMessage (T.Toggle "float"))   -- Toggles 'floats' layout
        , ("M-t",   withFocused $ windows . W.sink)    -- Push floating window back to tile
        , ("M-S-t", sinkAll)                           -- Push ALL floating windows to tile
--------------------------------------------------------------




---------------------------------------------------------------
    -- Increase/decrease spacing (gaps)
        , ("C-M1-j", decWindowSpacing 4)         -- Decrease window spacing
        , ("C-M1-k", incWindowSpacing 4)         -- Increase window spacing
        , ("C-M1-h", decScreenSpacing 4)         -- Decrease screen spacing
        , ("C-M1-l", incScreenSpacing 4)         -- Increase screen spacing
--------------------------------------------------------------



---------------------------------------------------------------
    -- Windows navigation
        , ("M-y",           windows W.focusMaster)  -- Move focus to the master window
        , ("M-j",           windows W.focusDown)    -- Move focus to the next window
        , ("M-k",           windows W.focusUp)      -- Move focus to the prev window
        , ("M-<Backspace>", promote)                -- Moves focused window to master, others maintain order
        , ("M-<Tab>",     rotAllDown)             -- Rotate all the windows in the current stack
--------------------------------------------------------------



---------------------------------------------------------------
    -- Layouts
        , ("M-S-<Tab>", sendMessage NextLayout)                                       -- Switch to next layout
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)   -- Toggles noborder/full
--------------------------------------------------------------




---------------------------------------------------------------
    -- Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-M1-h", sendMessage MirrorShrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-l", sendMessage MirrorExpand)                   -- Expand horiz window width
--------------------------------------------------------------



---------------------------------------------------------------
    -- Scratchpads
    -- Toggle them to hide and it sends them back to hidden workspace (NSP).
        , ("M-s t", namedScratchpadAction myScratchPads "terminal")
        , ("M-s g", namedScratchpadAction myScratchPads "sysinfo")
        , ("M-s r", namedScratchpadAction myScratchPads "screenrec")
-------------------------------------------------------------




---------------------------------------------------------------
    -- Multimedia Keys
    
        -- Sound
        , ("<XF86AudioMute>",        spawn "pamixer -t")
        , ("<XF86AudioLowerVolume>", spawn "pamixer --allow-boost -d 5")
        , ("<XF86AudioRaiseVolume>", spawn "pamixer --allow-boost -i 5")

        -- Brightness
        , ("<XF86MonBrightnessUp>",   spawn "brightnessctl set +5%")
        , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")

        -- Misc.
        , ("<XF86HomePage>", spawn (web_browser ++ " https://gitlab.com/vojjvoda") )
        , ("<XF86Mail>",     spawn (web_browser ++ " https://protonmail.com") )

        , ("<Print>",		spawn "maim -s | xclip -selection clipboard -t image/png")
        , ("M-<Print>",		spawn "screenshot.sh")
        ]
--------------------------------------------------------------




          


-- ##################################################################
--- Main func, also xmobar handling
-- ##################################################################


main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobar.hs"
    xmonad $ ewmh def
        { manageHook         = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        , handleEventHook    = docksEventHook 
        , modMask            = mod_mask
        , terminal           = terminal_emulator
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook -- showWName' myShowWNameTheme $ myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = border_width
        , normalBorderColor  = normal_w_color
        , focusedBorderColor = focused_w_color
        , logHook = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP -- $ fadeInactiveLogHook 0.8

              

        -- settings for xmobar.
              { ppOutput          = \x -> hPutStrLn xmproc0 x

              , ppCurrent         = \s -> clickable s "\xf111 "
              , ppVisible         = \s -> clickable s $ xmobarColor "#AAAAAA" "#555555" "\xf111 "
--              , ppVisible         = xmobarColor "#FFFFFF" "#555555" . \s -> clickable s
              , ppHidden          = \s -> clickable s "\xf192 "
              , ppHiddenNoWindows = \s -> clickable s "\xf10c "

              , ppLayout          = xmobarColor "#BBBBBB" "#555555" . (\s -> rightSep "" ++ s) . leftSep . wrap " <fn=3>" "</fn>"
              , ppTitle           = xmobarColor "#FFFFFF" "#555555" . rightSep . wrap "<fn=3>" "</fn> "
              , ppSep             = " " -- "<fn=2><fc=#555555>\xe0b8 </fc></fn><fn=2><fc=#555555>\xe0be </fc></fn>"
              , ppWsSep           = " "

              , ppOrder           = \(ws:l:t:ex) -> [ws,l]  ++ [t] -- order of things in xmobar : workspaces, layout, title
              }
        } `additionalKeysP` myKeys
