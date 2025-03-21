
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
import XMonad.Hooks.ManageDocks -- (SetStruts, avoidStruts, avoidStrutsOn, docks, manageDocks, ToggleStruts(..), Direction2D(..))
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ScreenCorners
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.StatusBar.PP

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

mod_mask :: KeyMask
mod_mask = mod4Mask                                       -- Sets modkey to super/windows key

terminal_emulator :: String
terminal_emulator = "alacritty "

web_browser :: String
web_browser = "firefox "

rss_reader :: String
rss_reader = terminal_emulator ++ " -e newsboat"    

text_editor :: String
text_editor = terminal_emulator ++ " -e vim "
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

startWindowSetup :: X ()
startWindowSetup = do
    -- Focus the second screen.
    screenWorkspace 1 >>= flip whenJust (windows . W.view)
    -- Force the second screen to "10", e.g. if the first screen already has
    -- the workspace associated the screens will swap workspaces.
    windows $ W.view "10"
    -- Focus the first screen again.
    screenWorkspace 0 >>= flip whenJust (windows . W.view)


-- ##################################################################
--- Startup
-- ##################################################################

myStartupHook :: X ()
myStartupHook = do

  --  spawnOnce "picom &"                      -- Transparency and stuff
    spawnOnce "setxkbmap es"                   -- Set es keyboard
    spawnOnce "~/.fehbg"                       -- Set the wallpaper 
    spawnOnce "unclutter"                    -- Remove mouse when idle
    spawnOnce "xsetroot -cursor_name left_ptr" -- Change cursor outside windows
    spawnOnce "dunst"                        -- Notifications
    spawnOnce "stalonetray"                    -- System tray
    startWindowSetup                           -- Set screen 2 to workspace 10
    -- spawnOnce "fixStruts.sh"                   -- Fix Struts for xmobar on second monitor. DOES NOT WORK :(

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
--
--screenCornerLayoutHook 

myLayoutHook = avoidStruts $ T.toggleLayouts floats
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
-- clickable s wsIcon = "<action=xdotool key super+" ++ s ++ ">" ++ wsIcon ++ "</action>"
clickable s wsIcon = wsIcon -- not clickable


leftSep :: String
leftSep = "<fn=2><fc=#555555>\xe0b2</fc></fn>"

rightSep :: String
rightSep = "<fn=2><fc=#555555>\xe0b0</fc></fn>"


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
     , className =? "feh"             --> doFloat
     , title =? "Mozilla Firefox"     --> doShift ( "1" )
     , className =? "discord"         --> doShift ( "10" )
     , className =? "Spotify"         --> (className >>= io . appendFile "/home/noutimbaler/xmonad_debug" >> idHook)
     , className =? "Spotify"         --> doShift ( "10" )
     , resource =? "Dialog"           --> doFloat  -- Float Firefox Dialog
     , title ^? "OpenGL"              --> doFloat
     , title ^? "Turtle"              --> doFloat
     , className =? "Threadscope"     --> doFloat
     ] <+> namedScratchpadManageHook myScratchPads






-- ##################################################################
--- Keybindings
-- ##################################################################

myKeys :: [(String, X ())]
myKeys =

--------------------------------------------------------------
    -- Xmonad
        [ ("M-S-r", spawn "restart_xmonad")                             -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                                     -- Quits xmonad
        , ("M-x"  , spawn (text_editor ++ " .xmonad/xmonad.hs") ) 
        , ("M-S-x", spawn (text_editor ++ " .config/xmobar/xmobar.hs") ) 
--------------------------------------------------------------

    -- Non Greedy Workspaces
        , ("M-1",   changeWindow "1")
        , ("M-2",   changeWindow "2")
        , ("M-3",   changeWindow "3")
        , ("M-4",   changeWindow "4")
        , ("M-5",   changeWindow "5")
        , ("M-6",   changeWindow "6")
        , ("M-7",   changeWindow "7")
        , ("M-8",   changeWindow "8")
        , ("M-9",   changeWindow "9")
        
    -- Make 2nd screen workspace work properly (workspace 10)
        , ("M-0",   windows $ W.view "10")
        , ("M-S-0", windows $ W.shift "10")


--------------------------------------------------------------
    -- Dmenu Scripts
        , ("M-p",   spawn (power_menu))    -- power menu
        , ("M-m",   spawn "manly")         -- open man page as pdf
        , ("M-c",   spawn "edit_confs.sh") -- conf editor
--------------------------------------------------------------





---------------------------------------------------------------
   -- Useful programs to have a keybinding for launch
        , ("M-<Return>", spawn (terminal_emulator))
        , ("M-w",        spawn (web_browser))
        , ("M-S-w",      spawn (network_tool))  -- network tool
        , ("M-d",        spawn "dmenu_run -fn '-15' -c -l 5 -bw 3  -m 0 -p \"Run command:\"")
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
        , ("M-<Tab>",       promote)                -- Moves focused window to master, others maintain order
        , ("M-<Backspace>", rotAllDown)             -- Rotate all the windows in the current stack
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
        , ("<XF86MonBrightnessUp>",   spawn "xbacklight -inc 5")
        , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")

        -- Misc.
        , ("<XF86HomePage>", spawn (web_browser ++ " https://gitlab.com/vojjvoda") )
        , ("<XF86Mail>",     spawn (web_browser ++ " https://protonmail.com") )

        , ("<Print>",           spawn "maim -s | xclip -selection clipboard -t image/png")
        , ("M-<Print>",         spawn "screenshot.sh")
        ]
--------------------------------------------------------------

-- helper for changing windows
changeWindow :: String -> X()
changeWindow i = do
    screenWorkspace 0 >>= flip whenJust (windows . W.view)
    windows $ W.view i

          

split :: String -> Char -> [String]
split "" _ = []
split xs c = let (ys, zs) = break (== c) xs
             in  if null zs then [ys] else ys : split (tail zs) c

join :: [String] -> String
join [] = ""
join (x:xs) = x ++ join xs


-- ##################################################################
--- Main func, also xmobar handling
-- ##################################################################


main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobar.hs"
    -- xmproc1 <- spawnPipe "xmobar1 -x 0 $HOME/.config/xmobar/xmobar1.hs"
    xmonad $ docks $ ewmh def
        { manageHook         = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        , modMask            = mod_mask
        , terminal           = terminal_emulator
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook -- showWName' myShowWNameTheme $ myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = border_width
        , normalBorderColor  = normal_w_color
        , focusedBorderColor = focused_w_color
        , logHook = dynamicLogWithPP $ filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP -- $ fadeInactiveLogHook 0.8

        -- settings for xmobar.
              { ppOutput          = \s -> do
                                            let a = split s '^'
                                            let ws = a !! 0
                                            let lay = a !! 1
                                            let tit = (join . tail . tail) a
                                            hPutStrLn xmproc0 ( ws ++ lay )
--                                            hPutStrLn xmproc1 tit

              , ppCurrent         = \s -> clickable s $ xmobarColor "#FFFFFF" "#555555" "\xf111 "
              , ppVisible         = \s -> clickable s $ xmobarColor "#AAAAAA" "#555555" "\xf111 "
              , ppHidden          = \s -> clickable s $ xmobarColor "#FFFFFF" "#555555" "\xf192 "
              , ppHiddenNoWindows = \s -> clickable s $ xmobarColor "#FFFFFF" "#555555" "\xf10c "
              , ppUrgent          = \s -> clickable s $ xmobarColor "#000000" "#555555" "\xf111 "

              , ppLayout          = \s -> xmobarColor "#BBBBBB" "#555555" (wrap "<fn=1> | " " </fn>" s)
              , ppTitle           = \s -> xmobarColor "#FFFFFF" "#555555:0" (" " ++ s)
              , ppSep             = "^"
              , ppWsSep           = " "

              , ppOrder           = \(ws:l:t:ex) -> [ws,l] ++ [t] -- order of things in xmobar : workspaces, layout, title
              }
        } `additionalKeysP` myKeys

