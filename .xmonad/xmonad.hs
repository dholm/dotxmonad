import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Solarized

import System.IO
import System.Taffybar.Hooks.PagerHints (pagerHints)

import Text.Printf

import XMonad

import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect

import XMonad.Config.Gnome (gnomeConfig)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Mosaic
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad


myBaseConfig = gnomeConfig
myTerminal = "gnome-terminal"
myWorkspaces = [ "1:web"
               , "2:mail"
               , "3:comm"
               , "4:term"
               , "5"
               , "6"
               , "7"
               , "8:tsrv"
               , "9:emacs"
               ]
myNormalBorderColor = solarizedBase01
myFocusedBorderColor = solarizedRed

mprisPlay = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.%s /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
mprisPrev = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.%s /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
mprisNext = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.%s /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"

myKeys =
  [ -- Launching and managing applications
    ("M-S-q", spawn "gnome-session-quit --logout --no-prompt")
  , ("M-S-l", spawn "xscreensaver-command -lock")
  , ("M-<Space>", spawn "synapse")
  , ("M-S-<Space>", spawn "gmrun")
  , ("M-s", scratchpadSpawnActionCustom "gnome-terminal --disable-factory --name scratchpad")
  , ("M-r", spawn "killall -9 taffybar-linux-x86_64; xmonad --recompile && xmonad --restart")
  , ("M-<Tab>", goToSelected defaultGSConfig)

    -- Modifying the layout
  , ("M-S-<Space>", sendMessage NextLayout)
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-,", sendMessage (IncMasterN 1))
  , ("M-.", sendMessage (IncMasterN (-1)))
  , ("M-b", sendMessage ToggleStruts)

    -- Changing focus
  , ("M-m", windows W.focusMaster)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)

   -- Modifying windows
  , ("M-q", kill)
  , ("M-S-c", withFocused forceKill)
  , ("M-n", refresh)

    -- Changing the window order
  , ("M-S-m", windows W.swapMaster)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)

    -- Resizing the master/slave ratio
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)

    -- Refresh the screen
  , ("M-r", rescreen)

    -- Manage video output
  , ("<XF86LaunchA>", spawn "disper --cycle-stages \"-S:-c:-s:-e\" -C")

    -- Multimedia keys
  , ("<XF86AudioMute>", spawn "amixer -q -D pulse set Master toggle")
  , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 2dB- unmute")
  , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 2dB+ unmute")
  , ("<XF86AudioPlay>", spawn (printf mprisPlay "mopidy"))
  , ("<XF86AudioPrev>", spawn (printf mprisPrev "mopidy"))
  , ("<XF86AudioNext>", spawn (printf mprisNext "mopidy"))
  ]
  ++
  -- Managing workspaces
  [ (mask ++ "M-" ++ [key], windows $ f i)
    | (i, key) <- zip myWorkspaces ['1' .. '9']
    , (f, mask) <- [ (W.greedyView, ""), (W.shift, "S-") ]
  ]
  ++
  -- Managing physical screens
  [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
    | (key, scr) <- zip "wer" [1, 2, 3]
    , (action, mask) <- [ (W.view, ""), (W.shift, "S-") ]
  ]

myLayoutHook = onWorkspace "4:term" htiled $
               onWorkspace "3:comm" pidginLayout $
               onWorkspace "8:tsrv" (noBorders Full) $
               avoidStruts $ toggleLayouts (noBorders Full)
               standardLayouts
               where
                 standardLayouts = Full ||| tiled ||| mosaic 2 [3,2] ||| Mirror tiled
                 tiled = ResizableTall nmaster delta ratio []
                 htiled = avoidStruts $ Tall hmaster delta ratio
                 hmaster = 8
                 nmaster = 1
                 delta = 2 / 100
                 ratio = 1 / 2
                 gridLayout = spacing 8 Grid
                 pidginLayout = avoidStruts $ withIM (18/100) pidginRoster $ reflectHoriz $
                                withIM (18/100) skypeRoster standardLayouts
                 pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"
                 skypeRoster  = ClassName "Skype"  `And` Role "MainWindow"

forceKill :: Window -> X ()
forceKill w = withDisplay $ \d -> io $ do
  killClient d w
  return ()

myManageHook :: ManageHook
myManageHook = composeAll
               [ isFullscreen --> doFullFloat
               , isDialog --> doFloat
               , className =? "sun-awt-X11-XFramePeer" --> doFloat
               , className =? "Firefox" <&&> fmap not isFullscreen --> doShift "1:web"
               , className =? "Mail" --> doShift "2:mail"
               , className =? "Pidgin" --> doShift "3:comm"
               , className =? "Emacs" --> doShift "9:emacs"
               ]

myXmobarPP = defaultPP { ppCurrent = xmobarColor solarizedBlue "" . wrap "[" "]"
                       , ppTitle = xmobarColor solarizedBase1 "" . shorten 50
                       , ppVisible = wrap "(" ")"
                       , ppUrgent = xmobarColor solarizedRed solarizedBase02
                       }

main = xmonad . pagerHints . ewmh $ myBaseConfig
                      { normalBorderColor = myNormalBorderColor
                      , focusedBorderColor = myFocusedBorderColor
                      , layoutHook = myLayoutHook
                      , workspaces = myWorkspaces
                      , modMask = mod4Mask
                      , terminal = myTerminal
                      , handleEventHook = fullscreenEventHook
                      , manageHook = manageDocks <+> myManageHook
                                     <+> manageHook myBaseConfig
                      , logHook = dynamicLogWithPP myXmobarPP
                                                           { ppTitle = xmobarColor solarizedBase0 solarizedBase03 . shorten 50
                                                           }
                      , startupHook = startup
                      , focusFollowsMouse = True
                      }
                      `additionalKeysP` myKeys

startup :: X ()
startup = do
  startupHook gnomeConfig
  spawn "${HOME}/.xmonad/startup-hook.sh"
