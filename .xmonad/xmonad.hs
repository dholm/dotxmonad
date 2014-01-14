import XMonad

import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect

import XMonad.Config.Gnome (gnomeConfig)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Mosaic
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad

import Solarized

import System.IO
import System.Taffybar.Hooks.PagerHints (pagerHints)


myBaseConfig = gnomeConfig
myTerminal = "gnome-terminal"
myWorkspaces = [ "1:info", "2:mail", "3:comm", "4:term", "5", "6", "7", "8:tsrv", "9:scratch" ]
myNormalBorderColor = solarizedBase01
myFocusedBorderColor = solarizedRed

myKeys =
  [ -- Launching and managing applications
    ("M-S-q", spawn "gnome-session-quit --logout --no-prompt")
  , ("M-S-l", spawn "gnome-screensaver-command -l")
  , ("M-<Space>", spawn "dmenu_run")
  , ("M-s", scratchpadSpawnActionCustom "gnome-terminal --disable-factory --name scratchpad")
  , ("M-q", spawn "killall -9 taffybar-linux-x86_64; xmonad --recompile && xmonad --restart")
  , ("M-<Tab>", goToSelected defaultGSConfig)

    -- Modifying the layout
  , ("M-S-<Space>", sendMessage NextLayout)
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-,", sendMessage (IncMasterN 1))
  , ("M-.", sendMessage (IncMasterN (-1)))
  , ("M-b", sendMessage $ ToggleStruts)

    -- Changing focus
  , ("M-m", windows W.focusMaster)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)

    -- Changing the window order
  , ("M-S-m", windows W.swapMaster)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)

    -- Resizing the master/slave ratio
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)

    -- Refresh the screen
  , ("M-r", rescreen)
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
               ( Full ||| tiled ||| mosaic 2 [3,2] ||| Mirror tiled)
               where
                 tiled = ResizableTall nmaster delta ratio []
                 htiled = avoidStruts $ Tall hmaster delta ratio
                 hmaster = 8
                 nmaster = 1
                 delta = 2 / 100
                 ratio = 1 / 2
                 gridLayout = spacing 8 $ Grid
                 pidginLayout = withIM (18/100) (Role "buddy_list") gridLayout

myManageHook = composeAll
               [ className =? "Pidgin" --> doShift "3:comm"
               ]

myXmobarPP = defaultPP { ppCurrent = xmobarColor solarizedBlue "" . wrap "[" "]"
                       , ppTitle = xmobarColor solarizedBase1 "" . shorten 50
                       , ppVisible = wrap "(" ")"
                       , ppUrgent = xmobarColor solarizedRed solarizedBase02
                       }

main = do
  xmproc <- spawn "taffybar"
  xmonad . pagerHints . ewmh $ myBaseConfig
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
                      , focusFollowsMouse = True
                      }
                      `additionalKeysP` myKeys
