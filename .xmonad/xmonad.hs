import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.FloatKeys

import XMonad.Config.Gnome

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.IM

import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad

import Solarized
import System.IO


main = do
  xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobar.hs"
  xmonad $ gnomeConfig
                      { normalBorderColor = solarizedBase01
                      , focusedBorderColor = solarizedRed
                      , workspaces = myWorkspaces
                      , modMask = mod4Mask
                      , terminal = myTerminal
                      , manageHook = manageDocks <+> manageHook defaultConfig
                      , layoutHook = avoidStruts $ layoutHook defaultConfig
                      , logHook = dynamicLogWithPP myXmobarPP
                                                           { ppOutput = hPutStrLn xmproc
                                                           , ppTitle = xmobarColor solarizedBase0 solarizedBase03 . shorten 50
                                                           }
                      }
                      `additionalKeysP` myKeys

myTerminal = "gnome-terminal"

myXmobarPP = defaultPP { ppCurrent = xmobarColor solarizedBlue "" . wrap "[" "]"
                       , ppTitle = xmobarColor solarizedBase1 "" . shorten 40
                       , ppVisible = wrap "(" ")"
                       , ppUrgent = xmobarColor solarizedRed solarizedBase02
                       }

myWorkspaces =
  [ "1:info"
  , "2:comm"
  , "3:term"
  , "4"
  , "5"
  , "6"
  , "7"
  , "8:tsrv"
  , "9:scratch"
  ]

myKeys =
  [ ("M-S-q", spawn "gnome-session-quit --logout --no-prompt")
  , ("M-S-l", spawn "gnome-screensaver-command -l")
  , ("M-S-<Space>", spawn "dmenu_run")
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-s", scratchpadSpawnActionCustom "gnome-terminal --disable-factory --name scratchpad")
  ]
