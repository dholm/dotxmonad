import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Config.Gnome
import XMonad.Util.Run (spawnPipe)
import System.IO

import qualified XMonad.StackSet as W

myWorkspaces = ["1:web","2:communication","3","4:terminals","5","6","7","8:ts","9:scratch"]

main = do
  xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobar.hs"
  --trproc <- spawnPipe "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 0 --tint 0x000000 --height 16"
  xmonad $ gnomeConfig {
    workspaces = myWorkspaces,
    modMask = mod4Mask,
    terminal = "gnome-terminal",
    manageHook = manageDocks <+> manageHook defaultConfig,
    layoutHook = avoidStruts $ layoutHook defaultConfig,
    logHook = dynamicLogWithPP xmobarPP {
      ppOutput = hPutStrLn xmproc,
      ppTitle = xmobarColor "blue" "" . shorten 50
      --ppLayout = const "" -- to disable the layout info on xmobar
      }
    } `additionalKeysP`
    [ ("M-S-q", spawn "gnome-session-quit --logout --no-prompt"),
      ("M-S-l", spawn "gnome-screensaver-command -l"),
      ("M-S-<Space>", spawn "dmenu_run"),
      ("M-t", withFocused $ windows . W.sink)
    ]
