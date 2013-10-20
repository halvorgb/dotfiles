import XMonad hiding( (|||) )
import XMonad.Layout.LayoutCombinators
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Config.Desktop(desktopLayoutModifiers)

import XMonad.StackSet
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import System.IO

myManageHook = composeAll
               [ isFullscreen --> doFloat ]


main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/halvor/.xmobarc.hs"
  xmonad $ defaultConfig
    { modMask = mod4Mask,
      manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig,
      startupHook = startup <+> startupHook defaultConfig,
      layoutHook = avoidStruts $ smartBorders $ desktopLayoutModifiers (tall ||| Full),
      logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc,
                  ppTitle = xmobarColor "green" "" . shorten 50
                },
      borderWidth = 4,
      normalBorderColor = "#cccccc",
      focusedBorderColor = "#eb8f00",
      focusFollowsMouse = True -- prøver å bytte
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock"),
      ((0, xK_Print), spawn "gnome-screenshot"),
      ((mod4Mask, xK_b), sendMessage ToggleStruts),
      ((mod4Mask, xK_c), spawn "emacs24"),
      ((mod4Mask, xK_d), spawn "google-chrome"),
      ((mod4Mask, xK_a), spawn "nautilus"),
      ((mod4Mask, xK_w), kill),
      ((mod4Mask .|. controlMask,  xK_q), spawn "gnome-session-quit"),
      ((mod4Mask, xK_r), spawn "dmenu_run"),
      ((mod1Mask .|. controlMask, xK_Delete), spawn "gnome-system-monitor"),

      ((0, 0x1008ff13), spawn "amixer -c 1 set Master 4+"),
      ((0, 0x1008ff11), spawn "amixer -c 1 set Master 4-"),
      ((0, 0x1008ff12), spawn "amixer -c 1 set Master 0db"),


      ((0, 0x1008ff02), spawn "amixer -c 1 set Master 2-"),
      ((0, 0x1008ff03), spawn "amixer -c 1 set Master 2-")

    ]

workspaces = ["web", "emacs", "term"] ++ map show ([4..8] :: [Int]) ++ ["irssi"]

startup = do
  spawn "synaptikscfg load"
  spawn "synaptikscfg init"
tall = Tall 1 (3/100) (1/2)
