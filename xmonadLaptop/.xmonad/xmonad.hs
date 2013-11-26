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
  customConfig <- myConfig
  spawn "killall urxvtd; urxvtd -q -f -o" >>  xmonad customConfig

myConfig = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/halvor/.xmobarc.hs"
  return $ defaultConfig {
               modMask = mod4Mask,
               terminal = "urxvtc",
               XMonad.workspaces = myWorkspaces,
               manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig,
               --      startupHook = startup <+> startupHook defaultConfig,
               layoutHook = avoidStruts $ smartBorders $ desktopLayoutModifiers (tall ||| Full),
               logHook = dynamicLogWithPP xmobarPP
               { ppOutput = hPutStrLn xmproc,
                 ppTitle = xmobarColor "orange" "" . shorten 50
               },
               borderWidth = 4,
               normalBorderColor = "#cccccc",
               focusedBorderColor = "#eb8f00",
               focusFollowsMouse = True -- prøver å bytte
             } `additionalKeys`
             [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock"),

               ((mod4Mask, xK_b), sendMessage ToggleStruts),
               ((mod4Mask, xK_c), spawn "emacs"),
               ((mod4Mask, xK_d), spawn "chromium"),
               ((mod4Mask, xK_w), kill),
--               ((mod4Mask .|. controlMask,  xK_q), spawn "gnome-session-quit"),
               ((mod4Mask, xK_r), spawn "dmenu_run"),
               -- terminal apps:
               -- screen
               ((0, xK_Print), spawn "urxvtc -e scrot '%Y-%m-%d_$wx$h_scrot.png' -e 'mv $f ~/screens/' "),
               -- task mgr
               ((mod1Mask .|. controlMask, xK_Delete), spawn "urxvtc -e htop"),
               -- irssi
               ((mod4Mask, xK_i), spawn "urxvtc -e irssi"),

               -- Audio
               ((mod4Mask, xK_a), spawn "urxvtc -e alsamixer -c 1"),
               ((0, 0x1008ff13), spawn "amixer -c 1 set Master 4+"),
               ((0, 0x1008ff11), spawn "amixer -c 1 set Master 4-"),
               ((0, 0x1008ff12), spawn "amixer -c 1 set Master 0db"),

               -- Brightness
               ((0, 0x1008ff02), spawn "xbacklight -inc 10"),
               ((0, 0x1008ff03), spawn "xbacklight -dec 10")


             ]

myWorkspaces = ["web", "emacs", "term"] ++ map show ([4..8] :: [Int]) ++ ["irssi"]

--startup = do
--  spawn "synaptikscfg load"
--  spawn "synaptikscfg init"
tall = Tall 1 (3/100) (1/2)
