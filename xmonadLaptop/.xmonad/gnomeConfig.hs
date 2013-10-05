import XMonad hiding( (|||) )
import XMonad.Layout.LayoutCombinators
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Config.Gnome
import XMonad.Config.Desktop(desktopLayoutModifiers)

import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders


main = do
  xmonad $ gnomeConfig
    { modMask = mod4Mask,
      layoutHook = smartBorders $ desktopLayoutModifiers (tall ||| Full),
      manageHook = composeAll [ isFullscreen --> doFullFloat, manageHook gnomeConfig ],
      borderWidth = 4,      
      normalBorderColor = "#cccccc",
      focusedBorderColor = "#eb8f00",
      focusFollowsMouse = True -- prøver å bytte
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock"),
      ((0, xK_Print), spawn "gnome-screenshot"),
      ((mod4Mask, xK_c), spawn "emacs24"),
      ((mod4Mask, xK_d), spawn "chromium-browser"),
      ((mod4Mask, xK_a), spawn "nautilus"),
      ((mod4Mask, xK_w), kill),
      ((mod4Mask .|. controlMask,  xK_q), spawn "gnome-session-quit"),
      ((mod4Mask, xK_r), spawn "dmenu_run -l 15")
    ]
    
    
tall = Tall 1 (3/100) (1/2)
