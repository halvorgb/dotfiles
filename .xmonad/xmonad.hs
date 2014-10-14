import XMonad hiding( (|||) )
import XMonad.Layout.LayoutCombinators
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Config.Desktop(desktopLayoutModifiers)

import XMonad.StackSet
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect
import XMonad.Layout.Accordion
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)

import Control.Monad
import Graphics.X11.Xinerama
import Control.Monad.Writer
import Control.Applicative
import Data.Monoid
import Data.Traversable(traverse)
import Data.Maybe
import Data.List
import System.IO

myManageHook = composeAll
               [ isFullscreen --> doFloat ]


main = do
  customConfig <- myConfig -- =<< mapM xmobarScreen =<< getScreens
  xmonad customConfig

myConfig = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/halvor/.xmobarc.hs"
  return $ defaultConfig {
               modMask = mod4Mask,
               terminal = "urxvtc",
               XMonad.workspaces = myWorkspaces,
               manageHook = manageSpawn <+> manageDocks <+> myManageHook <+> manageHook defaultConfig,
               layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig,  -- $ desktopLayoutModifiers (tall ||| Full),
               logHook = dynamicLogWithPP xmobarPP
               { ppOutput = hPutStrLn xmproc,
                 ppCurrent = xmobarColor "#B7F200" "" ,
                 ppVisible = xmobarColor "#1CA4B9" "" ,
                 ppHidden = xmobarColor "#779E00" "" ,
                 ppTitle = xmobarColor "#FD0A7A" "" . shorten 125,
                 ppLayout = xmobarColor "#FFFF00" "" . take 1,
                 ppSep = " ",
                 ppOrder = \(ws:l:t:_) -> [l,ws,t]
               },
               --      startupHook = startup <+> startupHook defaultConfig,
               startupHook = myStartupHook,
               borderWidth = 2,
               normalBorderColor = "#cccccc",
               focusedBorderColor = "#eb8f00",
               focusFollowsMouse = True -- prøver å bytte
             } `additionalKeys`
             [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command --lock"),

               ((mod4Mask, xK_q), sendMessage ToggleStruts), -- remove when editing. just sick of misclicking this key.
               ((mod4Mask, xK_b), sendMessage ToggleStruts),
               ((mod4Mask, xK_c), spawn "emacs"),
               ((mod4Mask, xK_d), spawn "chromium"),
               ((mod4Mask, xK_a), spawn "nautilus"),
               ((mod4Mask, xK_m), spawn "audacious"),
               ((mod4Mask, xK_f), spawn "/home/halvor/.calc.sh"),
               ((mod4Mask, xK_w), kill),
               ((mod4Mask, xK_v), spawn dmenuLaunchString),


               -- dual screen shennanigans
               ((mod4Mask, xK_e), prevScreen),
               ((mod4Mask, xK_r), nextScreen),
               ((shiftMask .|. mod4Mask, xK_e), shiftPrevScreen),
               ((shiftMask .|. mod4Mask, xK_r), shiftNextScreen),




               -- terminal apps:
               -- screen
               ((0, xK_Print), spawn "urxvtc -e scrot '%Y-%m-%d_$wx$h_scrot.png' -e 'mv $f ~/screens/' "),
               -- task mgr
               ((mod1Mask .|. controlMask, xK_Delete), spawn "urxvtc -e htop"),
               -- irssi
               ((mod4Mask, xK_i), spawn "urxvtc -e irssi"),
               -- slack
               ((mod4Mask .|. shiftMask, xK_i), spawn "chromium --app=http://hooplab.slack.com"),

               ((mod4Mask, xK_g), goToSelected defaultGSConfig),


               -- Audio
               ((0, 0x1008ff13), spawn "amixer set Master 4+"),
               ((0, 0x1008ff11),  spawn "amixer set Master 4-"),
               ((0, 0x1008ff12),  spawn "amixer set Master toggle"),

               -- Brightness
               ((0, 0x1008ff02), spawn "xbacklight -inc 10"),
               ((0, 0x1008ff03), spawn "xbacklight -dec 10")


             ]

myWorkspaces = ["web", "emacs", "term"] ++ map show ([4..7] :: [Int]) ++ ["music", "chat"]

myStartupHook = do
  spawn "killall urxvtd; urxvtd -q -f -o;"
  spawnOn "chat" "chromium --app=http://p15ntnu.slack.com"
  spawnOn "emacs" "emacs"
  spawnOn "web" "chromium"


tall = Tall 1 (3/100) (1/2)

dmenuLaunchString = "dmenu_run -b -i -l 10 -p \">\" -fn \"-*-dejavu sans mono-medium-r-*-*-20-*-*-*-*-*-*-*\""

getScreens :: IO [Int]
getScreens = openDisplay "" >>= liftA2 (<*) f closeDisplay
    where f = fmap (zipWith const [0..]) . getScreenInfo


multiPP :: (PP -> X String) -> PP -> PP -> [Handle] -> X ()
multiPP dynlStr focusPP unfocusPP handles = do
  state <- get
  let pickPP :: WorkspaceId -> WriterT (Last XState) X String
      pickPP ws = do
        let isFoc = (ws ==) . W.tag . W.workspace . W.current $ windowset state
        put state { windowset = W.view ws $ windowset state }
        out <- lift $ dynlStr $ if isFoc
                                then focusPP
                                else unfocusPP
        when isFoc $ get >>= tell . Last . Just
        return out
  traverse put . getLast
    =<< execWriterT . (io . zipWithM_ hPutStrLn handles <=< mapM pickPP) . catMaybes
    =<< mapM screenWorkspace (zipWith const [0..] handles)

  return ()


mergePPOutputs :: [PP -> X String] -> PP -> X String
mergePPOutputs x pp = fmap (intercalate (ppSep pp)) . sequence . sequence x $ pp
