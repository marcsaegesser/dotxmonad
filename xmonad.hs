{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Exit
import Control.Monad

import Data.Map                        (fromList, union)

import XMonad

import qualified XMonad.Actions.CycleWS as CWS

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Mosaic

import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce

import XMonad.Actions.Warp
import XMonad.Actions.Volume
import XMonad.Actions.CopyWindow
import XMonad.Actions.SpawnOn

import qualified XMonad.StackSet as W

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.ConfirmPrompt

import DBus
import DBus.Client

-- equivalent to spawn "dbus-send --print-reply --dest=com.spotify.qt /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player" ++ cmd
mprisCommand :: String -> X ()
mprisCommand c = catchIO $ do
                     dbusSession <- connectSession
                     _ <- call_ dbusSession (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" (memberName_ c))
                         { methodCallDestination = Just "org.mpris.MediaPlayer2.spotify" }
                     disconnect dbusSession


main :: IO ()
main = do
  myStatusBarPipe <- spawnPipe "xmobar"
  xmonad $ docks $ withUrgencyHook NoUrgencyHook $ def
    { terminal          = "xterm"
    , workspaces        = myWorkspaces
    , modMask           = mod4Mask
    , layoutHook        = myLayoutHook
    , manageHook        = myManageHook <+> manageSpawn <+> scratchpadManageHookDefault <+> namedScratchpadManageHook myScratchpads
    , keys              = liftM2 union myKeys (keys def)
    , startupHook       = myStartupHook
    , logHook           = myLogHook myStatusBarPipe
    , focusFollowsMouse = False
    }

myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:monospace:size=9"
  }

myManageHook = composeOne
  [ isDialog                     -?> doFloat
  , className =? "trayer"        -?> doIgnore
  , className =? "Skype"         -?> doShift "chat"
  , className =? "Spotify"       -?> doShift "media"  -- I give up. Spotify just won't shift
  , appName   =? "spotify"       -?> doShift "media"  -- I give up. Spotify just won't shift
  , appName   =? "libreoffice"   -?> doShift "office"
  , return True                  -?> doF W.swapDown
  ]

myScratchpads = [
      NS "htop" "xterm -name htop -e htop" (title =? "htop") (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))
    , NS "nautilus" "nautilus --no-desktop" (className =? "Nautilus") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    ]

myWorkspaces = [ "web", "emacs", "chat", "vm", "office", "media", "xterms", "8", "9", "0"]

myKeys conf@(XConfig {XMonad.modMask = modm}) = fromList $
  [ ((modm                , xK_a          ), sendMessage MirrorShrink)
  , ((modm                , xK_z          ), sendMessage MirrorExpand)
  , ((modm                , xK_g          ), warpToWindow (1/2) (1/2))    -- Move pointer to focused window
  , ((modm                , xK_n          ), namedScratchpadAction myScratchpads "nautilus")
  , ((modm                , xK_o          ), namedScratchpadAction myScratchpads "htop")
  , ((modm                , xK_F2         ), scratchpadSpawnAction conf)
  , ((modm .|. controlMask, xK_x          ), shellPrompt def)
  , ((modm                , xK_F5         ), lowerVolume 2 >> return ())
  , ((modm                , xK_F6         ), raiseVolume 2 >> return ())
  , ((modm                , xK_F10        ), spawn "bin/stop-compton.sh")     -- Stop compton (doesn't get as confused as xcompmgr but sometimes still necessary)
  , ((modm                , xK_F11        ), spawn "bin/restart-compton.sh")  -- Restart compton
  , ((modm                , xK_F12        ), spawn "xscreensaver-command -lock")
  , ((modm .|. controlMask, xK_j          ), CWS.nextWS)    -- Cycle through workspaces
  , ((modm .|. controlMask, xK_k          ), CWS.prevWS)
  , ((modm                , xK_BackSpace  ), focusUrgent)     -- Urgency hints
  , ((modm .|. shiftMask  , xK_BackSpace  ), clearUrgents)
  , ((modm                , xK_b          ), sendMessage ToggleStruts)
  , ((modm .|. shiftMask  , xK_q          ), confirmPrompt myXPConfig "exit" (io exitSuccess))
  , ((modm .|. shiftMask  , xK_c          ), kill1)
  , ((modm                , xK_v          ), windows copyToAll)
  , ((modm .|. shiftMask  , xK_v          ), killAllOtherCopies )
  , ((0                   , xK_Print      ), spawn "scrot")
  , ((0                   , 0x1008ff11    ), lowerVolume 2 >> return ())  -- Keyboard volume down
  , ((0                   , 0x1008ff13    ), raiseVolume 2 >> return ())  -- Keyboard volume up
  , ((0                   , 0x1008ff12    ), toggleMute >> return ())     -- Keyboard volume mute
  , ((0                   , 0x1008ff16    ), mprisCommand "Previous")     -- previous track
  , ((0                   , 0x1008ff14    ), mprisCommand "PlayPause")    -- play
  , ((0                   , 0x1008ff17    ), mprisCommand "Next")         -- next track
  ]
  ++
  [ ((m .|. modm, k), windows $ f i)    -- Shift/Copy window
  | (i, k) <- zip myWorkspaces  $ [ xK_1..xK_9 ] ++ [ xK_0 ]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
  ]


-- | Shifts a window to the specified workspace and then moves it down the stack.
-- Useful for ordering windows from the StartupHook in a natural way.
shiftAndDown w = doF W.swapDown <+> doShift w

myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawnOnce "feh --bg-scale `feh -U -z /home/msaegesser/Pictures/B1bApproach.jpg | head -1`"
  spawnOnce "trayer --edge bottom --align right --height 19 --width 7 --widthtype percent --transparent true"
  spawnOnce "xscreensaver -no-splash"
  spawnOnce "autocutsel -fork"
  spawnOnce "autocutsel -selection PRIMARY -fork"
  spawnOnce "SpiderOakONE"
  spawnOnce "ssh-add ~/.ssh/id_rsa"
  spawnOnce "ssh-add ~/.ssh/git_rsa"
  spawnOnce "compton --backend glx"
  -- spawn*Once functions are an unreleased experiment
  spawnOnOnce "web" "google-chrome"
  spawnAndDoOnce (shiftAndDown "web") "google-chrome --new-window news.google.com"
  spawnOnOnce "emacs" "emacs"
  spawnOnOnce "chat" "hipchat4"
  spawnNOnOnce 4 "xterms" "xterm"

myLayoutHook = smartBorders $ avoidStruts $ standardLayouts
  where standardLayouts = tiled ||| mosaic 2 [3,2]  ||| Mirror tiled ||| Full
        tiled = ResizableTall nmaster delta ratio []
        nmaster = 1
        delta = 0.03
        ratio = 0.6

myLogHook p =  do
  copies <- wsContainingCopies
  let check ws | ws == "NSP" = ""                               -- Hide the scratchpad workspace
               | ws `elem` copies = xmobarColor "red" "black" $ ws  -- Workspaces with copied windows are red on black
               | otherwise = ws
  dynamicLogWithPP $ xmobarPP { ppHidden = check
                              , ppOutput = hPutStrLn p
                              , ppUrgent = xmobarColor "white" "red"
                              , ppTitle  = xmobarColor "green" "" . shorten 180
                              }
  fadeInactiveLogHook 0.6

