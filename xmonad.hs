{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
import System.IO
-- import System.Posix.Env

import Control.Monad
--import Control.Monad                   (liftM2)

import Data.List                       (isInfixOf)
import Data.List.Split
import Data.Map                        (fromList, union, Map)
import Data.Monoid

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

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

import qualified XMonad.StackSet as W

import XMonad.Prompt
import XMonad.Prompt.Shell

import DBus
import DBus.Client

-- equivalent to spawn "dbus-send --print-reply --dest=com.spotify.qt /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player" ++ cmd
mprisCommand :: String -> X ()
mprisCommand cmd = catchIO $ do
                     dbusSession <- connectSession
	             reply <- call_ dbusSession (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" (memberName_ cmd))
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
    , manageHook        = myManageHook
    , keys              = liftM2 union myKeys (keys def)
    , startupHook       = myStartupHook
    , logHook           = myLogHook myStatusBarPipe
    , focusFollowsMouse = False
    }

-- | Shifts a window to the specified workspace and then moves it down the stack.
-- Useful for ordering windows from the StartupHook in a natural way.
shiftAndDown w = doF W.swapDown <+> doShift w

myManageHook :: Query (Endo WindowSet)
myManageHook = composeOne
  [ isDialog                     -?> doFloat
  , className =? "trayer"        -?> doIgnore
  , className =? "Emacs"         -?> doShift "emacs"
  , className =? "HipChat"       -?> doShift "chat"
  , className =? "Skype"         -?> doShift "chat"
  , className =? "Spotify"       -?> doShift "media"  -- I give up. Spotify just won't shift
  , appName   =? "spotify"       -?> doShift "media"  -- I give up. Spotify just won't shift
  , appName   =? "libreoffice"   -?> doShift "office"
  , appName   =* decodeWSName   --?> shiftAndDown
  , return True                  -?> doF W.swapDown
  ] <+> scratchpadManageHookDefault <+> namedScratchpadManageHook myScratchpads

myScratchpads :: [NamedScratchpad]
myScratchpads = [
      NS "htop" "xterm -name htop -e htop" (title =? "htop") (customFloating $ W.RationalRect (1/4) (1/6) (1/2) (2/3))
    , NS "nautilus" "nautilus --no-desktop" (className =? "Nautilus") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    ]

myWorkspaces :: [String]
myWorkspaces = [ "web", "emacs", "chat", "vm", "office", "media", "xterms", "8", "9", "0"]

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = fromList $
  [ ((modm                , xK_a          ), sendMessage MirrorShrink)
  , ((modm                , xK_z          ), sendMessage MirrorExpand)
  , ((modm                , xK_g          ), warpToWindow (1/2) (1/2))    -- Move pointer to focused window
  , ((modm                , xK_n          ), namedScratchpadAction myScratchpads "nautilus")
  , ((modm                , xK_o          ), namedScratchpadAction myScratchpads "htop")
  , ((modm                , xK_F2         ), scratchpadSpawnAction conf)
  , ((modm                , xK_F5         ), lowerVolume 2 >> return ())
  , ((modm                , xK_F6         ), raiseVolume 2 >> return ())
  , ((modm                , xK_F10        ), spawn "bin/stop-compton.sh")  -- Stop compton (doesn't get as confused as xcompmgr but sometimes still necessary)
  , ((modm                , xK_F11        ), spawn "bin/restart-compton.sh")  -- Restart compton
  , ((modm                , xK_F12        ), spawn "xscreensaver-command -lock")
  , ((modm .|. controlMask, xK_j          ), CWS.nextWS)    -- Cycle through workspaces
  , ((modm .|. controlMask, xK_k          ), CWS.prevWS)
  , ((modm                , xK_BackSpace  ), focusUrgent)     -- Urgency hints
  , ((modm .|. shiftMask  , xK_BackSpace  ), clearUrgents)
  , ((modm                , xK_b          ), sendMessage ToggleStruts)
  , ((0                   , xK_Print      ), spawn "scrot")
  , ((0                   , 0x1008ff11    ), lowerVolume 2 >> return ())  -- Keyboard volume down
  , ((0                   , 0x1008ff13    ), raiseVolume 2 >> return ())  -- Keyboard volume up
  , ((0                   , 0x1008ff12    ), toggleMute >> return ())     -- Keyboard volume mute
  , ((0, 0x1008ff16)      , mprisCommand "Previous")    -- previous track
  , ((0, 0x1008ff14)      , mprisCommand "PlayPause")   -- play
  , ((0, 0x1008ff17)      , mprisCommand "Next")        -- next track
  , ((0                   , xK_Super_L    ), return())     -- Eat Windows key to (useful for Windows VMs)
  ]
  ++
  [ ((m .|. modm, k), windows $ f i)    -- Shift/Copy window
  | (i, k) <- zip myWorkspaces  $ [ xK_1..xK_9 ] ++ [ xK_0 ]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
  ]
  ++
  [ ((modm .|. shiftMask, xK_c ), kill1)
  , ((modm              , xK_v ), windows copyToAll)
  , ((modm .|. shiftMask, xK_v ), killAllOtherCopies )
  ]

myStartupHook :: X ()
myStartupHook = do
                setDefaultCursor xC_left_ptr
                spawnOnce "feh --bg-scale `feh -U -z /home/msaegesser/Pictures/B1bApproach.jpg | head -1`"
                spawnOnce "trayer --edge bottom --align right --height 19 --width 7 --widthtype percent --transparent true"
                spawnOnce "xscreensaver -no-splash"
                spawnOnce "autocutsel -fork"
                spawnOnce "autocutsel -selection PRIMARY -fork"
                spawnOnce "SpiderOakONE"
                spawnOnce "google-chrome"
                spawnOnce "google-chrome --new-window news.google.com"
                spawnOnce "emacs"
                spawnOnce "hipchat4"
                spawnOnce "xterm -name ws_xterms_0"
                spawnOnce "xterm -name ws_xterms_1"
                spawnOnce "xterm -name ws_xterms_2"
                spawnOnce "xterm -name ws_xterms_3"
                spawnOnce "ssh-add ~/.ssh/id_rsa"
                spawnOnce "ssh-add ~/.ssh/git_rsa"
                spawnOnce "compton --backend glx"

myLayoutHook = smartBorders $ avoidStruts $ standardLayouts
  where standardLayouts = tiled ||| mosaic 2 [3,2]  ||| Mirror tiled ||| Full
        tiled = ResizableTall nmaster delta ratio []
        nmaster = 1
        delta = 0.03
        ratio = 0.6

myLogHook :: Handle -> X ()
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


decodeWSName v = case splitOn "_" v of ("ws":n:_) -> Just n
                                       _          -> Nothing


infix 0 --->, --?>

-- | A helper for compaseAll and =*. If the contents of the
-- Monad are defined then apply f to value otherwise do nothing.
(--->) :: (Monad m, Monoid a) => m (Maybe b) -> (b -> m a) -> m a
p ---> f = p >>= \mb -> maybe (return mempty) f mb

-- | A helper for compseOne and =*. If the contents of the
-- Monad are defined then apply f to the value otherwise do nothing.
(--?>) :: (Monad m, Monoid a) => m (Maybe b) -> (b -> m a) -> m (Maybe a)
p --?> f = p >>= \mb -> sequence $ fmap f mb

-- | Similar to =? but rather than checking for equality applies a
-- function to the value inside the Monad to a Maybe.
-- Combine with ---> or --?> to excute the Monad (Query) if
-- it's contents are defined.
(=*) :: (Monad m) => m a -> (a -> Maybe b) -> m (Maybe b)
q =* f = fmap f q
