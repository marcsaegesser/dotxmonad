import System.IO
import System.Posix.Env

import XMonad
import XMonad.Core
import XMonad.Config
import XMonad.Operations

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Util.Scratchpad

import XMonad.Actions.Volume
import XMonad.Actions.CopyWindow

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import Data.List

myTerminal = "xterm"

myModMask = mod4Mask

myManageHook = composeAll
	[  className =? "stalonetray"	--> doIgnore
	,  className =? "trayer"		--> doIgnore
	,  className =? "Pidgin" <&&> title=? "Buddy List" --> doFloat <+> doShift "float"
	,  className =? "Pidgin"		--> doFloat
	,  className =? "Gimp"			--> doFloat <+> doShift "8"
	,  className =? "TweetDeck" <&&> stringProperty "WM_ICON_NAME" =? ""	--> doFloat <+> doIgnore
	,  className =? "TweetDeck" <&&> title =? "TweetDeck Settings" --> doFloat
	,  className =? "TweetDeck"		--> doShift "tweet"
	,  className =? "Zimbra Desktop" <&&> stringProperty "WM_NAME" =? "Downloads"	--> doFloat
	,  className =? "Zimbra Desktop" --> doShift "mail"
	,  className =? "127.0.0.1__desktop_login.jsp" --> doShift "mail"
	,  fmap ( "Emacs" `isInfixOf` ) className --> doShift "emacs"
	,  fmap ( "Skype" `isInfixOf` ) className --> doFloat <+> doShift "float"
	,  className =? "Eclipse" --> doShift "code"
	,  className =? "XTerm" <&&> title =? "Emacs" --> doShift "emacs"
	,  fmap ( "LibreOffice" `isInfixOf` ) (stringProperty "WM_NAME") --> doShift "office"
	,  scratchpadManageHookDefault
	]

myWorkspaces = [ "web", "emacs", "code", "vm", "office", "float", "7", "8", "9", "0"]

--keybindings for adjusting the non-Master tiles' size
resizeKeys conf@(XConfig {XMonad.modMask = modm}) =
	[  ((modm, xK_a), sendMessage MirrorShrink)
	,  ((modm, xK_z), sendMessage MirrorExpand)
	]

--keybindings for handling urgency hints
urgentKeys conf@(XConfig {XMonad.modMask = modm}) =
	[  ((modm, xK_BackSpace), focusUrgent)
	,  ((modm .|. shiftMask, xK_BackSpace), clearUrgents)
	]

screensaverKeys conf@(XConfig {XMonad.modMask = modm}) =
	[  ((modm, xK_F12), spawn "xscreensaver-command -lock") 
	]

killWindowsKey conf@(XConfig {XMonad.modMask = modm}) =
	[  ((0, xK_Super_L), return()) 
	]

copyWindowKeys conf@(XConfig {XMonad.modMask = modm}) =
	[ ((m .|. modm, k), windows $ f i)
		| (i, k) <- zip (workspaces conf) [ xK_1 ..]
		, (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
	]

copyWindowKeys' conf@(XConfig {XMonad.modMask = modm}) =
	[  ((modm .|. shiftMask, xK_c ), kill1)
	,  ((modm, xK_v ), windows copyToAll)
	,  ((modm .|. shiftMask, xK_v ), killAllOtherCopies )
	]

scratchPadKeys conf@(XConfig {XMonad.modMask = modm}) =
	[  ((mod1Mask, xK_F2),	scratchpadSpawnAction conf)
	]

volumeKeys conf@(XConfig {XMonad.modMask = modm}) =
	[  ((modm, xK_F5), lowerVolume 4 >> return ())
	,  ((modm, xK_F6), raiseVolume 4 >> return ())
	,  ((0, 0x1008ff11), lowerVolume 4 >> return ())
	,  ((0, 0x1008ff13), raiseVolume 4 >> return ())
	,  ((0, 0x1008ff12), toggleMute >> return ())
	,  ((modm, xK_F11), spawn "tools/restart-xcompmgr.sh")
	]

--merge all the keybindings together.
myKeys x = M.unions [ (keys defaultConfig x)
					, (M.fromList (resizeKeys x))
					, (M.fromList (urgentKeys x))
					, (M.fromList (screensaverKeys x))
					, (M.fromList (copyWindowKeys x))
					, (M.fromList (copyWindowKeys' x))
					, (M.fromList (scratchPadKeys x))
					, (M.fromList (killWindowsKey x))
					, (M.fromList (volumeKeys x))
					, (keys defaultConfig x)
					]

myStartupHook = setDefaultCursor xC_left_ptr


myFont = "Inconsolata"
myFgColor = "#DCDCCC"
myBgColor = "#3f3f3f"
myStatusBar = "xmobar /home/msaegesser/.xmobarrc"

myLayoutHook = avoidStruts $ standardLayouts
               where standardLayouts = tiled ||| Mirror tiled ||| Full
                     tiled = ResizableTall nmaster delta ratio []
                     nmaster = 1 
                     delta = 0.03
                     ratio = 0.6

--myLogHook :: Handle -> X ()
myLogHook myStatusBarPipe =  do
	copies <- wsContainingCopies
	let check ws | ws `elem` copies = pad . xmobarColor "red" "black" $ ws
				 | otherwise = ws
	dynamicLogWithPP $ xmobarPP { ppHidden = check
	  				, ppOutput = hPutStrLn myStatusBarPipe
				 	, ppUrgent = xmobarColor "white" "red"
					, ppTitle = xmobarColor "purple" "" . shorten 120
	  			    }
	fadeInactiveLogHook fadeAmount
	    where fadeAmount = 0.6

main = do
--	setEnv "KDE_FULL_SESSION" "true" True
--	setEnv "KDE_SESSION_VERSION" "4" True
	myStatusBarPipe <- spawnPipe myStatusBar
	spawn "feh --bg-scale `feh -U -z Pictures/IMG_4113-new4.jpg | head -1`"
	spawn "kwalletd"
	spawn "trayer --edge bottom --align right --height 16 --width 192 --widthtype pixel --transparent true"
	spawn "wicd-client -t"
	spawn "xscreensaver -no-splash"
	spawn "xcompmgr -cfCF -D 10"
	spawn "dropbox start"
	spawn "synclient PalmDetect=1 MinSpeed=0.3 MaxSpeed=0.3 AccelFactor=0"
	spawn "syndaemon -t -d"
	spawn "xterm -title Emacs"
        spawn "emacs"
	spawn "pidgin"
	spawn "skype"
	xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
		{ terminal		= myTerminal,
		  workspaces	= myWorkspaces,
	  	  modMask		= myModMask,
		  layoutHook	= myLayoutHook,
		  manageHook	= myManageHook,
		  keys			= myKeys,
		  startupHook	= myStartupHook,
		  logHook		= myLogHook myStatusBarPipe
		}
