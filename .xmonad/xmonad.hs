import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.ManageHelpers
import System.IO
import Data.Default(def)
import XMonad.Hooks.EwmhDesktops
import Graphics.X11.ExtraTypes.XF86

myModMask = mod1Mask -- Bind Mod to the left alt key
-- myModMask = mod4Mask -- Bind Mod to the windows key

myTabConfig = def { inactiveBorderColor = "#928374"
		    , inactiveColor = "#282828"
	            , activeTextColor = "#689d6a"
		    , activeBorderColor = "#689d6a"
		    , activeColor = "#282828"
	            , urgentTextColor = "red"
	            , decoHeight = 20 }

myLayout = (tabbed shrinkText myTabConfig) ||| tiled ||| Full
  where 
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myManageHook = composeAll [
        className =? "Spotify" --> doShift "3:music",
        className =? "pavucontrol" --> doShift "3:music",
	manageDocks,
	(isFullscreen --> doFullFloat),
	manageHook def
	]

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ def
        { manageHook = myManageHook
	, layoutHook = (smartBorders . avoidStruts) myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#ebdbb2" "" . shorten 50
                        , ppCurrent = xmobarColor "#689d6a" ""
                        }
        , handleEventHook = handleEventHook def <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook <+> docksEventHook
        , modMask = myModMask
        , borderWidth = 2
        , normalBorderColor = "#928374"
        , focusedBorderColor = "#689d6a"
	, workspaces = ["1:dev","2:web","3:music","4:steam","5","6","7","8","9"]
        } `additionalKeys`
        [ ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((myModMask , xK_p), spawn "rofi -show run")
        , ((0, xF86XK_AudioPlay), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        , ((myModMask, xK_F12), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        , ((0, xF86XK_AudioPrev), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
        , ((myModMask, xK_F10), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
        , ((0, xF86XK_AudioNext), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
        , ((myModMask, xK_F11), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
        , ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 5%-; notify-send -i audio-volume-medium -t 1000 'Volume: '$(amixer -D pulse sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 } ')")
        , ((myModMask, xK_Down), spawn "amixer -D pulse sset Master 5%-; notify-send -i audio-volume-medium -t 1000 'Volume: '$(amixer -D pulse sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 } ')")
        , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 5%+; notify-send -i audio-volume-medium -t 1000 'Volume: '$(amixer -D pulse sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 } ')")
        , ((myModMask, xK_Up), spawn "amixer -D pulse sset Master 5%+; notify-send -i audio-volume-medium -t 1000 'Volume: '$(amixer -D pulse sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 } ')")
        ]
