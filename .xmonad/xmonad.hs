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

-- myModMask = mod1Mask -- Bind Mod to the left alt key
myModMask = mod4Mask -- Bind Mod to the windows key

greyColor = "#928374"
greyColor2 = "#282828"
greenColor = "#689d6a"
yellowColor = "#ebdbb2"

myTabConfig = def { inactiveBorderColor = greyColor
                    , inactiveColor = greyColor2
                    , activeTextColor = greenColor
                    , activeBorderColor = greenColor
                    , activeColor = greyColor2
                    , urgentTextColor = "red"
                    , decoHeight = 20 }

myLayout = (tabbed shrinkText myTabConfig) ||| tiled ||| Full
  where 
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myManageHook = composeAll [
        className =? "spotify" --> doShift "3:music",
        className =? "pavucontrol" --> doShift "3:music",
        className =? "steam" --> doShift "4:games",
        (isFullscreen --> doFullFloat),
        manageDocks,
        manageHook def
        ]

audioPlayPauseCommand = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
audioPreviousCommand = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
audioNextCommand = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
audioLowerVolumeCommand = "amixer -D pulse sset Master 5%-; notify-send -i audio-volume-medium -t 1000 'Volume: '$(amixer -D pulse sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 } ')"
audioRaiseVolumeCommand = "amixer -D pulse sset Master 5%+; notify-send -i audio-volume-medium -t 1000 'Volume: '$(amixer -D pulse sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 } ')"

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ def
        { manageHook = myManageHook
        , layoutHook = (smartBorders . avoidStruts) myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor yellowColor "" . shorten 50
                        , ppCurrent = xmobarColor greenColor ""
                        }
        , handleEventHook = handleEventHook def <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook <+> docksEventHook
        , modMask = myModMask
        , borderWidth = 2
        , normalBorderColor = greyColor
        , focusedBorderColor = greenColor
        , workspaces = ["1:dev","2:web","3:music","4:games","5","6","7","8","9"]
        } `additionalKeys`
        [ ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((myModMask , xK_p), spawn "rofi -show run")
        , ((0, xF86XK_AudioPlay), spawn audioPlayPauseCommand)
        , ((myModMask, xK_F12), spawn audioPlayPauseCommand)
        , ((0, xF86XK_AudioPrev), spawn audioPreviousCommand)
        , ((myModMask, xK_F10), spawn audioPreviousCommand)
        , ((0, xF86XK_AudioNext), spawn audioNextCommand)
        , ((myModMask, xK_F11), spawn audioNextCommand)
        , ((0, xF86XK_AudioLowerVolume), spawn audioLowerVolumeCommand)
        , ((myModMask, xK_Down), spawn audioLowerVolumeCommand)
        , ((0, xF86XK_AudioRaiseVolume), spawn audioRaiseVolumeCommand)
        , ((myModMask, xK_Up), spawn audioRaiseVolumeCommand)
        ]
