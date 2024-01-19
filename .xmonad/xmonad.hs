import Data.Char (toLower)
import Data.Default (def)
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import MyTheme
import System.IO
import XMonad
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

myModMask = mod4Mask -- Bind Mod to the windows key

myTheme :: MyTheme
myTheme = draculaTheme

fuzzyMatch :: String -> String -> Bool
fuzzyMatch [] _ = True
fuzzyMatch _ [] = False
fuzzyMatch xxs@(x : xs) (y : ys)
  | toLower x == toLower y = fuzzyMatch xs ys
  | otherwise = fuzzyMatch xxs ys

myXPConfig :: XPConfig
myXPConfig =
  def
    { fgColor = (_myTheme_xpConfigFgColor myTheme),
      searchPredicate = fuzzyMatch
    }

myLayoutHook = onWorkspace "9:mon" ((smartBorders . avoidStruts) myLayout') $ ((smartBorders . avoidStruts) myLayout)
  where
    myLayout = tab ||| tiled ||| Accordion
    myLayout' = tiled ||| Accordion ||| tab
    tab = tabbed shrinkText (_myTheme_theme myTheme)
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myManageHook =
  composeAll
    [ className =? "Spotify" --> doShift "3:music",
      className =? "Steam" --> doShift "4:game",
      className =? "mpv" --> doShift "2:vid",
      className =? "vlc" --> doShift "2:vid",
      className =? "Gnome-system-monitor" --> doShift "9:mon",
      className =? "Pavucontrol" --> doShift "9:mon",
      className =? "Chromium-browser" --> doShift "2:vid",
      title =? "Mozilla Firefox" --> doShift "1:web",
      (isFullscreen --> doFullFloat),
      manageDocks,
      manageHook def
    ]

-- audioPlayPauseCommand = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
-- audioPreviousCommand = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
-- audioNextCommand = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
audioPlayPauseCommand = "playerctl play-pause -p spotifyd,spotify,chromium,firefox"

audioPreviousCommand = "playerctl previous -p spotifyd,spotify,chromium,firefox"

audioNextCommand = "playerctl next -p spotifyd,spotify,chromium,firefox"

audioQueryTrackInfoCommand = "notify-send -i media-optical -t 1000 \"$(playerctl metadata artist) - $(playerctl metadata title)\""

audioLowerVolumeCommand = "amixer -D pulse sset Master 5%-; notify-send -i audio-volume-medium -t 1000 'Volume: '$(amixer -D pulse sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 } ')"

audioRaiseVolumeCommand = "amixer -D pulse sset Master 5%+; notify-send -i audio-volume-medium -t 1000 'Volume: '$(amixer -D pulse sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 } ')"

main = do
  xmproc <- spawnPipe "xmobar"
  spawn "xfsettingsd"
  spawn "start-pulseaudio-x11"
  -- spawn "redshift -l 47.608013:-122.335167 -t 6500:3500" -- causes issues when starting it this way for some reason... TODO: figure out why
  emacsDaemon <- spawnPipe "emacs --daemon"
  greenclipDaemon <- spawnPipe "greenclip daemon"
  spawn "pavucontrol"
  spawn "gnome-system-monitor"
  xmonad $
    ewmh $
      def
        { manageHook = myManageHook,
          layoutHook = myLayoutHook,
          logHook =
            dynamicLogWithPP
              xmobarPP
                { ppOutput = hPutStrLn xmproc,
                  ppTitle = xmobarColor (_myTheme_ppTitleColor myTheme) "" . shorten 50,
                  ppCurrent = xmobarColor (_myTheme_ppCurrentColor myTheme) "",
                  ppLayout = xmobarColor (_myTheme_ppLayoutColor myTheme) "",
                  ppSep = " | "
                },
          handleEventHook = handleEventHook def <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook <+> dynamicPropertyChange "WM_CLASS" myManageHook <+> docksEventHook,
          modMask = myModMask,
          borderWidth = 2,
          normalBorderColor = (_myTheme_normalBorderColor myTheme),
          focusedBorderColor = (_myTheme_focusedBorderColor myTheme),
          workspaces = ["1:web", "2:vid", "3:music", "4:game", "5:dev", "6:term", "7", "8", "9:mon"]
        }
        `additionalKeys` [ ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off"),
                           ((myModMask, xK_p), spawn "rofi -show run"),
                           ((myModMask, xK_i), spawn "rofi -show window"),
                           -- ((myModMask, xK_o), spawn "twitchy-emacs-play-script"),
                           ((myModMask, xK_o), spawn "play-yt-script"),
                           ((myModMask .|. shiftMask, xK_o), spawn "play-yt-script-format"),
                           ((myModMask, xK_s), promptSearch myXPConfig duckduckgo),
                           ((myModMask .|. shiftMask, xK_s), selectSearch duckduckgo),
                           ((myModMask, xK_v), shellPrompt myXPConfig),
                           ((myModMask .|. shiftMask, xK_v), prompt ("xterm" ++ " -e") myXPConfig),
                           ((myModMask, xK_g), windowPrompt myXPConfig Goto allWindows),
                           ((myModMask .|. shiftMask, xK_g), windowPrompt myXPConfig Bring allWindows),
                           ((myModMask, xK_c), spawn "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"),
                           ((myModMask, xK_y), spawn "ytfzf -D"),
                           ((myModMask, xK_F9), spawn audioQueryTrackInfoCommand),
                           ((0, xF86XK_AudioPlay), spawn audioPlayPauseCommand),
                           ((myModMask, xK_F12), spawn audioPlayPauseCommand),
                           ((0, xF86XK_AudioPrev), spawn audioPreviousCommand),
                           ((myModMask, xK_F10), spawn audioPreviousCommand),
                           ((0, xF86XK_AudioNext), spawn audioNextCommand),
                           ((myModMask, xK_F11), spawn audioNextCommand),
                           ((0, xF86XK_AudioLowerVolume), spawn audioLowerVolumeCommand),
                           ((myModMask, xK_Down), spawn audioLowerVolumeCommand),
                           ((0, xF86XK_AudioRaiseVolume), spawn audioRaiseVolumeCommand),
                           ((myModMask, xK_Up), spawn audioRaiseVolumeCommand),
                           ( (myModMask, xK_semicolon),
                             (submap . M.fromList) $
                               [ ((0, xK_f), notifySpawn "sensible-browser"),
                                 ((0, xK_c), notifySpawn "chromium"),
                                 ((0, xK_v), notifySpawn "rofi-bluetooth"),
                                 ((0, xK_b), notifySpawn "xterm -e bluetoothctl"),
                                 ((0, xK_s), notifySpawn "spotify"),
                                 ((0, xK_a), notifySpawn "steam"),
                                 ((0, xK_p), notifySpawn "pavucontrol"),
                                 ((0, xK_m), notifySpawn "gnome-system-monitor"),
                                 ((0, xK_r), notifySpawn "xterm -e ranger"),
                                 ((0, xK_t), notifySpawn "thunar"),
                                 ((0, xK_d), notifySpawn "xterm -e dropbox"),
                                 ((0, xK_x), notifySpawn "termonad"),
                                 ((0, xK_e), notifySpawn "emacsclient -n -c"),
                                 ((0, xK_i), spawn "rofi -modi emoji -show emoji -font 'Noto Color Emoji 12'")
                               ]
                           )
                         ]

notifySpawn s = do
  spawn ("notify-send -t 3000 'Launching " ++ s ++ "'")
  spawn s
