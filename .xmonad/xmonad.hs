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
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

myModMask :: KeyMask
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

myLayoutHook = onWorkspace "9" ((smartBorders . avoidStruts) myLayout') $ ((smartBorders . avoidStruts) myLayout)
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
    [ className =? "steam" --> doShift "3:game",
      className =? "mpv" --> doShift "2:vid",
      className =? "vlc" --> doShift "2:vid",
      className =? "Chromium-browser" --> doShift "2:vid",
      className =? "Emacs" --> doShift "9:dev",
      className =? "Termonad-linux-x86_64" --> doShift "8:term",
      className =? "Alacritty" --> doShift "8:term",
      title =? "Mozilla Firefox" --> doShift "1:web",
      (isFullscreen --> doFullFloat),
      manageDocks,
      namedScratchpadManageHook myScratchPads,
      manageHook def
    ]

rectCentered :: Rational -> W.RationalRect
rectCentered percentage = W.RationalRect offset offset percentage percentage
  where
    offset = (1 - percentage) / 2

-- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads = [btm, term, fileManager, lazygit, music, volumeControl]
  where
    btm = NS "btm" spawn' find manage'
      where
        spawn' = glWrapper "alacritty --class btm_term -e btm"
        find = className =? "btm_term"
        manage' = customFloating $ rectCentered 0.9
    volumeControl = NS "volumeControl" spawn' find manage'
      where
        spawn' = "pavucontrol"
        find = className =? "Pavucontrol"
        manage' = customFloating $ rectCentered 0.9
    term = NS "term" spawn' find manage'
      where
        spawn' = glWrapper "alacritty --class scratchpad_term"
        find = className =? "scratchpad_term"
        manage' = customFloating $ rectCentered 0.9
    fileManager = NS "fileManager" spawn' find manage'
      where
        spawn' = "thunar"
        find = className =? "Thunar"
        manage' = customFloating $ rectCentered 0.9
    lazygit = NS "lazygit" spawn' find manage'
      where
        spawn' = glWrapper "alacritty --class lazygit_term -e lazygit"
        find = className =? "lazygit_term"
        manage' = customFloating $ rectCentered 0.95
    music = NS "music" spawn' find manage'
      where
        spawn' = glWrapper "alacritty --class spotify_term -e spotify_player"
        find = className =? "spotify_term"
        manage' = customFloating $ rectCentered 0.9

-- music = NS "music" spawn' find manage'
--   where
--     spawn' = "spotify"
--     find = className =? "Spotify"
--     manage' = customFloating $ rectCentered 0.9

openScratchPad :: String -> X ()
openScratchPad = namedScratchpadAction myScratchPads

audioPlayPauseCommand = "playerctl play-pause -p spotifyd,spotify,chromium,firefox"

audioPreviousCommand = "playerctl previous -p spotifyd,spotify,chromium,firefox"

audioNextCommand = "playerctl next -p spotifyd,spotify,chromium,firefox"

audioQueryTrackInfoCommand = "notify-send -i media-optical -t 1000 \"$(playerctl metadata artist) - $(playerctl metadata title)\""

audioLowerVolumeCommand = "amixer -D pulse sset Master 5%-; notify-send -i audio-volume-medium -t 1000 'Volume: '$(amixer -D pulse sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 } ')"

audioRaiseVolumeCommand = "amixer -D pulse sset Master 5%+; notify-send -i audio-volume-medium -t 1000 'Volume: '$(amixer -D pulse sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 } ')"

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xfsettingsd"
  spawnOnce "start-pulseaudio-x1ll"
  spawnOnce "trayer --edge bottom --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x282A36 --expand true"
  spawnOnce "redshift-gtk"
  spawn "xrdb ~/.XResources"
  greenclipDaemon <- spawnPipe "greenclip daemon"
  spawn "xmodmap -e 'keycode 127 = Insert'"
  spawn "xmodmap -e 'keycode 118 = Pause'"

main = do
  xmproc <- spawnPipe "xmobar"
  -- spawn "redshift -l 47.608013:-122.335167 -t 6500:3500" -- causes issues when starting it this way for some reason... :TODO: figure out why
  -- emacsDaemon <- spawnPipe "emacs --daemon" -- maybe re-enable this at some point... :TODO: figure out why svg-tag-mode causes issues when started as a daemon
  xmonad $
    ewmh $
      def
        { manageHook = myManageHook,
          layoutHook = myLayoutHook,
          startupHook = myStartupHook,
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
          workspaces = ["1:web", "2:vid", "3:game", "4", "5", "6", "7", "8:term", "9:dev"]
        }
        `additionalKeys` [ ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off"),
                           ((myModMask, xK_p), spawn "rofi -show run"),
                           ((myModMask, xK_i), spawn "rofi -show window"),
                           ((myModMask, xK_o), spawn "play-yt-script"),
                           ((myModMask .|. shiftMask, xK_o), spawn "play-yt-script-format"),
                           ((myModMask, xK_s), promptSearch myXPConfig duckduckgo),
                           ((myModMask .|. shiftMask, xK_s), selectSearch duckduckgo),
                           ((myModMask, xK_v), shellPrompt myXPConfig),
                           ((myModMask .|. shiftMask, xK_v), prompt (glWrapper "alacritty" ++ " -e") myXPConfig),
                           ((myModMask, xK_g), windowPrompt myXPConfig Goto allWindows),
                           ((myModMask .|. shiftMask, xK_g), windowPrompt myXPConfig Bring allWindows),
                           ((myModMask, xK_c), spawn "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"),
                           ((myModMask, xK_y), spawn "ytfzf -D"),
                           ((myModMask, xK_backslash), openScratchPad "term"),
                           ((myModMask .|. shiftMask, xK_backslash), openScratchPad "lazygit"),
                           ((myModMask, xK_bracketright), openScratchPad "btm"),
                           ((myModMask .|. shiftMask, xK_bracketright), openScratchPad "volumeControl"),
                           ((myModMask, xK_bracketleft), openScratchPad "music"),
                           ((myModMask .|. shiftMask, xK_bracketleft), openScratchPad "fileManager"),
                           -- ((myModMask, xK_j), spawn "xdotool key Page_Down"), -- Remap mod+j, mod+k to page down / up
                           -- ((myModMask, xK_k), spawn "xdotool key Page_Up"),
                           ((myModMask, xK_d), spawn "export BROWSER=sensible-browser && rofi-buku"),
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
                                 ((0, xK_b), notifySpawn $ glWrapper "alacritty -e bluetoothctl"),
                                 ((0, xK_s), notifySpawn "spotify"),
                                 ((0, xK_a), notifySpawn "steam"),
                                 ((0, xK_p), notifySpawn "pavucontrol"),
                                 ((0, xK_m), notifySpawn "gnome-system-monitor"),
                                 ((0, xK_r), notifySpawn $ glWrapper "alacritty -e ranger"),
                                 ((0, xK_t), notifySpawn "thunar"),
                                 ((0, xK_d), notifySpawn $ glWrapper "alacritty -e dropbox"),
                                 ((0, xK_x), notifySpawn $ glWrapper "alacritty"),
                                 ((0, xK_e), notifySpawn "emacsclient -n -c"),
                                 ((shiftMask, xK_e), notifySpawn "emacs"),
                                 ((0, xK_i), spawn "rofi -modi emoji -show emoji -font 'Noto Color Emoji 12'")
                               ]
                           )
                         ]

notifySpawn s = do
  spawn ("notify-send -t 3000 'Launching " ++ s ++ "'")
  spawn s

glWrapper s = "nixGL " ++ s
