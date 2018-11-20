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

-- Bind Mod to the left alt key
myModMask = mod1Mask

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
        } `additionalKeys`
        [ ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((myModMask , xK_p), spawn "rofi -show run")
        ]
