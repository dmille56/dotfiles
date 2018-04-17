import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-- Bind Mod to the left alt key
myModMask = mod1Mask

myLayout = smartBorders tiled ||| noBorders Full ||| smartBorders simpleTabbed
  where 
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
	, layoutHook = avoidStruts myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#ebdbb2" "" . shorten 50
                        , ppCurrent = xmobarColor "#689d6a" ""
                        }
        , modMask = myModMask
        , borderWidth = 2
        , normalBorderColor = "#928374"
        , focusedBorderColor = "#689d6a"
        } `additionalKeys`
        [ ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((myModMask , xK_p), spawn "rofi -show run")
        ]
