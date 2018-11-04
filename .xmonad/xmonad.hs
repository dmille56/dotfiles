import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.ManageHelpers
import System.IO

-- Bind Mod to the left alt key
myModMask = mod1Mask

--    activeColor :: String
--    Color of the active window
--    inactiveColor :: String
--    Color of the inactive window
--    urgentColor :: String
--    Color of the urgent window
--    activeBorderColor :: String
--    Color of the border of the active window
--    inactiveBorderColor :: String
--    Color of the border of the inactive window
--    urgentBorderColor :: String
--    Color of the border of the urgent window
--    activeTextColor :: String
--    Color of the text of the active window
--    inactiveTextColor :: String
--    Color of the text of the inactive window
--    urgentTextColor :: String
--    Color of the text of the urgent window
--    fontName :: String
--    Font name
--    decoWidth :: Dimension
--    Maximum width of the decorations (if supported by the DecorationStyle)
--    decoHeight :: Dimension
--    Height of the decorations
--    windowTitleAddons :: [(String, Align)]
--    Extra text to appear in a window's title bar. Refer to for a use XMonad.Layout.ImageButtonDecoration
--    windowTitleIcons :: [([[Bool]], Placement)]
--    Extra icons to appear in a window's title bar. Inner [Bool] is a row in a icon bitmap.

myTabConfig = def { inactiveBorderColor = "#928374"
		    , inactiveColor = "#282828"
	            , activeTextColor = "#689d6a"
		    , activeBorderColor = "#689d6a"
		    , activeColor = "#282828"
	            , urgentTextColor = "red"
	            , decoHeight = 20 }

myLayout = smartBorders tiled ||| noBorders Full ||| smartBorders (tabbed shrinkText myTabConfig)
  where 
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myManageHook = composeAll [
	manageDocks,
	isFullscreen --> doFullFloat,
	manageHook defaultConfig
	]

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ defaultConfig
        { manageHook = myManageHook
	, layoutHook = avoidStruts myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#ebdbb2" "" . shorten 50
                        , ppCurrent = xmobarColor "#689d6a" ""
                        }
        , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
        , modMask = myModMask
        , borderWidth = 2
        , normalBorderColor = "#928374"
        , focusedBorderColor = "#689d6a"
        } `additionalKeys`
        [ ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((myModMask , xK_p), spawn "rofi -show run")
        ]
