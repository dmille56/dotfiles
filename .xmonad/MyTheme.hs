module MyTheme (MyTheme (..), draculaTheme, matrixTheme) where

import XMonad
import XMonad.Layout.Decoration (Theme (..))

data MyTheme = MyTheme
  { _myTheme_theme :: Theme,
    _myTheme_ppTitleColor :: String, -- xmobar title
    _myTheme_ppCurrentColor :: String, -- xmobar current window
    _myTheme_ppLayoutColor :: String, -- xmobar current layout
    _myTheme_normalBorderColor :: String,
    _myTheme_focusedBorderColor :: String,
    _myTheme_xpConfigFgColor :: String
  }

draculaActiveColor = "#6272a4"

draculaActiveTextColor = "#F8F8F2"

draculaActiveBorderColor = "#6272a4"

draculaInactiveBorderColor = "#384261"

draculaInactiveColor = "#282A36"

draculaInactiveTextColor = "#dfe3ec"

draculaUrgentTextColor = "#FF5555"

draculaNormalBorderColor = draculaInactiveBorderColor

draculaFocusedBorderColor = draculaActiveBorderColor

draculaPPCurrent = "#FF79C6"

draculaPPTitle = "#8BE9fd"

draculaPPLayout = "#BD93f9"

draculaTheme :: MyTheme
draculaTheme =
  MyTheme
    { _myTheme_theme =
        def
          { inactiveBorderColor = draculaInactiveBorderColor,
            inactiveColor = draculaInactiveColor,
            inactiveTextColor = draculaInactiveTextColor,
            activeTextColor = draculaActiveTextColor,
            activeBorderColor = draculaActiveBorderColor,
            activeColor = draculaActiveColor,
            urgentTextColor = draculaUrgentTextColor,
            urgentColor = draculaInactiveColor,
            urgentBorderColor = draculaUrgentTextColor
          },
      _myTheme_ppTitleColor = draculaPPTitle,
      _myTheme_ppCurrentColor = draculaPPCurrent,
      _myTheme_ppLayoutColor = draculaPPLayout,
      _myTheme_normalBorderColor = draculaNormalBorderColor,
      _myTheme_focusedBorderColor = draculaFocusedBorderColor,
      _myTheme_xpConfigFgColor = draculaPPTitle
    }

greyColor = "#928374"

greyColor2 = "#282828"

greenColor = "#689d6a"

yellowColor = "#ebdbb2"

redColor = "red"

matrixTheme :: MyTheme
matrixTheme =
  MyTheme
    { _myTheme_theme =
        def
          { inactiveBorderColor = greyColor,
            inactiveColor = greyColor2,
            activeTextColor = greenColor,
            activeBorderColor = greenColor,
            activeColor = greyColor2,
            urgentTextColor = redColor,
            decoHeight = 20
          },
      _myTheme_ppTitleColor = yellowColor,
      _myTheme_ppLayoutColor = yellowColor,
      _myTheme_ppCurrentColor = greenColor,
      _myTheme_normalBorderColor = greyColor,
      _myTheme_focusedBorderColor = greenColor,
      _myTheme_xpConfigFgColor = yellowColor
    }
