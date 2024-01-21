{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import Termonad.App (defaultMain)
import Termonad.Config
  ( FontConfig,
    FontSize (FontSizePoints),
    Option (Set),
    ShowScrollbar (ShowScrollbarAlways),
    confirmExit,
    defaultConfigOptions,
    defaultFontConfig,
    defaultTMConfig,
    enableSixel,
    fontConfig,
    fontFamily,
    fontSize,
    options,
    showMenu,
    showScrollbar,
  )
import Termonad.Config.Colour
  ( AlphaColour,
    ColourConfig,
    List8,
    Palette (..),
    addColourExtension,
    backgroundColour,
    createColour,
    createColourExtension,
    cursorBgColour,
    defaultColourConfig,
    defaultStandardColours,
    foregroundColour,
    mkList8,
    palette,
  )

-- | This sets the color of the cursor in the terminal.
--
-- This uses the "Data.Colour" module to define a dark-red color.
-- There are many default colors defined in "Data.Colour.Names".
cursBgColour :: AlphaColour Double
cursBgColour = createColour 204 0 0

-- This is our Dracula 'ColourConfig'.
dracula :: ColourConfig (AlphaColour Double)
dracula =
  defaultColourConfig
    { -- Set the default background & foreground colour of text of the terminal.
      backgroundColour = Set (createColour 40 42 54), -- black.0
      foregroundColour = Set (createColour 248 248 242), -- white.7
      -- Set the extended palette that has 2 Vecs of 8 Dracula palette colours
      palette = ExtendedPalette draculaNormal draculaBright
    }
  where
    draculaNormal :: List8 (AlphaColour Double)
    draculaNormal =
      fromMaybe defaultStandardColours $
        mkList8
          [ createColour 40 42 54, -- black.0
            createColour 255 85 85, -- red.1
            createColour 80 250 123, -- green.2
            createColour 241 250 140, -- yellow.3
            createColour 189 147 249, -- blue.4
            createColour 255 121 198, -- magenta.5
            createColour 139 233 253, -- cyan.6
            createColour 191 191 191 -- white.7
          ]

    draculaBright :: List8 (AlphaColour Double)
    draculaBright =
      fromMaybe defaultStandardColours $
        mkList8
          [ createColour 77 77 77, -- black.8
            createColour 255 110 103, -- red.9
            createColour 90 247 142, -- green.10
            createColour 244 249 157, -- yellow.11
            createColour 202 169 250, -- blue.12
            createColour 255 146 208, -- magenta.13
            createColour 154 237 254, -- cyan.14
            createColour 230 230 230 -- white.15
          ]

-- | This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "DejaVu Sans Mono",
      fontSize = FontSizePoints 13
    }

main :: IO ()
main = do
  colExt <- createColourExtension dracula
  let termonadConf =
        defaultTMConfig
          { options =
              defaultConfigOptions
                { fontConfig = fontConf,
                  -- Make sure the scrollbar is always visible.
                  showScrollbar = ShowScrollbarAlways,
                  confirmExit = False,
                  showMenu = False,
                  enableSixel = True
                }
          }
          `addColourExtension` colExt
  defaultMain termonadConf
