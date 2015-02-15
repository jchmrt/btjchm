{-# LANGUAGE OverloadedStrings #-}
module Color ( Color (..)
             , fColor
             , reset
             , bold ) where

import Data.Text
import qualified Prelude as P

data Color = White
           | Black
           | Blue
           | Green
           | Red
           | Brown
           | Purple
           | Orange
           | Yellow
           | Lime
           | Teal
           | Cyan
           | Royal
           | Pink
           | Grey
           | Silver

colorCode :: Color -> Text
colorCode White  = "00"
colorCode Black  = "01"
colorCode Blue   = "02"
colorCode Green  = "03"
colorCode Red    = "04"
colorCode Brown  = "05"
colorCode Purple = "06"
colorCode Orange = "07"
colorCode Yellow = "08"
colorCode Lime   = "09"
colorCode Teal   = "10"
colorCode Cyan   = "11"
colorCode Royal  = "12"
colorCode Pink   = "13"
colorCode Grey   = "14"
colorCode Silver = "15"

fColor :: Color -> Text
fColor c = '\^C' `cons` colorCode c

bold :: Text
bold = "\^B"

reset :: Text
reset = "\^P"
