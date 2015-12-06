{-# LANGUAGE OverloadedStrings #-}

import Clay
import Clay.Stylesheet (StyleM)
import Prelude hiding (div)

main :: IO ()
main = putCss $ do
  body ? do
    color       base00
    fontSize    (px 18)
    fontFamily [ "Palatino Linotype" , "Book Antiqua"
               , "Palatino" , "FreeSerif" ] [serif]

  div # "#header" ? do
    background  base3
    borderBottom solid (px 2) base01
    borderLeft solid (px 2) base01
    borderRight solid (px 2) base01
    marginBottom (px 30)
    posizionaOrizzontalmente

  div # "#logo" ? do
    display inline

  div # "#navigation" ? do
    float floatRight

  div # "#content" ? do
    background  base3
    border      solid (px 3) base1
    posizionaOrizzontalmente
    marginBottom (px 30)

  div # "#footer" ? do
    marginTop (px 30)
    background  base3
    border      solid (px 3) base1
    posizionaOrizzontalmente
    marginBottom (px 30)


-- Dark background colors
base03  = rgb   0  43  54
base02  = rgb   7  54  66
-- Emphasized content
base01  = rgb  88 110 117
-- primary content
base00  = rgb 101 123 131

base0   = rgb 131 148 150
-- comments, secondary content
base1   = rgb 147 161 161

-- Light background colors
-- In light mode, is background highlight
base2   = rgb 238 232 213
-- In light mode, is background
base3   = rgb 253 246 227


-- Accent Colors
mYellow  = rgb  181 137   0
mOrange  = rgb  203  75  22
mRed     = rgb  220  50  47
mMagenta = rgb  211  54 130
mViolet  = rgb  108 113 196
mBlue    = rgb   38 139 210
mCyan    = rgb   42 161 152
mGreen   = rgb  133 153   0


--------------------------------------------------------------------------------
-- Funzioni di posizionamento orizzontale
--------------------------------------------------------------------------------

posizionaOrizzontalmente :: StyleM ()
posizionaOrizzontalmente = do
  marginLeft   (pct 12.5)
  paddingLeft  (pct 5)
  width        (pct 65)
  paddingRight (pct 5)
  marginRight  (pct 12.5)
