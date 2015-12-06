{-# LANGUAGE OverloadedStrings #-}

import Clay
import Clay.Stylesheet (StyleM)
import qualified Clay.Font as F
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
    overflow auto

  div # "#logo" ? do
    display inline

  div # "#navigation" ? do
    margin (px 10) (px 50) (px 10) (px 50)
    float floatRight

  div # "#navigation" |> ul ? do
    overflow auto
    listStyleType none

  div # "#navigation" |> ul |> li ? do
    height (px 25)
    float floatLeft
    marginRight (px 0)
    borderRight solid (px 2) base1
    paddingLeft (px 20)
    paddingRight (px 20)

  div # "#navigation" |> ul |> li # lastChild ? do
    borderRightStyle none

  div # "#navigation" |> ul |> li |> a ? do
    textDecoration none
    color base01
    textTransform uppercase
    transition "all" (sec 0.2) ease (sec 0)

  div # "#navigation" |> ul |> li |> a # hover ? do
    color base1

  div # "#navigation" |> ul |> li |> a # active ? do
    fontWeight bold
    color mRed

  div # "#content" ? do
    background  base3
    border      solid (px 3) base1
    posizionaOrizzontalmente
    marginBottom (px 30)

  div # "#footer" ? do
    marginTop (px 30)
    posizionaOrizzontalmente
    fontSizeCustom F.small

  -- Stiliamo i link dentro la lista
  li ? do
    listStyleType none

  li |> a ? do
    color mOrange

  li |> u ? do
    fontStyle italic
    textDecoration none

  highlightSource


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


highlightSource :: Css
highlightSource = do
  pre ? do
    backgroundColor base2
    border solid (px 3) base1
    padding (px 20) (px 20) (px 20) (px 20)
    -- KeyWord
    star # ".sourceCode" |> star # ".kw" ? color mBlue
    -- DataType
    star # ".sourceCode" |> star # ".dt" ? do color mRed; fontWeight bold
    -- Decimal Value, BaseN, Float
    star # ".sourceCode" |> star # ".dv" ? color mMagenta
    -- Char
    star # ".sourceCode" |> star # ".ch" ? color mRed
    -- String
    star # ".sourceCode" |> star # ".st" ? color mCyan
    -- Comment
    star # ".sourceCode" |> star # ".co" ? do color base1; fontStyle italic
    -- Other
    -- star # ".sourceCode" |> star # ".ot" ? color base01
    -- .sourceCode .ot { color: #A57800; }
    -- Alert
    star # ".sourceCode" |> star # ".al" ? color mOrange
    -- FunctionTok
    star # ".sourceCode" |> star # ".fu" ? color mBlue
    -- -- RegionMarkerTok
    -- .sourceCode .re { }
    -- -- ErrorTok
    -- .sourceCode .er { color: #D30102; font-weight: bold; }
