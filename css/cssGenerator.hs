{-# LANGUAGE OverloadedStrings #-}

import Clay
import Clay.Stylesheet (StyleM)
import qualified Clay.Font as F
import qualified Clay.Size as S

import Prelude hiding (div)

main :: IO ()
main = putCss $ do
  tufte
  highlightSource
  codeStyling
  headerStyling2
  footerStyling

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

highlightSource :: Css
highlightSource = do
  pre ? do
    -- backgroundColor base03
    -- border solid (px 3) base1
    backgroundImage (url "../images/denim.png")
    padding (px 10) (px 20) (px 10) (px 20)
    -- KeyWord
    color tufteBackground
    star # ".sourceCode" |> star # ".kw" ? color mYellow
    -- DataType
    star # ".sourceCode" |> star # ".dt" ? do color mBlue; fontWeight bold
    -- Decimal Value, BaseN, Float
    star # ".sourceCode" |> star # ".dv" ? color mMagenta
    -- Char
    star # ".sourceCode" |> star # ".ch" ? color mRed
    -- String
    star # ".sourceCode" |> star # ".st" ? color mCyan
    -- Comment
    star # ".sourceCode" |> star # ".co" ? do color tufteBackground; fontStyle italic
    -- Other
    star # ".sourceCode" |> star # ".ot" ? color tufteBackground
    -- Alert
    star # ".sourceCode" |> star # ".al" ? color mOrange
    -- FunctionTok
    star # ".sourceCode" |> star # ".fu" ? color mBlue
    -- -- RegionMarkerTok
    -- .sourceCode .re { }
    -- -- ErrorTok
    -- .sourceCode .er { color: #D30102; font-weight: bold; }

--------------------------------------------------------------------------------
-- Inizio a lavorare un poco su tufte
--------------------------------------------------------------------------------
tufte :: Css
tufte = do
  tufteGeneralLayout
  ul ? do
    listStyleType none
  time ? do
    fontSize (S.rem 0.8)
    fontStyle italic
  a ? do
    textDecoration none
    star # ":link" ? color darkred
    star # ":visited" ? color darkred
    star # ":hover" ? color darkred
    star # ":active" ? color darkred

tufteGeneralLayout :: Css
tufteGeneralLayout = do
  html ? fontSize (px 15)
  body ? do
    width (pct 87.5)
    marginLeft  auto
    marginRight auto
    paddingLeft (pct 12.5)
    fontFamily ["ETBembo", "Palatino", "Palatino Linotype", "Palatino LT STD", "Book Antiqua", "Georgia"] [serif]
    backgroundColor tufteBackground
    color (Other "#111")
    maxWidth (px 1400)
    -- Mi manca counter-reset
  h1 ? do
    fontWeight   (weight 400)
    marginTop    (S.rem 4)
    marginBottom (S.rem 1.5)
    fontSize     (S.rem 3.2)
    lineHeight   (S.rem 1)

  h2 ? do
    fontStyle italic
    fontWeight   (weight 400)
    marginTop    (S.rem 2.1)
    marginBottom (S.rem 0)
    fontSize     (S.rem 2.2)
    lineHeight   (S.rem 1)

  h3 ? do
    fontStyle italic
    fontWeight   (weight 400)
    marginTop    (S.rem 2)
    marginBottom (S.rem 0)
    fontSize     (S.rem 1.7)
    lineHeight   (S.rem 1)

  p # ".subtitle" ? do
    fontStyle italic
    marginTop    (S.rem 1)
    marginBottom (S.rem 1)
    fontSize     (S.rem 1.8)
    display block
    lineHeight   (S.rem 1)

  star # ".numeral" ? do
    fontFamily ["ETBembo"] []
    fontStyle (other "RomanOSF")

  star # ".danger" ? do
    color red

tufteBackground = Other "#fffff8"

codeStyling = do
  pre ? do
    fontFamily ["Consolas", "Liberation Mono", "Menlo", "Courier"] [monospace]
    fontSize (S.rem 1.125)
    lineHeight (S.rem 1.6)
  pre # ".code" ? do
    width (pct 52.5)
    paddingLeft (pct 2.5)
    overflowX scroll

headerStyling :: Css
headerStyling = do
  div # "#header" ? do
    backgroundImage (url "../images/footer_lodyas.png")
    a ? do
      color tufteBackground

    div # "#logo" ? do
      padding (S.rem 1) (S.rem 1) (S.rem 1) (S.rem 1)
      display inline
      a ? do
        fontFamily ["AquilineTwo"] [cursive]
        fontSize (S.rem 17)

    div # "#navigation" ? do
      display inline
      margin (px 10) (px 50) (px 10) (px 50)
      float floatRight

      ul ? do
        listStyleType none

        li ? do
          height (px 25)
          float floatLeft
          marginRight (px 0)
          borderRight solid (px 2) base1
          paddingLeft (px 20)
          paddingRight (px 20)
          star # lastChild ? do
            borderRightStyle none
          a ? do
            fontFamily ["ETBembo"] []
            fontStyle (other "RomanOSF")
            textDecoration none
            color tufteBackground
            textTransform uppercase
            transition "all" (sec 0.2) ease (sec 0)
            star # hover ? do
              color mCyan
            star # active ? do
              fontWeight bold
              color mRed

headerStyling2 :: Css
headerStyling2 = do
  div # "#header" |> div # "#logo" ? do
      display inline
      a ? do
        fontFamily ["AquilineTwo"] [cursive]
        fontSize (S.rem 1.5)

footerStyling :: Css
footerStyling = do
  div # "#footer" ? do
    fontSize (S.rem 0.8)
