{-# LANGUAGE OverloadedStrings #-}

import Clay
import Prelude hiding (div)

main :: IO ()
main = putCss $ do
  body ? do
    background  base3
    color       base00
    border      dashed (px 2) orange
    fontSize    (px 18)
    fontFamily [ "Palatino Linotype" , "Book Antiqua"
               , "Palatino" , "FreeSerif" ] [serif]

  div # "#header" ? do
    borderBottom solid (px 2) base01
    marginBottom (px 30)
    padding (px 20) (px 0) (px 20) (px 0)

  div # "#logo" ? a ? do
    color base01
    float floatLeft
    fontSize (px 30)
    fontWeight bold
    textDecoration none
    
  div # "#header"
          # "#navigation" ? do
            textAlign end

  div # "#header"
          # "#navigation" ? a ? do
            color black
            fontSize (px 18)
            fontWeight bold
            marginLeft (px 12)
            textDecoration none
            textTransform uppercase

base03  = rgb   0  43  54 
base02  = rgb   7  54  66 
base01  = rgb  88 110 117 
base00  = rgb 101 123 131 
base0   = rgb 131 148 150 
base1   = rgb 147 161 161 
base2   = rgb 238 232 213 
base3   = rgb 253 246 227 

-- yellow    181 137   0 
-- orange    203  75  22 
-- red       220  50  47 
-- magenta   211  54 130 
-- violet    108 113 196 
-- blue       38 139 210 
-- cyan       42 161 152 
-- green     133 153   0 
