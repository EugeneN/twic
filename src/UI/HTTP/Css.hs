{-# LANGUAGE OverloadedStrings #-}

module UI.HTTP.Css where

import           Clay
import qualified Clay as C
import           Data.Monoid ((<>))

entityColor = "#2FC2EF" :: Color
baseColor = "#404040" :: Color


bodyCss :: Css
bodyCss = do
  body ? do
    background  ("#ececec" :: Color)
--    fontFamily  ["Helvetica Neue"] [sansSerif]
    textAlign (alignSide sideCenter)
    margin (px 0) (px 0) (px 0) (px 0)
    color baseColor

    fontFamily ["Roboto"] [sansSerif]
    fontSize (px 16)
    fontWeight normal

  li ? do
    listStyleType none
    position relative
    margin (px 25) (px 5) (px 25) (px 5)
    clear both
    --verticalAlign textTop

  li # hover ? do
    ".toolbar-target" ? do
        display block

  ul ? do
    padding (px 0) (px 0) (px 0) (px 0)

  a ? do
    color baseColor
    textDecoration none
    --borderBottom solid (px 1) blue

  a # hover ? do
    textDecoration underline

  pre ? do
    display none

  ".inline-link" ? do
--    borderBottom solid (px 1) lightblue
    color entityColor
    --width (em 2)
    --display inlineBlock
    --overflow hidden
    --position relative
    --top (px 4)

  --".inline-link" # after ? do
    --content (stringContent "...")

  ".inline-link" # hover ? do
    color entityColor
    --width auto

  --".inline-link" # after # hover ? do
    --content none

  ".username-tag" ? do
    fontWeight bold
    color entityColor


  ".colon" ? do
    marginRight (px 15)

  ".container" ? do
    width (px 700)
    marginTop (px 20)
    marginBottom (px 20)
    marginLeft (auto)
    marginRight (auto)
    textAlign (alignSide sideLeft)
    background white
    paddingTop (px 10)
    paddingBottom (px 20)
    paddingLeft (px 20)
    paddingRight (px 20)
    boxShadow 0 0 (px 4) (setA 50 baseColor)
    transition "all" (ms 100) linear (ms 100)
    fontSize (px 23)


  ".user-icon" ? do
    display inlineBlock
    width (px 30)
    height (px 30)
    marginTop auto
    marginBottom auto
    marginRight (px 20)
    verticalAlign textTop
    --position relative
    --top (px 7)

  ".user-icon-img" ? do
    width (px 30)
    height (px 30)
    margin (px 0) (px 0) (px 0) (px 0)
    borderRadius (pct 50) (pct 50) (pct 50) (pct 50)

  ".tweet-body" ? do
    width (px 600)
    display inlineBlock
    verticalAlign textTop

  ".no-tweets" ? do
    textAlign (alignSide sideCenter)
--    fontWeight bold

  ".error" ? do
    textAlign (alignSide sideCenter)
    color red
    padding (px 25) (px 25) (px 25) (px 25)
    marginBottom (px (-20))

  ".success" ? do
      textAlign (alignSide sideCenter)
      color green
      padding (px 25) (px 25) (px 25) (px 25)
      marginBottom (px (-20))

  ".no-new-tweets" ? do
    background grey

  ".refresh" ? do
    textAlign (alignSide sideCenter)
    fontWeight bold
    padding (px 40)  (px 40)  (px 40)  (px 40)
    fontSize (px 30)
    marginBottom (px 100)

    ".there-are-new-tweets" ? do
        padding (px 4)  (px 4)  (px 4)  (px 4)
        borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
        background  orange
        borderTop solid (px 0) red
        borderBottom solid (px 0) red
        borderLeft solid (px 0) red
        borderRight solid (px 0) red
        width (px 70)
        height (px 70)
        color white
        fontWeight bold
        fontSize (px 20)
        cursor pointer
        boxShadow 0 0 (px 4) (setA 50 baseColor)

    ".no-new-tweets" ? do
        padding (px 4)  (px 4)  (px 4)  (px 4)
        borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
        borderTop solid (px 0) red
        borderBottom solid (px 0) red
        borderLeft solid (px 0) red
        borderRight solid (px 0) red
        width (px 70)
        height (px 70)
        color white
        fontWeight bold
        fontSize (px 20)
        cursor pointer
        boxShadow 0 0 (px 4) (setA 50 baseColor)
        background grey

    ".there-are-new-tweets" # focus ? do
        outline solid (px 0) baseColor

    ".no-new-tweets" # focus ? do
        outline solid (px 0) baseColor

    ".there-are-new-tweets" # hover ? do
        background cyan

    a ? do
      transition "all" (ms 100) linear (ms 100)

    a # hover ? do
      textDecoration none
      textShadow (px 1) (px 1) (px 2) ("#404040" :: Color)

  ".refresh-bottom" ? do
    textAlign (alignSide sideCenter)
    marginTop (px 40)
    fontWeight bold

  ".inline-img" ? do
    marginLeft (px 50)
    marginTop (px 15)

  ".unknown-media" ? do
    padding (px 20) (px 20) (px 20) (px 20)
    background  ("#cccccc" :: Color)
    textAlign (alignSide sideCenter)

  ".unparsable" ? do
    borderTop solid (px 1) red
    borderBottom solid (px 1) red
    borderLeft solid (px 1) red
    borderRight solid (px 1) red

  ".toolbar-target" ? do
    width (px 140)
    height (px 40)
    position absolute
    right (px (-80))
    top (px 0)
    zIndex 50
    display none
    borderTop solid (px 1) transparent
    borderBottom solid (px 1) transparent
    borderLeft solid (px 1) transparent
    borderRight solid (px 1) transparent

  ".toolbar" ? do
    background transparent
    color white
    position absolute
    zIndex 100
    padding (px 0) (px 0) (px 0) (px 0)
    margin (px 0) (px 0) (px 0) (px 0)

    li ? do
        padding (px 5) (px 10) (px 5) (px 10)
        display inline
        margin (px 0) (px 0) (px 0) (px 0)
        cursor pointer
        background $ rgba 40 40 40 90

    li # hover ? do
        background red


  ".toolbar" # hover ? do
      display block


  ".hash-tag" ? do
    color blue

    a ? do
        color baseColor
        fontStyle italic

--    a # hover ? do
--        borderBottom solid (px 1) blue


usernameCss :: Css
usernameCss = ".user-name" ? do
  --fontWeight bold
  width (px 180)
  display none
  verticalAlign textTop
  position relative
  top (px 6)
  position absolute

  a # hover ? do
    borderBottom solid (px 1) blue

  ".user-name" # hover ? do
    borderBottom solid (px 1) blue


allCss =  (render bodyCss) <> (render usernameCss)
