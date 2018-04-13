{-# LANGUAGE OverloadedStrings #-}
import Clay
import qualified Clay.Media as Mq
import Control.Monad
import Prelude hiding (div, span, (**), all)
import qualified Prelude as P

travisLightGray = grayish 136
travisBlue      = rgb 0 0x99 0xFF
codeBackground  = grayish 0xEE
headerRuleColor = grayish 0xDD

titleFamily = do
  fontFamily ["Open Sans", "Nexa Light"] [sansSerif]
  fontWeight (weight 300)
bodyFamily  = fontFamily ["Palatino Linotype", "Palatino", "Baskerville", "Book Antiqua", "URW Palladio L"] [serif]

-- Remember to update these in gallery.js as well
headerWidth = 280
headerRightSpace = 20
contentMaxWidth = 700
contentMinWidth = 300
contentPaddingLeft = 75
contentPaddingTop = 75
contentPaddingTopNarrow = miniHeaderHeight + 10
contentPaddingLeftNarrow = 10
contentPaddingRightNarrow = 10
normalBodyPadding = 190
miniHeaderHeight :: Num a => a
miniHeaderHeight = 50

siteMinWidth = headerWidth - headerRightSpace + contentMinWidth + contentPaddingLeft + normalBodyPadding * 2
contentMarginLeft = headerWidth - contentPaddingLeft + 10

plainLinks =
    a ? do
      textDecoration none
      color         black

narrowW = query all [Mq.maxWidth (px siteMinWidth)]
normalW = query all [Mq.minWidth (px siteMinWidth)]
figureShadow = pure ()

-- Related speciically to projects
projects = do
  ul # byId "projects" ? do
    "list-style"   -: "none"
    sym padding       (px 0)
    h2 ? do
      marginBottom    (px 2)
      textTransform   none
    li ? do
      marginTop       (em 3)
      firstChild &
        marginTop     (px 0)
    ul # byClass "info" ? do
      titleFamily
      "list-style" -: "none"
      marginTop       (px 3)
      marginBottom    (px 10)
      paddingLeft     (em 1)
      fontSize        (pt 10)
      li ? do
        display       inline
        marginLeft    (em 1.5)
        firstChild &
          marginLeft  (px 0)
        span ? do
          byClass "label" & do
            color      travisBlue
            marginRight (px 5)
          byClass "value" & do
            textDecoration underline

galleryStyles = do
  let prevNextCommon arrowImg = do position relative
                                   width (px prevNextWidth)
                                   background (url arrowImg, (noRepeat, placed sideCenter sideCenter))
                                   backgroundColor (grayish 0x66)
      prevNextWidth = 100

  star # ".modal" ? do
    overflow scroll
  star # "#gallery-prev" ? do
    prevNextCommon "/images/stock/left-arrow.png"
    float floatLeft
  star # "#gallery-next" ? do
    prevNextCommon  "/images/stock/right-arrow.png"
    float floatRight
  star # "#gallery-image" ? do
    minWidth (px (prevNextWidth * 2))
    minHeight (px (prevNextWidth * 2))
    marginLeft (px (-prevNextWidth))
    marginRight (px (-prevNextWidth))
  star # "#gallery-caption" ** star # ".gallery-figure" ? do
    fontWeight        bold
  star # "#gallery-caption" ?
    do narrowW $ do
        color   white
        position absolute
        bottom (px 0)
        sym padding (px 5)
        backgroundColor (rgba 0x33 0x33 0x33 192)
        fontSize (pt 18)
  narrowW $ do
    div # ".modal" ? do
      sym borderRadius (px 0)
      backgroundColor black
      sym padding (px 0)

main :: IO ()
main = putCss $ do
  body ? do
    bodyFamily
    sym margin        (px 0)
    normalW $ do
      paddingLeft       (px normalBodyPadding)
      paddingRight      (px normalBodyPadding)
    h2 ? do
      fontWeight      normal

  ul # byClass "blogroll" ? do
    "list-style"   -: "none"
    li ? do
      marginTop       (em 1)
      firstChild & do
        marginTop     (px 0)
  "#post" |> p ? do
      firstOfType & do
        titleFamily
        fontSize (pt 16)
        lineHeight (em 1.8)
      ":first-of-type:first-line" & do
        fontSize (pt 18)
        fontWeight bold
  div # "#content" ? do
    marginLeft        auto
    marginRight       auto
    fontSize   (pt 13)
    narrowW $ do
      paddingTop  (px contentPaddingTopNarrow)
      paddingLeft (px contentPaddingLeftNarrow)
      paddingRight (px contentPaddingRightNarrow)
      fontSize (pt 11)
    normalW $ do
      paddingTop      (px contentPaddingTop)
      paddingLeft     (px contentPaddingLeft)
      maxWidth (px contentMaxWidth)
      marginLeft (px contentMarginLeft)
    p ? do
      lineHeight (em 1.7)
      textAlign       justify
      -- Can't have overflow: hidden because it makes image wrapping funny
    div # ".header-image" ? do
      sym padding       (px 15)
      border            solid (px 1) travisLightGray
      float             floatRight
      sym margin        (px 10)
      width             (px 400)
      textAlign         (alignSide sideCenter)
      figureShadow
      ".caption" ? do
        fontSize        (pt 10)
        color           gray
        textAlign       (alignSide sideLeft)
    div # ".large-quote" ? do
      sym margin        (em 1)
      sym padding       (em 1)
      border            solid (px 1) travisLightGray
      insetBoxShadow    inset 0 0 (px 15) (setA 128 gray)
      ".quote-content" ? do
        titleFamily
        fontSize        (pt 20)
        display         block
      ".quote-source" ? do
        fontSize        (pt 10)
        color           gray
        display         block
        textAlign       (alignSide sideRight)
    div # ".gallery" |> div # ".figure" ? do
      sym margin (px 0)
      marginBottom (px 10)
    div # ".figure" ? do
--      figureShadow
      sym padding       (px 10)
      border            solid (px 1) travisLightGray
      sym margin        (px 5)
      width             (px 200)
      textAlign         (alignSide sideCenter)
      ".figure-centered" & do
        marginLeft      auto
        marginRight     auto
        width           auto
        img ? do width  auto
                 height auto
                 maxWidth none
                 maxHeight none
      ".figure-flow" & do
        marginRight     (em 2)
        float           floatLeft
      ".figure-right" & do
        float           floatRight
        marginLeft      (em 2)
        marginRight     (px 0)
      firstChild & do
        marginLeft      (px 0)
        marginTop       (px 0)
      img ? do
        maxWidth        (px 200)
        maxHeight       (px 200)
      p # ".caption" ? do
        lineHeight      (em 1.1)
        fontSize        (pt 10)
        narrowW $ do display none
        textAlign       (alignSide sideLeft)

    forM_ [ul, ol] (? do
                      li ? do
                        lineHeight    (em 1.4)
                        marginBottom  (em 1)
                        textAlign     justify
                        "last-child" & do
                             marginBottom (px 0))
    h1 ? do
      normalW $ do bodyFamily
                   fontWeight normal
      narrowW $ do titleFamily
                   fontSize (pt 16)
      marginBottom    (px 12)
    h2 ? do
      titleFamily
      fontWeight     bold
      textTransform   uppercase
      fontSize (pt 15)
      borderBottom    dotted (px 1) headerRuleColor
      overflow auto
    div # byClass "info" ? do
      paddingLeft     (em 2)
      fontStyle       italic
      color           gray
      span ? do
        byClass "author" & do
          color black
        byClass "date" & do
          color navy
      narrowW $ fontSize (pt 10)
    div # byClass "tags" ? do
      paddingLeft     (em 2)
      fontStyle       italic
      color           gray
      marginTop     (px 10)
      narrowW $ fontSize (pt 10)
      ul ? do
        color         black
        fontStyle     normal
        "list-style"  -: "none"
        sym padding   (px 0)
        sym margin    (px 0)
        display       inline
        li ? do
          a ? do
            color     black
          display     inline

    pre ? do
      overflowX       auto
      width           (pct 100)
      marginLeft      (px 20)
      sym padding     (em 1)
      lineHeight      (em 1.2)
      backgroundColor codeBackground
      fontSize        (pt 10)

  div # "#mini-header-bar" ? do
     normalW $ display none
     narrowW $ do position relative
                  width    (pct 100)
                  height   (px miniHeaderHeight)
                  backgroundColor black
                  titleFamily
                  star # "#show-header" ? do
                    color white
                    fontSize (px (miniHeaderHeight / 2))
                    width    (px (miniHeaderHeight / 2))
                    sym margin (px 7)
                    float floatRight
                    "cursor" -: "pointer"
                  h1 # "#mini-logo" ? do
                    plainLinks
                    fontSize (pt 14)
                    letterSpacing (px 5)
                    textTransform uppercase
                    fontWeight normal
                    display block
                    marginLeft auto
                    marginRight auto
                    marginTop (px 10)
                    textAlign (alignSide sideCenter)
                    p ? do
                      sym margin (px 0)
                      color travisBlue
                      display inline
                      ".travis" & do
                         color white
                         marginRight (px 15)

  div # "#header" ? do
    titleFamily
--    position          fixed
    overflow          auto
--    float             floatLeft
    width             (px (headerWidth - headerRightSpace))
    marginRight       (px headerRightSpace)
    position absolute
    narrowW $ do
      display none
      backgroundColor black
      color white
      textAlign (alignSide sideCenter)
      marginTop (px miniHeaderHeight)
      height    (pct 100)
      width     (pct 100)
      fontSize  (pt 28)
    div # byId "shamelessplug" ? do
      normalW $ fontSize (pt 8)
      narrowW $ fontSize (pt 10)
    h1 # byId "logo" ? do
      titleFamily
      -- display         block
      -- float           floatLeft
      narrowW $
        display none -- The header bar has this logo in the narrow display mode
      plainLinks
      fontSize        (pt 24)
      letterSpacing   (px 6)
      textTransform   uppercase
      display         block
      marginTop       (px 60)
      marginBottom    (px 10)
      p ? do
        sym margin    (px 0)
        marginBottom  (px 4)
        ":first-letter" & do
          fontSize    (pt 26)
          display     inlineBlock
        ".travis" & do
          color       travisLightGray

    div # byId "taglines" ? do
      fontSize        (pt 14)
      color           travisBlue

    ul # byId "navigation" ? do
      "list-style" -: "none"
      sym padding     (px 0)
      marginTop       (px 10)
      h2 ? do
        titleFamily
        textTransform uppercase
        sym margin    (px 0)
        marginBottom  (px 5)
        fontSize      (pt 13)
        narrowW $ do
             backgroundColor (grayish 0x33)
      li ? do
        marginTop     (px 10)
        firstChild &
          marginTop   (px 0)
      ul ? do
        "list-style" -: "none"
        normalW $ paddingLeft   (px 20)
        narrowW $ do
             paddingLeft (px 0)
             a ? color white
        li ? do
          narrowW $ fontSize (pt 14)
          marginTop   (px 4)
          firstChild &
            marginTop (px 0)

  projects
  galleryStyles
