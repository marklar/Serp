module Display (display) where

import Config
    (collWd, collHt, bgColor,
     appleColor, appleRadius,
     snakeColor, snakeWidth)

import Model
    (Snake, Pos, Input, State)

----

display : State -> Element
display state =
    flow down [ asText state.light
              , asText state.apple
              , collage collWd collHt [ showApple state.apple
                                      , showSnake state.snake
                                      ]
              ]

-- Snake

showSnake : Snake -> Form
showSnake snake =
    traced snakeLineStyle (snakePath snake)

snakePath : Snake -> Path
snakePath {hd,front,back,tl} =
    (path (hd :: front ++ (reverse back) ++ [tl]))

snakeLineStyle : LineStyle
snakeLineStyle = { defaultLine | color <- snakeColor
                               , width <- snakeWidth
                               , cap   <- Round
                               , join  <- Smooth
                               }

-- Apple

showApple : Maybe Pos -> Form
showApple applePos =
    case applePos of
      Nothing -> nothingForm
      Just p  -> move p appleForm

appleForm : Form
appleForm = filled appleColor (circle appleRadius)

nothingForm : Form
nothingForm = alpha 0.0 (filled bgColor (circle 0))

--  (croppedImage (40,100) 40 80 "trev.png")
--  (croppedImage (80,60) 130 150 "zoe.png")
