module Display (display) where

import Model (Snake, Pos, Input, State, collWd, collHt)

snakeGreen : Color
snakeGreen = rgb 40 140 80

----

display : State -> Input -> Pos -> Element
display ({snake} as state) input apple =
    flow down [ asText input
              , asText apple
              , collage collWd collHt [ showSnake snake
                                      , showApple apple
                                      ]
              ]

----

showSnake : Snake -> Form
showSnake {hd,front,back,tl} =
    traced { defaultLine | color <- snakeGreen
                           , width <- 15
                           , cap   <- Round
                           , join  <- Smooth
                           }
             (path (hd :: front ++ (reverse back) ++ [tl]))

showApple : Pos -> Form
showApple applePos =
    move applePos (filled red (circle 10))

--  (croppedImage (40,100) 40 80 "trev.png")
--  (croppedImage (80,60) 130 150 "zoe.png")
