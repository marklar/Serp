module Model where

import Array

type Pos = (Float,Float)
type Dir = {x:Int, y:Int}
data Light = Green | Red

-- What to do *next*.
type Input = { light : Light
             , arrowDir : Dir
             , applePos : Pos
             , elapsed : Time
             }

type LightInput = { space : Bool
                  , light : Light
                  }

type Snake = { hd : Pos
             , front : [Pos]
             , back : [Pos]
             , tl : Pos
             }

type State = { snakeDir : Dir
             , snake : Snake
             , light : Light
             , apple : Maybe Pos
             }

initSnake : Int -> Snake
initSnake numSegments =
    let init n = (0.0, 0.0 - ((toFloat n) * 8))
        segmentsAry = Array.initialize numSegments init
        len = Array.length segmentsAry
    in { hd    = Array.getOrElse (0,0) 0 segmentsAry
       , front = []
       , back  = reverse (Array.toList (Array.slice 1 (len-1) segmentsAry))
       , tl    = Array.getOrElse (0,0) (len-1) segmentsAry
       }

initState : State
initState = { snakeDir = {x=0, y=1}
            , snake = initSnake 20
            , light = Red
            , apple = Nothing
            }
