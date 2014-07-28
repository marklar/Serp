module Model where

import Array

----

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

{-

TODO: Rather than storing each position,
store only where the vertices (head, tail, and turns) are.

One way to do that is to record (Pos, Dir).
That is:
  1. where the body part is at that moment, and
  2. what direction it came from to get there.

If pushing a new value:
  * whose Dir is different from the previous, simply push.
    You have a new vertex.
  * whose Dir is the same as the previous,
    then you can simply replace the previous with the new one
    (thus extending that segment of the Path).

When removing a value, move it closer by the appropriate amount
to the subsequent.  If doing so makes its Pos equal to or *pass*
the other, then simply remove it.  To see whether it goes past,
look at the subsequent's Dir, and see whether the sign of the
delta in that direction changes.

-}

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
