module Update (updateState, updateLightInput) where

import Config (appleRadius, snakeWidth, snakeSpeed)
import Model (Input, LightInput, State, Snake, Pos, Dir, Green, Red)


-- LIGHT      

updateLightInput : Bool -> LightInput -> LightInput
updateLightInput newSpace {space,light} =
    { space = newSpace
    , light = case (newSpace, space, light) of
                (True, False, Red)   -> Green
                (True, False, Green) -> Red
                _                    -> light
    }


-- SNAKE

updateState : Input -> State -> State
updateState {light,arrowDir,applePos,elapsed} state =
    if state.light == Green
      then
          let newHeadPos = getSnakePos elapsed state
          in { snakeDir = getSnakeDir arrowDir state.snakeDir
             , snake = moveSnake newHeadPos state.snake
             -- , snake = pushSnake newHeadPos state.snake
             , light = light
             , apple = getAppleState state applePos
             }
      else { state | light <- light }

----

pushSnake : Pos -> Snake -> Snake
pushSnake newHeadPos snake =
    { snake | hd    <- newHeadPos
            , front <- snake.hd :: snake.front
            }

popSnake : Snake -> Snake
popSnake ({hd,front,back,tl} as snake) =
    case (front,back) of
      ([], []) -> snake
      (_, [])  -> let revFront = reverse front
                  in  { snake | front <- []
                              , back  <- tail revFront
                              , tl    <- head revFront
                              }
      otherwise -> { snake | back <- tail back
                           , tl   <- head back
                           }

moveSnake : Pos -> Snake -> Snake
moveSnake newHeadPos snake =
    snake |> pushSnake newHeadPos
          |> popSnake

turn : Int -> Int -> Int
turn arrowVal snakeVal =
    if snakeVal == 0 then arrowVal else snakeVal

-- May not reverse direction.  Only turn.
-- TODO: Use only l/r keys?
getSnakeDir : Dir -> Dir -> Dir
getSnakeDir arrowDir snakeDir =
    case (arrowDir.x, arrowDir.y) of
      (0,0) -> snakeDir
      (0,y) -> { x = 0, y = turn y snakeDir.y }
      (x,_) -> { y = 0, x = turn x snakeDir.x }

getSnakePos : Time -> State -> Pos
getSnakePos elapsed {snake,snakeDir} =
    let (x,y) = snake.hd
    in  (x + (travelDistance elapsed snakeDir.x),
         y + (travelDistance elapsed snakeDir.y))

travelDistance : Time -> Int -> Float
travelDistance elapsed dirVal =
    snakeSpeed * elapsed * (toFloat dirVal)

-- Apple

getAppleState : State -> Pos -> Maybe Pos
getAppleState state newApplePos =
    case state.apple of
      Nothing -> Just newApplePos
      Just applePos ->
          if isEating state.snake applePos
            then Just newApplePos
            else Just applePos

isEating : Snake -> Pos -> Bool
isEating snake applePos =
    maxDistance > (distance snake.hd applePos)

distance : Pos -> Pos -> Float
distance (ax,ay) (bx,by) =
    sqrt <| (ax - bx) ^ 2 + (ay - by) ^ 2
          
maxDistance = (appleRadius + snakeWidth) / 3

