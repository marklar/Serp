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
              didEat = isEating state.snake state.apple
              snakeFn = if didEat then pushSnake else moveSnake
          in { snakeDir = getSnakeDir arrowDir state.snakeDir
             , snake = snakeFn newHeadPos state.snake
             , light = light
             , apple = case (didEat, state.apple) of
                         (True, _) -> Nothing -- Just applePos
                         (_, Nothing) -> Just applePos
                         (_, _) -> state.apple
             }
      else { state | light <- light }

isEating : Snake -> Maybe Pos -> Bool
isEating snake apple =
    case apple of
      Nothing -> False
      Just pos -> maxDistance > (distance snake.hd pos)

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

distance : Pos -> Pos -> Float
distance (ax,ay) (bx,by) =
    sqrt <| (ax - bx) ^ 2 + (ay - by) ^ 2
          
maxDistance = (appleRadius + snakeWidth) / 2

