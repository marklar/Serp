module Inputs (stateSignal, newAppleSignal) where

import Keyboard
import Random

import Config (collWd, collHt, appleRadius, snakeWidth)
import Update (updateLightInput, updateState)
import Model (Pos, Input, State, Light, Red, LightInput, initState)

-- State

-- TODO: Move `stateSignal` into Main?
-- We'll want to move the apple into the State.
stateSignal : Signal State
stateSignal = foldp updateState initState inputSignal

inputSignal : Signal Input
inputSignal = Input <~ (dropRepeats lightSignal)
                     ~ (dropRepeats Keyboard.arrows)
                     ~ newAppleSignal
                     ~ elapsedSignal

elapsedSignal : Signal Time
elapsedSignal = inSeconds <~ fpsWhen 30 (isGreen <~ lightSignal)

isGreen : Light -> Bool
isGreen light = light /= Red


-- Apple

{-

Currently, newAppleSignal is computing Random numbers every 'interval' (1
second).  But those numbers are being used only when the snake eats the
apple, which will be much less frequently.

Maybe this is no big deal.  Or maybe it's an important inefficiency?

If it does matter, then how can we modify newAppleSignal to 'fire' only
when needed (i.e. when state.apple goes to Nothing)?

Random.range produces a new event only when the input signal
*changes*.  We would need a signal which reports when state.apple goes
to Nothing, using `sampleOn`.

-}

-- Updates all the time, even though usually goes unused.
newAppleSignal : Signal Pos
newAppleSignal = 
    let f a b = (toFloat a, toFloat b)
    in lift2 f
           (Random.range minX maxX intervalSignal)
           (Random.range minY maxY intervalSignal)

intervalSignal : Signal Time
intervalSignal = every second

(minX, maxX) = boundCoords collWd
(minY, maxY) = boundCoords collHt

boundCoords : Int -> (Int,Int)
boundCoords full =
    let half = round (toFloat full / 2)
    in ((10 - half), (half - 10))

-- Light
lightSignal : Signal Light
lightSignal = lift .light <|
                foldp updateLightInput (LightInput False Red) Keyboard.space
