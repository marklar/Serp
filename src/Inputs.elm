module Inputs (stateSignal, newAppleSignal) where

import Keyboard
import Random

import Config (collWd, collHt, appleRadius, snakeWidth)
import Update (updateLightInput, updateState)
import Model (Pos, Input, State, Light, Red, LightInput, initState)


-- State

-- TODO: Move `stateSignal` into Main?
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
