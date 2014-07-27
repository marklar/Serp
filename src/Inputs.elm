module Inputs (stateSignal, inputSignal, appleSignal) where

import Keyboard
import Random

import Update (updateLightInput, updateState)
import Model (Pos, Input, State, Light, Red, LightInput, initState, collWd, collHt)


-- TODO: Move `stateSignal` int Main?
-- We'll want to move the apple into the State.
stateSignal : Signal State
stateSignal = foldp updateState initState inputSignal

appleSignal : Signal Pos
appleSignal = 
    let f a b = (toFloat a, toFloat b)
    in lift2 f (Random.range minY maxX intervalSignal)
               (Random.range minY maxY intervalSignal)

inputSignal : Signal Input
inputSignal = Input <~ (dropRepeats lightSignal)
                     ~ (dropRepeats Keyboard.arrows)
                     ~ elapsedSignal

-----

elapsedSignal : Signal Time
elapsedSignal = inSeconds <~ fps 35

-- Apple
boundCoords : Int -> (Int,Int)
boundCoords full =
    let half = round (toFloat full / 2)
    in ((10 - half), (half - 10))

-- (Int,Int)
(minX, maxX) = boundCoords collWd
(minY, maxY) = boundCoords collHt

intervalSignal : Signal Time
intervalSignal = every (3000 * millisecond)


-- Light
lightSignal : Signal Light
lightSignal = lift .light <|
                foldp updateLightInput (LightInput False Red) Keyboard.space
