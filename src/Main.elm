module Main where

import Display (display)
import Inputs (stateSignal, inputSignal, appleSignal)

main : Signal Element
main = display <~ stateSignal ~ inputSignal ~ appleSignal
