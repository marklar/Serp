module Main where

import Display (display)
import Inputs (stateSignal)

main : Signal Element
main = display <~ stateSignal
