port module Main exposing (..)

import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Test exposing (..)

import XsdTypes

main : Program Value
main =
    run emit allTests

allTests : Test
allTests = describe "Xml Unit Test Suite - All"
           [ XsdTypes.allTests
           ]

port emit : ( String, Value ) -> Cmd msg
