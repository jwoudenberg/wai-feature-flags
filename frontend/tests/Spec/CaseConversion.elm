module Spec.CaseConversion exposing (..)

import CaseConversion exposing (toSpaceCase)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "ConvertCase.toSpaceCase"
        [ test "camelCased string is converted to Space Case" <|
            \_ ->
                toSpaceCase "camelCasedString"
                    |> Expect.equal "Camel Cased String"
        , test "snake_cased string is converted to Space Case" <|
            \_ ->
                toSpaceCase "snake_cased_string"
                    |> Expect.equal "Snake Cased String"
        , test "camelCased string with numbers is converted to Space Case" <|
            \_ ->
                toSpaceCase "counting4Camels"
                    |> Expect.equal "Counting 4 Camels"
        , test "camelCased string with leading underscores is converted to Space Case" <|
            \_ ->
                toSpaceCase "___camelCasedString"
                    |> Expect.equal "Camel Cased String"
        , test "camelCased string with trailing underscores is converted to Space Case" <|
            \_ ->
                toSpaceCase "camelCasedString___"
                    |> Expect.equal "Camel Cased String"
        , test "camelCased string with leading capital is converted to Space Case" <|
            \_ ->
                toSpaceCase "CamelCasedString___"
                    |> Expect.equal "Camel Cased String"
        ]
