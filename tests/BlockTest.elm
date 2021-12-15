module BlockTest exposing (suite)

import HDML.Block as Block
import HDML.Block.Name as Name
import HDML.Block.Attr as Attr

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)


-- https://package.elm-lang.org/packages/elm-explorations/test/latest/Expect
suite : Test
suite =
    describe "HDML.Block"
        [test "create empty block" <|
            \_ ->
                let
                    blockName = Name.Paragraph
                    block =
                        Block.named
                            blockName
                            []
                            []
                in
                    Block.nameOf block
                    |> Expect.equal (Name.from "Paragraph")
        ]
