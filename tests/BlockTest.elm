module BlockTest exposing (suite)

import HDML.Block as Block

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "HDML.Block"
        [test "create empty block" <|
            \_ ->
                let
                    blockName = "Paragraph"
                    emptyBlock =
                        Block.Named blockName
                            [ ("indent", (Block.Attr 4 []))
                            ]
                            []
                in
                case emptyBlock of
                    Block.Named name _ _ ->
                        Expect.equal blockName name
                    _ ->
                        Expect.fail "No block matched"
        ]
