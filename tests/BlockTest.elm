module BlockTest exposing (suite)

import HDML.Block as Block
import HDML.Block.Name as BlockName
import HDML.Block.Attrs as BlockAttrs exposing
    ( AttrValue(..)
    )

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
                    blockName = BlockName.Paragraph
                    block =
                        Block.named
                            blockName
                            [ BlockAttrs.Attr "indent" (WithInt 4) []
                            ]
                            []
                in
                -- Expect.equal block (Block.text [] "")
                Block.nameOf block
                |> Expect.equal (BlockName.from "Paragraph")
        ]
