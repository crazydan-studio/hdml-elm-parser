module BlockTest exposing (suite)

import HDML.Block as Block
import HDML.Block.Name as Name
import HDML.Block.Attrs as Attrs

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
                    blockName = Name.Paragraph
                    block =
                        Block.named
                            blockName
                            []
                            []
                in
                    Block.nameOf block
                    |> Expect.equal (Name.from "Paragraph")
        , test "create attribute" <|
            \_ ->
                let
                    lang = "zh_CN"
                    attrs =
                        Attrs.fromList
                            [ Attrs.from "lang" (Attrs.String lang) []
                            ]
                in
                    case (Attrs.get ["lang"] attrs) of
                        Just (Attrs.Attr name value) ->
                            case value of
                                Attrs.String v ->
                                    Expect.equal lang v
                                _ ->
                                    Expect.fail ("No value found for " ++ name)
                        Nothing ->
                            Expect.fail "No attribute found"
        , test "create nested attribute" <|
            \_ ->
                let
                    authorName = "张三"
                    attrs =
                        Attrs.fromList
                            [ Attrs.from "author" (Attrs.None)
                                [ Attrs.from "name" (Attrs.String authorName) []
                                ]
                            ]
                in
                    case (Attrs.val ["author", "name"] attrs) of
                        Attrs.String v ->
                            Expect.equal authorName v
                        _ ->
                            Expect.fail ("No value found for @author.name")
        ]
