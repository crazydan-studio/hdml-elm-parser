module BlockTest exposing (suite)

import HDML.Block as Block
import HDML.Block.Name as Name
import HDML.Block.Attr as Attr

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
                        Attr.fromList
                            [ Attr.from "lang" (Attr.String lang) []
                            ]
                in
                    case (Attr.get ["lang"] attrs) of
                        Just (Attr.Attr name value) ->
                            case value of
                                Attr.String v ->
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
                        Attr.fromList
                            [ Attr.from "author" (Attr.None)
                                [ Attr.from "name" (Attr.String authorName) []
                                ]
                            ]
                in
                    case (Attr.val ["author", "name"] attrs) of
                        Attr.String v ->
                            Expect.equal authorName v
                        _ ->
                            Expect.fail ("No value found for @author.name")
        ]
