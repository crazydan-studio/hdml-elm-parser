module BlockAttrTest exposing (suite)

import HDML.Block.Attr as Attr

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)


-- https://package.elm-lang.org/packages/elm-explorations/test/latest/Expect
suite : Test
suite =
    describe "HDML.Block.Attr"
        [test "create attribute" <|
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
                    authorEmail = "zhangsan@example.com"
                    attrs =
                        Attr.fromList
                            [ Attr.from "author" (Attr.None)
                                [ Attr.from "name" (Attr.String authorName) []
                                , Attr.from "email" (Attr.String authorEmail) []
                                ]
                            ]
                in
                    [case (Attr.val ["author", "name"] attrs) of
                        Attr.String v ->
                            v
                        _ ->
                            ""
                    , case (Attr.val ["author", "email"] attrs) of
                        Attr.String v ->
                            v
                        _ ->
                            ""
                    ]
                    |> Expect.equalLists [ authorName, authorEmail ]

        , test "create block attribute" <|
            \_ ->
                let
                    authorName = "张三"
                    raw = "@author " ++ authorName
                    blockAttr =
                        Attr.forBlock
                            [ Attr.from "author" (Attr.String authorName) []
                            ]
                            [ Attr.from "raw" (Attr.String raw) []
                            ]
                in
                    [case (Attr.val ["author"] (Attr.declaredOf blockAttr)) of
                        Attr.String v ->
                            v
                        _ ->
                            ""
                    , case (Attr.val ["raw"] (Attr.reservedOf blockAttr)) of
                        Attr.String v ->
                            v
                        _ ->
                            ""
                    ]
                    |> Expect.equalLists [ authorName, raw ]
        ]
