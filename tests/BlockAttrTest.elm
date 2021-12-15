module BlockAttrTest exposing (suite)

import HDML.Block.Attr as Attr
import HDML.Block.Attr.Value as AttrValue

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
                            [ Attr.from "lang" (AttrValue.asString lang) []
                            ]
                in
                    case (Attr.get ["lang"] attrs) of
                        Just (Attr.Attr name value) ->
                            Expect.equal (AttrValue.asString lang) value
                        Nothing ->
                            Expect.fail "No attribute found"

        , test "create nested attribute" <|
            \_ ->
                let
                    authorName = "张三"
                    authorEmail = "zhangsan@example.com"
                    attrs =
                        Attr.fromList
                            [ Attr.from "author" (AttrValue.asNone)
                                [ Attr.from "name" (AttrValue.asString authorName) []
                                , Attr.from "email" (AttrValue.asString authorEmail) []
                                ]
                            ]
                in
                    [ AttrValue.get ["author", "name"] attrs
                    , AttrValue.get ["author", "email"] attrs
                    ]
                    |> Expect.equalLists
                        [ AttrValue.asString authorName
                        , AttrValue.asString authorEmail
                        ]

        , test "create block attribute" <|
            \_ ->
                let
                    authorName = "张三"
                    raw = "@author " ++ authorName
                    blockAttr =
                        Attr.forBlock
                            [ Attr.from "author" (AttrValue.asString authorName) []
                            ]
                            [ Attr.from "raw" (AttrValue.asString raw) []
                            ]
                in
                    [ AttrValue.get ["author"] (Attr.declaredOf blockAttr)
                    , AttrValue.get ["raw"] (Attr.reservedOf blockAttr)
                    ]
                    |> Expect.equalLists
                        [ AttrValue.asString authorName
                        , AttrValue.asString raw
                        ]

        , test "set or update attribute value" <|
            \_ ->
                let
                    authorName = "张三"
                    newAuthorName = "李四"
                    authorEmail = "lisi@example.com"
                    attrs =
                        Attr.fromList
                            [ Attr.from "author" (AttrValue.asNone)
                                [ Attr.from "name" (AttrValue.asString authorName) []
                                ]
                            ]
                    updatedAttrs =
                        attrs
                        |> AttrValue.set (AttrValue.asString newAuthorName) ["author", "name"]
                        |> AttrValue.set (AttrValue.asString authorEmail) ["author", "email"]
                in
                    [ AttrValue.get ["author", "name"] attrs
                    , AttrValue.get ["author", "email"] attrs
                    , AttrValue.get ["author", "name"] updatedAttrs
                    , AttrValue.get ["author", "email"] updatedAttrs
                    ]
                    |> Expect.equalLists
                        [ AttrValue.asString authorName
                        , AttrValue.asNone
                        , AttrValue.asString newAuthorName
                        , AttrValue.asString authorEmail
                        ]
        ]
