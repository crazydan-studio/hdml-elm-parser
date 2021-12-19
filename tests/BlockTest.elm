module BlockTest exposing (suite)

import HDML.Block as Block
import HDML.Block.Name as Name
import HDML.Block.Attr as Attr
import HDML.Block.Attr.Value as AttrValue

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)


-- https://package.elm-lang.org/packages/elm-explorations/test/latest/Expect
suite : Test
suite =
    describe "HDML.Block"
        [ test "create empty block" <|
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

        , test "add block" <|
            \_ ->
                let
                    block =
                        Block.named
                            Name.Paragraph
                            []
                            []
                    subBlock =
                        Block.text
                            []
                            "这是一段文本"
                    newBlock =
                        Block.addBlock
                            subBlock
                            block
                in
                    Block.blocksOf newBlock
                    |> Expect.equalLists
                        [ subBlock
                        ]

        , test "add declared attribute to block" <|
            \_ ->
                let
                    authorName = "张三"
                    authorNameAttr = Attr.Attr "name" (AttrValue.asString authorName)
                    authorEmail = "zhangsan@example.com"
                    authorEmailAttr = Attr.Attr "email" (AttrValue.asString authorEmail)
                    text = "这是一段文本"
                    lineAttr = Attr.Attr "line" (AttrValue.asInt 10)
                    rawAttr = Attr.Attr "raw" (AttrValue.asString text)
                    block =
                        Block.named
                            Name.Paragraph
                            []
                            [ Block.text [] text
                            ]
                    newBlock =
                        block
                        |> Block.addDeclaredAttr authorNameAttr ["author"]
                        |> Block.addDeclaredAttr authorEmailAttr ["author"]
                        |> Block.addReservedAttr lineAttr []
                        |> Block.addReservedAttr rawAttr []
                    declaredAttr = Block.declaredAttrOf newBlock
                    reservedAttr = Block.reservedAttrOf newBlock
                in
                    [ AttrValue.get ["author"] declaredAttr
                    , AttrValue.get ["author", "name"] declaredAttr
                    , AttrValue.get ["author", "email"] declaredAttr
                    , AttrValue.get ["line"] reservedAttr
                    , AttrValue.get ["raw"] reservedAttr
                    ]
                    |> Expect.equalLists
                        [ AttrValue.asNone
                        , AttrValue.asString authorName
                        , AttrValue.asString authorEmail
                        , AttrValue.asInt 10
                        , AttrValue.asString text
                        ]
        ]
