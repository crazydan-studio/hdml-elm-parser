module HDML.Block exposing
    ( Block(..)
    , named
    , text
    , nameOf
    , blocksOf
    , declaredAttrOf
    , reservedAttrOf
    )

import HDML.Block.Name as Name
import HDML.Block.Attr as Attr

import Dict exposing (Dict)


-- 块结构
type Block a =
    Named Name.Name (Attr.BlockAttr a) (Blocks a)
    | Text (Attr.BlockAttr a) String

type alias Blocks a =
    List (Block a)


named : Name.Name -> Attr.AttrList a -> Blocks a -> Block a
named name attrs blocks =
    Named
        name
        (Attr.forBlock attrs [])
        blocks


text : Attr.AttrList a -> String -> Block a
text attrs content =
    Text
        (Attr.forBlock attrs [])
        content


nameOf : Block a -> Name.Name
nameOf block =
    case block of
        Named name _ _ ->
            name
        Text _ _ ->
            Name.Text


blocksOf : Block a -> Blocks a
blocksOf block =
    case block of
        Named _ _ blocks ->
            blocks
        _ ->
            []


attrOf : Block a -> Attr.BlockAttr a
attrOf block =
    case block of
        Named _ attr _ ->
            attr
        Text attr _ ->
            attr


declaredAttrOf : Block a -> Attr.AttrTree a
declaredAttrOf block =
    Attr.declaredOf (attrOf block)


reservedAttrOf : Block a -> Attr.AttrTree a
reservedAttrOf block =
    Attr.reservedOf (attrOf block)
