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
import HDML.Block.Attr as Attr exposing
    ( AttrList, BlockAttr
    , forBlock, declaredOf, reservedOf
    )
import HDML.Block.Attr.Internal as Internal exposing
    ( AttrTree
    )

import Dict exposing (Dict)


-- 块结构
type Block a =
    Named Name.Name (BlockAttr a) (Blocks a)
    | Text (BlockAttr a) String

type alias Blocks a =
    List (Block a)


named : Name.Name -> AttrList a -> Blocks a -> Block a
named name attrs blocks =
    Named
        name
        (forBlock attrs [])
        blocks


text : AttrList a -> String -> Block a
text attrs content =
    Text
        (forBlock attrs [])
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


attrOf : Block a -> BlockAttr a
attrOf block =
    case block of
        Named _ attr _ ->
            attr
        Text attr _ ->
            attr


declaredAttrOf : Block a -> AttrTree a
declaredAttrOf block =
    declaredOf (attrOf block)


reservedAttrOf : Block a -> AttrTree a
reservedAttrOf block =
    reservedOf (attrOf block)
