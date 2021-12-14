module HDML.Block exposing
    ( Block(..)
    , named
    , text
    , nameOf
    , blocksOf
    , declaredAttrsOf
    , reservedAttrsOf
    )

import HDML.Block.Name as Name
import HDML.Block.Attrs as Attrs

import Dict exposing (Dict)


-- 块结构
type Block a =
    Named Name.Name (Attrs.BlockAttr a) (Blocks a)
    | Text (Attrs.BlockAttr a) String

type alias Blocks a =
    List (Block a)


named : Name.Name -> Attrs.AttrList a -> Blocks a -> Block a
named name attrs blocks =
    Named
        name
        (Attrs.forBlock attrs [])
        blocks


text : Attrs.AttrList a -> String -> Block a
text attrs content =
    Text
        (Attrs.forBlock attrs [])
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


attrOf : Block a -> Attrs.BlockAttr a
attrOf block =
    case block of
        Named _ attr _ ->
            attr
        Text attr _ ->
            attr


declaredAttrsOf : Block a -> Attrs.AttrList a
declaredAttrsOf block =
    Attrs.declaredOf (attrOf block)


reservedAttrsOf : Block a -> Attrs.AttrList a
reservedAttrsOf block =
    Attrs.reservedOf (attrOf block)
