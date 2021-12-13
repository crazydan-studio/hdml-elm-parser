module HDML.Block exposing
    ( Block(..)
    , named
    , text
    , nameOf
    , blocksOf
    , attrsOf
    )

import HDML.Block.Name as Name
import HDML.Block.Attrs as Attrs

import Dict exposing (Dict)


-- 块结构
type Block a =
    Named Name.Name (Attrs.BlockAttrs a) (Blocks a)
    | Text (Attrs.BlockAttrs a) String

type alias Blocks a =
    List (Block a)


named : Name.Name -> Attrs.AttrList a -> Blocks a -> Block a
named name attrs blocks =
    Named
        name
        (Attrs.attrsToBlockAttrs attrs)
        blocks


text : Attrs.AttrList a -> String -> Block a
text attrs content =
    Text
        (Attrs.attrsToBlockAttrs attrs)
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


attrsOf : Block a -> Attrs.BlockAttrs a
attrsOf block =
    case block of
        Named _ attrs _ ->
            attrs
        Text attrs _ ->
            attrs


declaredAttrsOf : Block a -> Attrs.AttrList a
declaredAttrsOf block =
    case (attrsOf block) of
        { declared, reserved } ->
            []


reservedAttrsOf : Block a -> Attrs.AttrList a
reservedAttrsOf block =
    case (attrsOf block) of
        { declared, reserved } ->
            []
