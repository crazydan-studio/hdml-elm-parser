module HDML.Block exposing
    ( Block(..)
    , named, text
    , nameOf, blocksOf
    , declaredAttrOf, reservedAttrOf
    , addDeclaredAttr, addReservedAttr
    , addBlock
    )

import HDML.Block.Name as Name
import HDML.Block.Attr as Attr exposing
    ( AttrList, BlockAttr
    , forBlock, declaredOf, reservedOf
    , addDeclared, addReserved
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
        Named _ blockAttr _ ->
            blockAttr
        Text blockAttr _ ->
            blockAttr


declaredAttrOf : Block a -> AttrTree a
declaredAttrOf block =
    declaredOf (attrOf block)


reservedAttrOf : Block a -> AttrTree a
reservedAttrOf block =
    reservedOf (attrOf block)


addDeclaredAttr : Attr.Attr a -> List String -> Block a -> Block a
addDeclaredAttr attr paths block =
    let
        declared = addDeclared attr paths (attrOf block)
    in
        case block of
            Named name _ blocks ->
                (Named name declared blocks)
            Text _ content ->
                (Text declared content)


addReservedAttr : Attr.Attr a -> List String -> Block a -> Block a
addReservedAttr attr paths block =
    let
        reserved = addReserved attr paths (attrOf block)
    in
        case block of
            Named name _ blocks ->
                (Named name reserved blocks)
            Text _ content ->
                (Text reserved content)

addBlock : Block a -> Block a -> Block a
addBlock subBlock topBlock =
    case topBlock of
        Named name blockAttr blocks ->
            (Named name blockAttr (blocks ++ [subBlock]))
        _ ->
            topBlock
