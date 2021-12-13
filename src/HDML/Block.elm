module HDML.Block exposing
    ( Block(..)
    , SubAttrs(..)
    , named
    , anonymous
    , text
    )

import Dict exposing (Dict)


-- 块结构
type Block a =
    Named String (BlockAttrs a) (Blocks a)
    | Anonymous (BlockAttrs a) (Blocks a)
    | Text (BlockAttrs a) String

type alias Blocks a =
    List (Block a)


-- 块属性
type alias BlockAttrs a =
    { pub: Attrs a
    , priv: Attrs a
    }

type alias Attrs a =
    List (String, a, SubAttrs a)

type SubAttrs a =
    SubAttrs (Attrs a)


-- 内部块属性的字典结构
type Attr_ a =
    Attr_ (Maybe a) (AttrDict_ a)

type alias AttrDict_ a =
    Dict String (Attr_ a)


named : String -> Attrs a -> Blocks a -> Block a
named name attrs blocks =
    Named
        name
        (attrsToBlockAttrs attrs)
        blocks


anonymous : Attrs a -> Blocks a -> Block a
anonymous attrs blocks =
    Anonymous
        (attrsToBlockAttrs attrs)
        blocks


text : Attrs a -> String -> Block a
text attrs content =
    Text
        (attrsToBlockAttrs attrs)
        content


attrsToBlockAttrs : Attrs a -> BlockAttrs a
attrsToBlockAttrs attrs =
    { pub = attrs
    , priv = []
    }
