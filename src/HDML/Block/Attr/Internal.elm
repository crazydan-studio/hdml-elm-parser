module HDML.Block.Attr.Internal exposing
    ( AttrValue(..)
    , AttrTree
    , AttrTreeNode(..)
    )

import Dict exposing (Dict)


type AttrValue =
    Integer Int
    | Float Float
    | Bool Bool
    | String String
    | None


-- 属性的树形结构
type AttrTreeNode a =
    AttrTreeNode AttrValue (AttrTree a)

type alias AttrTree a =
    Dict String (AttrTreeNode a)
