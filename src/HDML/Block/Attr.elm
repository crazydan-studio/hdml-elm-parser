module HDML.Block.Attr exposing
    ( Attr(..), AttrNested(..)
    , from, get, set
    , AttrList
    , fromList, toList
    , BlockAttr, forBlock, declaredOf, reservedOf
    )

import HDML.Block.Attr.Internal as Internal exposing
    ( AttrTree, AttrTreeNode(..)
    , AttrValue(..)
    )

import Dict exposing (Dict)


{-| 属性
-}
type Attr a =
    Attr String AttrValue

type AttrNested a =
    AttrNested (Attr a) (AttrList a)

type alias AttrList a =
    List (AttrNested a)


{-| 块属性
-}
type alias BlockAttr a =
    {
    -- 在文档内声明的属性
    declared: AttrTree a
    -- 保留属性，用于记录文档原始内容、原始标记等信息
    , reserved: AttrTree a
    }


from : String -> AttrValue -> AttrList a -> AttrNested a
from name value subs =
    AttrNested (Attr name value) subs


fromList : AttrList a -> AttrTree a
fromList attrs =
    fromListHelper attrs Dict.empty

-- 无法使用尾递归，好在属性的层级关系不会很深
fromListHelper : AttrList a -> AttrTree a -> AttrTree a
fromListHelper attrs tree =
    case attrs of
        [] ->
            tree
        (AttrNested (Attr name value) subs) :: tail ->
            let
                subTree = fromListHelper subs Dict.empty
                topTree =
                    Dict.insert name (AttrTreeNode value subTree) tree
            in
                fromListHelper tail topTree


toList : AttrTree a -> AttrList a
toList topTree =
    Dict.foldl
        (\name (AttrTreeNode value subTree) attrs ->
            let
                subs =
                    toList subTree
            in
                (AttrNested (Attr name value) subs) :: attrs
        )
        []
        topTree


forBlock : AttrList a -> AttrList a -> BlockAttr a
forBlock declared reserved =
    { declared = fromList declared
    , reserved = fromList reserved
    }


declaredOf : BlockAttr a -> AttrTree a
declaredOf attr =
    case attr of
        { declared, reserved } ->
            declared


reservedOf : BlockAttr a -> AttrTree a
reservedOf attr =
    case attr of
        { declared, reserved } ->
            reserved


{-| 获取指定的属性
-}
get : List String -> AttrTree a -> Maybe (Attr a)
get paths topTree =
    case paths of
        [] ->
            Nothing
        name :: subPaths ->
            case (Dict.get name topTree) of
                Nothing ->
                    Nothing
                Just (AttrTreeNode nodeValue subTree) ->
                    if List.isEmpty subPaths then
                        Just (Attr name nodeValue)
                    else
                        get subPaths subTree


{-| 添加或覆盖指定的属性
-}
set : Attr a -> List String -> AttrTree a -> AttrTree a
set attr paths topTree =
    topTree


-- travel =
