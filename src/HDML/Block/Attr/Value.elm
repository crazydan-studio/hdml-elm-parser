module HDML.Block.Attr.Value exposing
    ( get, set
    , asInt, asFloat, asBool, asString, asNone
    )

import HDML.Block.Attr as Attr
import HDML.Block.Attr.Internal as Internal exposing
    ( AttrTree, AttrTreeNode(..)
    , AttrValue(..)
    )

import Dict exposing (Dict)


-- 属性值类型转换
asInt : Int -> AttrValue
asInt i =
    Internal.Integer i

asFloat : Float -> AttrValue
asFloat f =
    Internal.Float f

asBool : Bool -> AttrValue
asBool b =
    Internal.Bool b

asString : String -> AttrValue
asString s =
    Internal.String s

asNone : AttrValue
asNone =
    Internal.None


{-| 获取指定属性的值
-}
get : List String -> AttrTree a -> AttrValue
get paths topTree =
    case (Attr.get paths topTree) of
        Nothing ->
            None
        Just (Attr.Attr name value) ->
            value


{-| 设置指定属性的值
-}
set : AttrValue -> List String -> AttrTree a -> AttrTree a
set value paths topTree =
    setForwardHelper value paths [] topTree []

-- 以尾递归方式为属性赋值，支持创建不存在的节点
setForwardHelper : AttrValue -> List String -> List String
                    -> AttrTree a -> List (AttrTreeNode a)
                    -> AttrTree a
setForwardHelper value forwardPaths backwardPaths topTree treeNodes =
    case forwardPaths of
        -- 沿路径向前到达树的底端，不存在的节点，则主动创建
        name :: subForwardPaths ->
            let
                topTreeValue =
                    if (List.isEmpty subForwardPaths) then
                        value
                    else
                        get [name] topTree
                subTree =
                    case (Dict.get name topTree) of
                        Nothing ->
                            Dict.empty
                        Just (AttrTreeNode _ tree) ->
                            tree

                newTreeNodes =
                    (AttrTreeNode topTreeValue topTree) :: treeNodes
            in
                setForwardHelper
                    value
                    subForwardPaths
                    (name :: backwardPaths)
                    subTree newTreeNodes

        -- 到达树的底端，依次为经过的节点重新赋值，直到后退至树的根节点
        [] ->
            setBackwardHelper backwardPaths topTree treeNodes

setBackwardHelper : List String -> AttrTree a -> List (AttrTreeNode a)
                    -> AttrTree a
setBackwardHelper backwardPaths topTree treeNodes =
    case backwardPaths of
        [] ->
            topTree
        name :: subBackwardPaths ->
            case treeNodes of
                [] ->
                    topTree
                treeNode :: leftTreeNodes ->
                    let
                        newTopTree =
                            case treeNode of
                                (AttrTreeNode topTreeValue upTopTree) ->
                                    Dict.insert
                                        name
                                        (AttrTreeNode topTreeValue topTree)
                                        upTopTree
                    in
                        setBackwardHelper
                            subBackwardPaths
                            newTopTree
                            leftTreeNodes
