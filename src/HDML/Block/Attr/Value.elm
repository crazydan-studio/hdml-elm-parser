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
    case paths of
        [] ->
            topTree
        name :: subPaths ->
            let
                topTreeNode =
                    Dict.get name topTree
            in
                case topTreeNode of
                    Nothing ->
                        if List.isEmpty subPaths then
                            Dict.insert name (AttrTreeNode value Dict.empty) topTree
                        else
                            let
                                subTree = set value subPaths Dict.empty
                            in
                                Dict.insert name (AttrTreeNode None subTree) topTree
                    Just (AttrTreeNode nodeValue subTree) ->
                        if List.isEmpty subPaths then
                            Dict.insert name (AttrTreeNode value subTree) topTree
                        else
                            let
                                newSubTree = set value subPaths subTree
                            in
                                Dict.insert name (AttrTreeNode nodeValue newSubTree) topTree
