module Dict.Extra exposing (groupBy, removeWhen)

{-|
Convenience functions for working with Dict
# List operations
@docs groupBy
# Manipulation
@docs removeWhen
-}

--Skinney/elm-dict-extra 0.18

import Dict exposing (Dict)


{-| Takes a key-fn and a list, creates a Dict which maps the key returned from key-fn, to a list of matching elements.
    mary = {groupId: 1, name: "Mary"}
    jack = {groupId: 2, name: "Jack"}
    jill = {groupId: 1, name: "Jill"}
    groupBy .groupId [mary, jack, jill] == Dict.fromList [(2, [jack]), (1, [mary, jill])]
-}
groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy keyfn list =
    groupByHelp keyfn list Dict.empty


groupByHelp : (a -> comparable) -> List a -> Dict comparable (List a) -> Dict comparable (List a)
groupByHelp keyfn list acc =
    case list of
        [] ->
            acc

        x :: xs ->
            let
                currentKey =
                    keyfn x

                ( newEntry, remains ) =
                    List.partition (\a -> keyfn a == currentKey) list

                newAcc =
                    Dict.insert currentKey newEntry acc
            in
                groupByHelp keyfn remains newAcc


{-| Keep elements which fails to satisfy the predicate.
    This is functionaly equivalent to `Dict.filter (not << predicate) dict`.
    removeWhen (\c v -> v == 1) Dict.fromList [("Mary", 1), ("Jack", 2), ("Jill", 1)] == Dict.fromList [("Jack", 2)]
-}
removeWhen : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
removeWhen pred dict =
    Dict.filter (\c v -> not (pred c v)) dict
