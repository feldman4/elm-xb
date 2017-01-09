module Valley exposing (..)

import List.Extra
import Html exposing (text)
import EveryDict


type alias Face =
    { x : Int, y : Int, z : Int }


type alias Ortho =
    Int


type alias Path =
    List Face


main : Html.Html msg
main =
    testFindPath |> toString |> text


findPath : Ortho -> List Face -> Face -> Face -> Maybe Path
findPath ortho faces a b =
    findPathBF (isLinked ortho) faces a b


testFindPath : Maybe (List Int)
testFindPath =
    let
        test a b =
            abs (a - b) < 2

        points =
            List.range 0 10
    in
        dijkstra test points 1 7


type alias DijkstraData a =
    { queue : List a
    , distance : EveryDict.EveryDict a Float
    , parent : EveryDict.EveryDict a (Maybe a)
    }


{-| Pops the closest node out of the queue, returning a sorted queue.
-}
next : DijkstraData a -> Maybe ( a, DijkstraData a )
next ({ queue, distance } as data) =
    let
        getDistance node =
            EveryDict.get node distance |> Maybe.withDefault (1 / 0)
    in
        case data.queue |> List.sortBy getDistance of
            x :: xs ->
                Just ( x, { data | queue = xs } )

            [] ->
                Nothing


{-| recursively traces marked nodes back to start
-}
unwind : EveryDict.EveryDict a (Maybe a) -> List a -> a -> Maybe (List a)
unwind parent soFar start =
    case soFar of
        x :: xs ->
            case EveryDict.get x parent of
                Just (Just p) ->
                    if p == start then
                        Just (p :: soFar)
                    else
                        unwind parent (p :: soFar) start

                _ ->
                    Nothing

        [] ->
            Nothing


{-| Would be nice to use elm-community/graph. Not sure if there is a way to use
the built-in `bfs` traversal with min-priority queue.
-}
dijkstra : (a -> a -> Bool) -> List a -> a -> a -> Maybe (List a)
dijkstra test nodes start goal =
    let
        initialData : DijkstraData a
        initialData =
            { queue = [ start ]
            , distance =
                nodes
                    |> List.map (\k -> ( k, 1 / 0 ))
                    |> EveryDict.fromList
                    |> EveryDict.insert start 0
            , parent = nodes |> List.map (\k -> ( k, Nothing )) |> EveryDict.fromList
            }

        -- process a node: put neighbors in queue, mark distance and parent
        process : DijkstraData a -> a -> DijkstraData a
        process ({ queue, distance, parent } as data) node =
            let
                unvisited n =
                    EveryDict.get n distance
                        |> Maybe.withDefault (1 / 0)
                        |> isInfinite

                currentDistance =
                    EveryDict.get node distance
                        |> Maybe.withDefault (1 / 0)

                visit n data_ =
                    let
                        -- both distance and parent are set if the distance is lowest
                        d =
                            EveryDict.get n data_.distance
                                |> Maybe.withDefault (1 / 0)
                                |> min (currentDistance + 1)

                        newParent =
                            if d == currentDistance + 1 then
                                EveryDict.insert n (Just node) data_.parent
                            else
                                parent
                    in
                        { data_
                            | distance = EveryDict.insert n d data_.distance
                            , parent = newParent
                            , queue = n :: data_.queue
                        }
            in
                nodes
                    |> List.filter (test node)
                    |> List.filter unvisited
                    |> List.foldl visit data

        -- main loop
        step : DijkstraData a -> Maybe (List a)
        step ({ queue, distance, parent } as data) =
            case next (Debug.log "data" data) of
                Just ( node, data2 ) ->
                    if node == goal then
                        let
                            -- do this update
                            data3 =
                                process data2 node
                        in
                            unwind data3.parent [ node ] start
                    else
                        process data2 node |> step

                Nothing ->
                    case EveryDict.get goal parent of
                        Just (Just node) ->
                            unwind parent [ node ] start

                        _ ->
                            Nothing
    in
        step initialData


{-| Brute force breadth-first solution. Use `dijkstra` instead.
-}
findPathBF : (a -> a -> Bool) -> List a -> a -> a -> Maybe (List a)
findPathBF test items a b =
    let
        neighbors : a -> List a
        neighbors x =
            items |> List.filter (test x)

        -- breadth-first search
        extendPath : a -> List (List a) -> Maybe (List a)
        extendPath goal soFar =
            let
                atGoal xs =
                    case List.head xs of
                        Just x ->
                            if x == goal then
                                True
                            else
                                False

                        Nothing ->
                            False

                addNeighbors xs =
                    case xs |> List.head of
                        Just x ->
                            neighbors x
                                |> List.map (\y -> y :: xs)

                        Nothing ->
                            []
            in
                case soFar |> List.Extra.find atGoal of
                    Just solution ->
                        Just solution

                    Nothing ->
                        extendPath goal (soFar |> List.concatMap addNeighbors)
    in
        extendPath b [ [ a ] ]


isLinked : Ortho -> Face -> Face -> Bool
isLinked ortho a b =
    if a == b then
        False
    else
        True
