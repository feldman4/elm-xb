module CFG exposing (..)

import Html exposing (text)
import Debug
import List.Extra
import EveryDict exposing (EveryDict)


type Symbol
    = S
    | N
    | N_
    | A
    | NPL
    | PP
    | Place
    | Terminal Word


type alias Word =
    String


type alias Vocab =
    List Word


type alias Sentence =
    List Symbol


type alias Production =
    ( Symbol, List Symbol )


type alias Grammar =
    List Production


exampleCFG : Grammar
exampleCFG =
    [ ( S, [ N_ ] )
    , ( N_, [ N ] )
    , ( N_, [ A, N ] )
    , ( S, [ N_, PP ] )
    , ( PP, [ Terminal "from", Place ] )
    , ( Place, [ Terminal "the Back of Beyond" ] )
    , ( Place, [ Terminal "the Hollow Earth" ] )
    , ( Place, [ Terminal "a One-Horse Town" ] )
    , ( A, [ Terminal "Predatory" ] )
    , ( A, [ Terminal "Lonely" ] )
    , ( A, [ Terminal "Disguised" ] )
    , ( A, [ Terminal "Candy-Assed" ] )
    , ( A, [ Terminal "Unsuccessful" ] )
    , ( A, [ Terminal "Talentless" ] )
    , ( NPL, [ Terminal "Dwarves" ] )
    , ( NPL, [ Terminal "Liars" ] )
    , ( NPL, [ Terminal "Rapists" ] )
    , ( N, [ Terminal "Latecomer" ] )
    , ( N, [ Terminal "Miscreant" ] )
    , ( N, [ Terminal "Liar" ] )
    , ( N, [ Terminal "Regurgitator" ] )
    , ( N, [ Terminal "Carrion" ] )
    , ( N, [ Terminal "Filth" ] )
    , ( N, [ Terminal "Traitor" ] )
    , ( N, [ Terminal "Neophyte" ] )
    ]


getTerminals : Grammar -> List Word
getTerminals grammar =
    let
        terminal x =
            case x of
                Terminal s ->
                    [ s ]

                _ ->
                    []
    in
        List.concatMap Tuple.second grammar
            |> List.concatMap terminal


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


replaceAt : Int -> List a -> List a -> List a
replaceAt pos x y =
    let
        before =
            List.take pos x

        after =
            List.drop (pos + 1) x
    in
        before ++ y ++ after


superUnique : List a -> List a
superUnique x =
    EveryDict.fromList (List.map (\y -> ( y, 0 )) x)
        |> EveryDict.toList
        |> List.map Tuple.first


{-| Given a partially completed sentence, find the set of terminals
    that could come next. If the sentence is complete as is, the list of
    choices includes Terminal "".
-}
choices : Grammar -> Symbol -> Sentence -> List Word
choices grammar top soFar =
    let
        keep sentence =
            not (isMismatch grammar soFar sentence)

        halt sentence =
            isValidExtension grammar soFar sentence

        minLength s =
            (List.length s) >= (List.length soFar)

        winners =
            choiceIterator grammar keep halt [ top ]

        options =
            winners
                |> List.filter minLength
                |> List.map (List.drop (List.length soFar))
                |> List.map List.head
                |> List.map (Maybe.withDefault (Terminal ""))

        toTerminal s =
            case s of
                Terminal x ->
                    x

                _ ->
                    "non-terminal-" ++ (toString s)
    in
        options |> superUnique |> List.map toTerminal


choiceIterator : Grammar -> (Sentence -> Bool) -> (Sentence -> Bool) -> Sentence -> List Sentence
choiceIterator grammar keep halt sentence =
    let
        extensions =
            extendFirst grammar sentence

        candidates =
            extensions
                |> List.filter keep

        ( halted, goOn ) =
            List.partition halt candidates

        -- discard =
        --     extensions
        --         |> List.filter (\x -> not (keep x))
        -- a =
        --     Debug.log "discard, halted, goOn" ( discard, halted, goOn )
        nextHalted =
            List.concatMap (choiceIterator grammar keep halt) goOn
    in
        (halted ++ nextHalted)


{-| Find all extensions of a sentence reached by
applying one production.
-}
extend : Grammar -> Sentence -> List Sentence
extend grammar sentence =
    let
        f i s =
            let
                chains =
                    getChains grammar s
            in
                List.map (replaceAt i sentence) chains
    in
        List.indexedMap f sentence |> List.concat


{-| Find extensions of a sentence reached by applying just the first
production that matches from the left.
-}
extendFirst : Grammar -> Sentence -> List Sentence
extendFirst grammar sentence =
    let
        f s =
            let
                prefix =
                    terminalPrefix sentence |> List.length

                firstNonTerminal =
                    List.drop prefix sentence |> List.head

                chains =
                    getChains grammar (firstNonTerminal |> Maybe.withDefault (Terminal ""))
            in
                List.map (replaceAt prefix sentence) chains
    in
        f sentence


{-| Checks if list of contiguous terminals at the start of candidate sentence
match the sentence soFar. Used to remove wrong branches when expanding
a grammar to match the sentence soFar.
-}
isMismatch : Grammar -> Sentence -> Sentence -> Bool
isMismatch grammar soFar sentence =
    let
        prefix =
            terminalPrefix sentence
    in
        (List.map2 (==) soFar prefix
            |> List.all identity
            |> not
        )


{-| Check if sentence matches terminals in so_far, and the next symbol
    is a terminal.
-}
isValidExtension : Grammar -> Sentence -> Sentence -> Bool
isValidExtension grammar soFar sentence =
    let
        matched =
            List.map2 (==) soFar sentence
                |> List.all identity

        next =
            List.drop (List.length soFar) sentence
                |> List.head
    in
        case next of
            Just (Terminal s) ->
                matched

            Nothing ->
                matched

            _ ->
                False


terminalPrefix : Sentence -> Sentence
terminalPrefix sentence =
    List.Extra.takeWhile
        (\s ->
            case s of
                Terminal s ->
                    True

                _ ->
                    False
        )
        sentence


{-| Find all productions from a given symbol.
-}
getChains : Grammar -> Symbol -> List (List Symbol)
getChains grammar lhs =
    grammar
        |> List.filter (\( s, _ ) -> s == lhs)
        |> List.map Tuple.second


main : Html.Html msg
main =
    let
        z =
            Debug.log "choices" (choices exampleCFG S [ Terminal "Disguised", Terminal "Carrion", Terminal "from" ])
    in
        text "Hello, World"
