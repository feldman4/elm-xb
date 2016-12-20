module Interaction exposing (..)

import Typewriter.Typewriter as Speech
import Random.Pcg as Random
import PTree exposing (kick, STree)
import Math.Vector3 as V3


type Interaction
    = SpeechEvent (Maybe Speech.Event)


type alias ModelWithForest a =
    { a | forest : List PTree.STree, seed : Random.Seed }


speechToTree : Interaction -> ( ModelWithForest a, b ) -> ( ModelWithForest a, b )
speechToTree interaction ( model, msgs ) =
    case interaction of
        SpeechEvent event ->
            case event of
                Just (Speech.SentenceAdded) ->
                    ( kickTree model, msgs )

                _ ->
                    ( model, msgs )


kickTree : ModelWithForest a -> ModelWithForest a
kickTree model =
    let
        gen =
            Random.int 0 (List.length model.forest)

        ( index, seed_ ) =
            Random.step gen model.seed
    in
        { model
            | seed = seed_
            , forest =
                model.forest
                    |> updateElement index (PTree.kick (V3.vec3 1 -1 0))
        }


updateElement : Int -> (a -> a) -> List a -> List a
updateElement index f list =
    let
        apply i x =
            if i == index then
                f x
            else
                x
    in
        List.indexedMap apply list
