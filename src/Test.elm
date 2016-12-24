-- Base.elm


module Base exposing (..)

import Html
import List exposing (..)
import String
import Lazy exposing (Lazy)


type Base
    = Base Object


type Effect
    = DrinkInk
    | LerpFood


type alias Object =
    { toGeneric : Generic {}
    , effects : Lazy Base
    , string :
        String
    , addEffect : Effect -> Base
    }


type alias Generic a =
    { a | name : String }


toGeneric : Generic a -> Generic {}
toGeneric data =
    { name = data.name }



-- Foo.elm


unwrapBase : Base -> Object
unwrapBase (Base x) =
    x


foldlf : a -> List (a -> a) -> a
foldlf acc fs =
    List.foldl (\f acc -> f acc) acc fs


foo : Generic { a | food : Int } -> List (Generic { a | food : Int } -> Generic { a | food : Int }) -> Base
foo data effects =
    let
        effects2 =
            Lazy.lazy (\_ -> foo (foldlf data effects) [])

        stringForm =
            [ "name:" ++ .name (toGeneric data)
            , "food:" ++ toString data.food
            ]
                |> String.join ", "

        addEffect : Effect -> Base
        addEffect effect =
            case effect of
                LerpFood ->
                    foo data (effects ++ [ foodLerp2 ])

                _ ->
                    foo data effects
    in
        Base { toGeneric = toGeneric data, effects = effects2, string = stringForm, addEffect = addEffect }



-- bar : { a | name : String, drink : Int } -> Base


bar : Generic { a | drink : Int } -> List (Generic { a | drink : Int } -> Generic { a | drink : Int }) -> Base
bar data effects =
    let
        effects2 =
            Lazy.lazy (\_ -> bar (foldlf data effects) [])

        stringForm =
            [ "name:" ++ .name (toGeneric data)
            , "drink:" ++ toString data.drink
            ]
                |> String.join ", "

        addEffect effect =
            case effect of
                _ ->
                    bar data effects
    in
        Base { toGeneric = toGeneric data, effects = effects2, string = stringForm, addEffect = addEffect }


foobar : Generic { a | drink : Int, food : Int } -> List (Generic { a | drink : Int, food : Int } -> Generic { a | drink : Int, food : Int }) -> Base
foobar data effects =
    let
        effects2 =
            Lazy.lazy (\_ -> foobar (foldlf data effects) [])

        stringForm =
            [ "name:" ++ .name (toGeneric data)
            , "drink:" ++ toString data.drink
            , "food:" ++ toString data.food
            ]
                |> String.join ", "

        addEffect effect =
            case effect of
                LerpFood ->
                    foobar data (effects ++ [ foodLerp2 ])

                _ ->
                    foobar data effects
    in
        Base { toGeneric = toGeneric data, effects = effects2, string = stringForm, addEffect = addEffect }



-- can we add an effect to data that supports it??
-- compose : (data -> Base) -> (data -> Base) -> (data -> Base)
-- composeEffects data =
--     List.foldl (\f acc -> f acc) data data.effects


inkDrink2 : { a | drink : Int } -> { a | drink : Int }
inkDrink2 data =
    { data | drink = data.drink + 1 }


foodLerp2 : { a | food : Int } -> { a | food : Int }
foodLerp2 data =
    { data | food = data.food + 1 }


{-| Can prepend parameters to generate different (static) effects. To get
local memory that is not part of the model/object, could
a) return new effect, i.e., (Base, Maybe Effect)
b) when apply effects, remove all effects and add any new ones.
-}
inkDrink : { a | name : String, drink : Int } -> Base
inkDrink data =
    { data | drink = data.drink + 1 } |> (flip bar) []


foodLerp : { a | name : String, food : Int } -> Base
foodLerp data =
    { data | food = data.food + 1 } |> (flip foo) []



-- big time update rule
-- bigTimeUpdateRule objects model =
--   let
--     warmUp effects = effects |> List.map
-- want to store Effects and apply them
-- apply : (Base -> a) -> Base -> a


apply : (Object -> a) -> Base -> a
apply f (Base x) =
    f x


applyEffects : Base -> Object
applyEffects (Base x) =
    x.effects
        |> Lazy.force
        |> (\(Base z) -> z)


main : Html.Html msg
main =
    let
        -- Fill a list with "derived instances".
        l : List Base
        l =
            [ foo { name = "foo", food = 0 } [ foodLerp2, foodLerp2 ]
            , bar { name = "bar", drink = 0 } [ inkDrink2, inkDrink2 ]
            , foobar { name = "foobar", food = 0, drink = 0 } [ inkDrink2 ]
            ]

        l_ =
            l |> List.map addLerp |> List.map applyEffects

        addLerp : Base -> Base
        addLerp (Base base) =
            if (base.toGeneric.name == "foobar") then
                base.addEffect LerpFood
            else
                Base base
    in
        -- Show result.
        l_ |> List.map .string |> String.join " - " |> Html.text
