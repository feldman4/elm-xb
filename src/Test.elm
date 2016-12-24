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
    | UpdateFood Int


type alias Object =
    { toGeneric : Generic {}
    , effects : Lazy Base
    , string :
        String
    , addEffect : Effect -> Base
    }


type alias Generic a =
    { a | name : String }


type alias Food a =
    { a | food : Int }


type alias Drink a =
    { a | drink : Int }


type alias Dinner a =
    Food (Drink a)


toGeneric : Generic a -> Generic {}
toGeneric data =
    { name = data.name }


foldlf : a -> List (a -> a) -> a
foldlf acc fs =
    List.foldl (\f acc -> f acc) acc fs


foodString : Generic (Food a) -> String
foodString data =
    [ "name:" ++ .name (toGeneric data)
    , "food:" ++ toString data.food
    ]
        |> String.join ", "


foo : Generic (Food a) -> List (Generic (Food a) -> Generic (Food a)) -> Base
foo data effects =
    let
        lazyEffects =
            Lazy.lazy (\_ -> foo (foldlf data effects) [])
    in
        Base
            { toGeneric = toGeneric data
            , effects = lazyEffects
            , string = foodString data
            , addEffect = foo data << (++) effects << fooEffects
            }


fooEffects : Effect -> List (Food a -> Food a)
fooEffects effect =
    case effect of
        LerpFood ->
            [ foodLerp2 ]

        UpdateFood val ->
            [ (\x -> { x | food = val }) ]

        _ ->
            []



-- copyGeneric f data effects input =
--     let
--         newData =
--             { data | name = input.name }
--     in
--         f newData effects
-- bar : { a | name : String, drink : Int } -> Base


bar : Generic (Drink a) -> List (Generic (Drink a) -> Generic (Drink a)) -> Base
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


foobar : Generic (Dinner a) -> List (Generic (Dinner a) -> Generic (Dinner a)) -> Base
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


inkDrink2 : Drink a -> Drink a
inkDrink2 data =
    { data | drink = data.drink + 1 }


foodLerp2 : Food a -> Food a
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


apply : (Object -> a) -> Base -> a
apply f (Base x) =
    f x


applyEffects : Base -> Object
applyEffects (Base x) =
    x.effects
        |> Lazy.force
        |> (\(Base z) -> z)


{-| Use like:
Base |> addEffect (UpdateFood 100)

Wouldn't it be nice to write:
 {(Base) | food = 100}

You can do UpdateFood to something that doesn't have .food and it just
does nothing. Should generate a type error instead. Could tie the type of Base
to allowed effects, but then can't use one Effect on multiple types.
-}
addEffect : Effect -> Base -> Base
addEffect effect (Base x) =
    x.addEffect effect


main : Html.Html msg
main =
    let
        preFoo =
            foo { name = "foo", food = 0 } [ foodLerp2, foodLerp2 ]
                |> addEffect (UpdateFood 100)

        -- Fill a list with "derived instances".
        l : List Base
        l =
            [ preFoo
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
