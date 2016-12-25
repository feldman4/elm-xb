module Base exposing (..)

import Html
import Html.Events
import List exposing (..)
import String
import Lazy exposing (Lazy)


type Base
    = Base Object


type Effect
    = DrinkInk
    | LerpFood
    | UpdateFood Int
    | FoodEffect


{-| Don't really have access to effects. Sure would be nice if you did.
Constructor lets you put raw effect functions in. Afterwards you need to use
.appendEffect since objects all look the same from outside. Appended effects
could be viewed and copied, like copying an animation from parent to child.

Compare to regular pattern matching.

1. Scales poorly: need to re-declare effect for all classes using it. Use common
 function to implement effect. Can filter objects by single type and get refined
 type signature ().
2. Allows easy inspection of effects on each object over time. Single source of
truth -- effects act by mapping apply or applyWithModel over effects for each
object.
3. Easy to remove effects.
-}
type alias Object =
    { toGeneric : Generic
    , string : String
    , doEffects : Lazy Base
    , fromEffects : List Effect -> Base
    , appendEffect :
        Effect -> Base
        -- , prependEffect : Effect -> Base
        -- , clearEffects : Base
        -- , viewAppendedEffects : List (Effect -> Base)
    }


type alias Generic =
    { name : String
    , effects : List Effect
    }


{-| Could use this to define an object class, allows any field in data
constructor. Not sure why you would want that.
-}
type alias HasGeneric a =
    { a | name : String, effects : List Effect }


type alias Food a =
    { a | food : Int }


type alias Drink a =
    { a | drink : Int }


type alias Dinner a =
    Food (Drink a)


toGeneric : HasGeneric a -> Generic
toGeneric data =
    { name = data.name, effects = data.effects }


foldlf : a -> List (a -> a) -> a
foldlf acc fs =
    List.foldl (\f acc -> f acc) acc fs


foodString : Food Generic -> String
foodString data =
    [ "name:" ++ .name (toGeneric data)
    , "food:" ++ toString data.food
    ]
        |> String.join ", "


fooEffects : Effect -> Maybe (Food Generic -> Food Generic)
fooEffects effect =
    case effect of
        LerpFood ->
            Just foodLerp2

        UpdateFood val ->
            Just (\x -> { x | food = val })

        _ ->
            Nothing


fromEffects foo data effects =
    foo { data | effects = effects }



-- generic : Generic -> List (Generic -> Generic) -> Base
-- generic data effects =
--     let
--         stringForm =
--             "name:" ++ .name (toGeneric data)
--
--         appendEffect effect =
--             case effect of
--                 _ ->
--                     generic data effects
--     in
--         Base
--             { toGeneric = toGeneric data
--             , doEffects = lazyEffects generic data effects
--             , fromEffects = fromEffects generic data
--             , string = stringForm
--             , appendEffect = appendEffect
--             }


foo : Food Generic -> Base
foo data =
    let
        trueEffects =
            data.effects
                |> List.filterMap fooEffects

        lazyEffects : Lazy Base
        lazyEffects =
            Lazy.lazy (\_ -> foo (foldlf data trueEffects))
    in
        Base
            { toGeneric = toGeneric data
            , doEffects = lazyEffects
            , fromEffects = fromEffects foo data
            , string = foodString data
            , appendEffect = (\e -> fromEffects foo data (data.effects ++ [ e ]))
            }



-- bar : HasGeneric (Drink a) -> List (HasGeneric (Drink a) -> HasGeneric (Drink a)) -> Base
-- bar data effects =
--     let
--         stringForm =
--             [ "name:" ++ .name (toGeneric data)
--             , "drink:" ++ toString data.drink
--             ]
--                 |> String.join ", "
--
--         appendEffect effect =
--             case effect of
--                 _ ->
--                     bar data effects
--     in
--         Base
--             { toGeneric = toGeneric data
--             , lazyEffects = lazyEffects bar data effects
--             , string = stringForm
--             , appendEffect = appendEffect
--             }
--
--
-- foobar : Dinner Generic -> List (Dinner Generic -> Dinner Generic) -> Base
-- foobar data effects =
--     let
--         stringForm =
--             [ "name:" ++ .name (toGeneric data)
--             , "drink:" ++ toString data.drink
--             , "food:" ++ toString data.food
--             ]
--                 |> String.join ", "
--
--         appendEffect effect =
--             case effect of
--                 LerpFood ->
--                     foobar data (effects ++ [ foodLerp2 ])
--
--                 _ ->
--                     foobar data effects
--     in
--         Base
--             { toGeneric = toGeneric data
--             , effects = lazyEffects foobar data effects
--             , string = stringForm
--             , appendEffect = appendEffect
--             }
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



-- inkDrink : { a | name : String, drink : Int } -> Base
-- inkDrink data =
--     { data | drink = data.drink + 1 } |> (flip bar) []
--
--
-- foodLerp : { a | name : String, food : Int } -> Base
-- foodLerp data =
--     { data | food = data.food + 1 } |> (flip foo) []
-- big time update rule
-- bigTimeUpdateRule objects model =
--   let
--     warmUp effects = effects |> List.map
-- want to store Effects and apply them


apply : (Object -> a) -> Base -> a
apply f (Base x) =
    f x


applyEffects : Base -> Base
applyEffects (Base x) =
    Lazy.force x.doEffects


{-| Use like:
Base |> appendEffect (UpdateFood 100)

Wouldn't it be nice to write:
 {(Base) | food = 100}

You can do UpdateFood to something that doesn't have .food and it just
does nothing. Should generate a type error instead. Could tie the type of Base
to allowed effects, but then can't use one Effect on multiple types.
-}
appendEffect : Effect -> Base -> Base
appendEffect effect (Base x) =
    x.appendEffect effect


main : Program Never (List Base) Msg
main =
    Html.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


type Msg
    = NoOp


update : Msg -> List Base -> ( List Base, Cmd Msg )
update msg model =
    (model |> List.map addLerp) ! []


addLerp : Base -> Base
addLerp (Base base) =
    if (base.toGeneric.name == "foobar") then
        base.appendEffect LerpFood
    else
        Base base


init : List Base
init =
    let
        preFoo =
            foo { name = "foo", food = 0, effects = [] }
                |> appendEffect (UpdateFood 50)

        -- Fill a list with "derived instances".
        l : List Base
        l =
            [ preFoo
              -- , bar { name = "bar", drink = 0, randomField = "yes" } [ inkDrink2, inkDrink2 ]
              -- , foobar { name = "foobar", food = 0, drink = 0 } [ inkDrink2 ]
            ]
    in
        l |> List.map addLerp


view : List Base -> Html.Html Msg
view l_ =
    let
        -- Show result.
        text =
            l_
                |> List.map applyEffects
                |> List.map (\(Base x) -> x.string)
                |> String.join " - "
    in
        Html.div [ Html.Events.onClick NoOp ] [ Html.text text ]
