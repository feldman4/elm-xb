module Base exposing (..)

import Html
import Html.Events
import List exposing (..)
import String
import Lazy exposing (Lazy)


{-| Adaptation of https://github.com/Dobiasd/articles/blob/master/from_oop_to_fp_-_inheritance_and_the_expression_problem.md

The main benefit is defining update/display functions in just one place (class
constructor). The downside is that you can't directly pattern match on created
objects and can't read/write the internal data except via the interface. You can
pattern match on the `.class` attribute but you have to trust it's accurate and
you don't simplify the resulting object type. You could get the constructor from
a concrete class this way, but what for?

Introducing an Effect union type lets us represent a chain of functions that
update objects as a list of Effects. Any Effect can go on any class. Effects can
be viewed and replaced using `.toGeneric.effects` and `.replaceEffects.` They
can be applied using `Lazy.force .doEffects`.

Effects represent one step in an update rule. An alternative would be to have
global update rules applied to every object (e.g., `object |> physics dt`), with
a special flag to indicate if an object should be bypassed, like `object |>
applyIfActive (physics dt)`. We have a choice to retain effects after applying
them or not. `classEffects : Effect -> Maybe (Class -> Class)` or 'classEffects
: Effect -> Maybe ((Class->Class), Maybe Effect)'. Persistent effects can retain
memory via parameters, e.g., `type Effect = GradualShrinkBy Easing Factor | ...
`. Effects requiring the model could be described by

```
type Effect = ModelEffect ModelEffect | SimpleEffect SimpleEffect
type ModelEffect = Physics Model | ` and
effects can be evaluated by `doEffectsWithModel : Lazy (Model -> Base) ``

matrix of decisions: EFFECTS x CLASSES
would be nice to only allow effects that a class supports
naive: new effect type for every class
better?: shared type variable between class and effect
better?: `type Object = {effects = List (Object -> Object)}`

Afterwards you can use
.replaceEffects since objects all look the same from outside. Effects are
visible in debugger under .toGeneric and can be copied to another object. There
is no guarantee they pattern match and do anything. It might be better to have
safe copying that is type aware (but you would need more increasingly large copy
functions as the number of classes increases).

Compare to regular pattern matching.

1. Scales poorly: need to re-declare effect for all classes using it. Use common
 function to implement effect. Can filter objects by single type and get refined
 type signature ().
2. Allows easy inspection of effects on each object over time. Single source of
truth -- effects act by mapping apply or applyWithModel over effects for each
object.
3. Easy to remove effects.
-}
type Base
    = Base Object


type Effect
    = DrinkInk
    | LerpFood
    | UpdateFood Int
    | FoodEffect


type ObjectClass
    = Foo (Food Generic -> Object)
    | Bar
    | Foobar (Dinner Generic -> Object)
    | GenericClass (Generic -> Object)


type alias Object =
    { toGeneric : Generic
    , class : ObjectClass
    , string : String
    , doEffects : DoEffects
    , replaceEffects :
        ReplaceEffects
        -- , prependEffect : Effect -> Base
        -- , clearEffects : Base
        -- , viewAppendedEffects : List (Effect -> Base)
    }


type DoEffects
    = DoEffects (Lazy Object)


type ReplaceEffects
    = ReplaceEffects (List Effect -> Object)


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


makeReplaceEffects : (HasGeneric a -> Object) -> HasGeneric a -> List Effect -> Object
makeReplaceEffects foo data effects =
    foo { data | effects = effects }


generic : Generic -> Object
generic data =
    let
        stringForm =
            "name:" ++ .name (toGeneric data)

        genericEffects effect =
            case effect of
                _ ->
                    Nothing

        trueEffects =
            data.effects |> List.filterMap genericEffects

        lazyEffects =
            Lazy.lazy (\_ -> generic (foldlf data trueEffects))
    in
        { toGeneric = toGeneric data
        , doEffects = DoEffects lazyEffects
        , replaceEffects = ReplaceEffects (makeReplaceEffects generic data)
        , string = stringForm
        , class = GenericClass generic
        }


foo : Food Generic -> Object
foo data =
    let
        trueEffects =
            data.effects
                |> List.filterMap fooEffects

        lazyEffects =
            Lazy.lazy (\_ -> foo (foldlf data trueEffects))
    in
        { toGeneric = toGeneric data
        , doEffects = DoEffects lazyEffects
        , replaceEffects = ReplaceEffects (makeReplaceEffects foo data)
        , string = foodString data
        , class = Foo foo
        }


bar : HasGeneric (Drink a) -> Object
bar data =
    let
        stringForm =
            [ "name:" ++ .name (toGeneric data)
            , "drink:" ++ toString data.drink
            ]
                |> String.join ", "

        barEffects effect =
            case effect of
                DrinkInk ->
                    Just inkDrink2

                _ ->
                    Nothing

        trueEffects =
            data.effects
                |> List.filterMap barEffects

        lazyEffects =
            Lazy.lazy (\_ -> bar (foldlf data trueEffects))
    in
        { toGeneric = toGeneric data
        , doEffects = DoEffects lazyEffects
        , replaceEffects = ReplaceEffects (makeReplaceEffects bar data)
        , string = stringForm
        , class = Bar
        }


foobar : Dinner Generic -> Object
foobar data =
    let
        stringForm =
            [ "name:" ++ .name (toGeneric data)
            , "drink:" ++ toString data.drink
            , "food:" ++ toString data.food
            ]
                |> String.join ", "

        foobarEffects effect =
            case effect of
                LerpFood ->
                    Just foodLerp2

                DrinkInk ->
                    Just inkDrink2

                _ ->
                    Nothing

        trueEffects =
            data.effects
                |> List.filterMap foobarEffects

        lazyEffects =
            Lazy.lazy (\_ -> foobar (foldlf data trueEffects))
    in
        { toGeneric = toGeneric data
        , doEffects = DoEffects lazyEffects
        , string = stringForm
        , replaceEffects = ReplaceEffects (makeReplaceEffects foobar data)
        , class = Foobar foobar
        }



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
apply : (Object -> a) -> Base -> a
apply f (Base x) =
    f x


applyEffects : Object -> Object
applyEffects { doEffects } =
    let
        (DoEffects x) =
            doEffects
    in
        Lazy.force x


{-| Use like:
Base |> appendEffect (UpdateFood 100)

Wouldn't it be nice to write:
 {(Base) | food = 100}

You can do UpdateFood to something that doesn't have .food and it just
does nothing. Should generate a type error instead. Could tie the type of Base
to allowed effects, but then can't use one Effect on multiple types.
-}
replaceEffects : Object -> List Effect -> Object
replaceEffects { replaceEffects } =
    let
        (ReplaceEffects x) =
            replaceEffects
    in
        x


appendEffect : Effect -> Object -> Object
appendEffect effect object =
    let
        effects =
            object.toGeneric.effects ++ [ effect ]
    in
        replaceEffects object effects


main : Program Never (List Object) Msg
main =
    Html.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


type Msg
    = NoOp


update : Msg -> List Object -> ( List Object, Cmd Msg )
update msg model =
    (model |> List.map addLerp) ! []


addLerp : Object -> Object
addLerp object =
    if (object.toGeneric.name == "foobar") then
        appendEffect LerpFood object
    else
        object


init : List Object
init =
    let
        preFoo =
            foo { name = "foo", food = 0, effects = [] }
                |> appendEffect (UpdateFood 50)

        -- Fill a list with "derived instances".
        l : List Object
        l =
            [ preFoo
            , bar { name = "bar", drink = 0, randomField = "yes", effects = [ DrinkInk, DrinkInk ] }
            , foobar { name = "foobar", food = 0, drink = 0, effects = [ DrinkInk, LerpFood ] }
            ]
    in
        l |> List.map addLerp


view : List Object -> Html.Html Msg
view l_ =
    let
        -- Show result.
        text =
            l_
                |> List.map applyEffects
                |> List.map .string
                |> String.join " - "
    in
        Html.div [ Html.Events.onClick NoOp ] [ Html.text text ]
