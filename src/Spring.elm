module Spring
    exposing
        ( Spring
        , create
        , createAt
        , current
        , animationHasEnded
        , map
        , map2
        , andMap
        , connect
        , setDestination
        , connectMany
        , animate
        , epsilon
        )

{-| Module for spring-based animations in Elm.

# Create Springs
@docs Spring, create, createAt

# Query and Modify Springs
@docs current, setDestination, animationHasEnded, animationsHaveEnded

# Animate Springs
@docs animate, animateNested

# Connect Springs
@docs connect, connectMany

# All the maps!
@docs map, map2, map3, map4, map5, andMap
-}

import Time exposing (Time)


{-| Main Spring Type.
A spring's behavior is defined by its stiffness and damping parameters.
-}
type alias Spring a =
    { stiffness : Float
    , damping : Float
    , position : a
    , velocity : a
    , destination : a
    }


{-| Create a Spring Float given values for stiffness and damping.
The Spring Float is the basic type of Spring and is the only one that
can be animated with the `animate` functon. You can use this as a building
block to create more complex Springs using functions like `map` or `andMap`.
-}
create : Float -> Float -> Spring Float
create =
    createAt 0


{-| Create a spring with a given position.
-}
createAt : Float -> Float -> Float -> Spring Float
createAt position stiffness damping =
    { stiffness = stiffness
    , damping = damping
    , position = position
    , velocity = 0
    , destination = position
    }


{-| Get the current value of the spring.
-}
current : Spring a -> a
current { position } =
    position


{-| Detect if a spring animation has ended.
A spring animation is considered to have ended if the spring
has reached its destination and has 0 velocity.
-}
animationHasEnded : Spring Float -> Bool
animationHasEnded spring =
    spring.position == spring.destination && spring.velocity == 0


{-| Map a function onto a spring.
-}
map : (a -> b) -> Spring a -> Spring b
map f spring =
    { spring
        | position = f spring.position
        , velocity = f spring.velocity
        , destination = f spring.destination
    }


{-| -}
map2 : (a -> b -> c) -> Spring a -> Spring b -> Spring c
map2 f springA springB =
    { stiffness = (springA.stiffness + springB.stiffness) / 2
    , damping = (springA.damping + springB.damping) / 2
    , position = f springA.position springB.position
    , velocity = f springA.velocity springB.velocity
    , destination = f springA.destination springB.destination
    }


{-| Chain mapping operations together
-}
andMap : Spring (a -> b) -> Spring a -> Spring b
andMap =
    map2 (<|)


{-| Connect two springs together. This function will modify the second spring
to have the first spring's position as its destination.
-}
connect : Spring a -> Spring a -> Spring a
connect spring1 spring2 =
    { spring2 | destination = spring1.position }


{-| Set the destination of a spring.
-}
setDestination : a -> Spring a -> Spring a
setDestination destination spring =
    { spring | destination = destination }


{-| Connect multiple strings end to end. The first spring's destination will be
set to the provided destination. The second spring's destination will be set to
the first spring's position. The third spring's destination will be set to
the second spring's position. And so on...
-}
connectMany : a -> List (Spring a) -> List (Spring a)
connectMany destination list =
    case list of
        [] ->
            []

        s :: ss ->
            setDestination destination s :: connectMany s.position ss


epsilon : Float
epsilon =
    0.0001


{-| Animate a spring given a framerate.

    animate framerate spring
-}
animate : Time -> Spring Float -> Spring Float
animate fpms spring =
    let
        frameRate =
            fpms / 1000

        fspring =
            -spring.stiffness * (spring.position - spring.destination)

        fdamper =
            -spring.damping * spring.velocity

        a =
            fspring + fdamper

        newV =
            spring.velocity + a * frameRate

        newX =
            spring.position + newV * frameRate
    in
        if abs (newV - spring.velocity) < epsilon && abs (newX - spring.position) < epsilon then
            { spring
                | position = spring.destination
                , velocity = 0
            }
        else
            { spring
                | position = newX
                , velocity = newV
            }
