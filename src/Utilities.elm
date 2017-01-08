module Utilities exposing (..)

{-| Useful functions with no project dependencies.
-}


{-| -}
map3T : (a -> b) -> ( a, a, a ) -> ( b, b, b )
map3T f ( a, b, c ) =
    ( f a, f b, f c )


map3L : (a -> b) -> ( a, a, a ) -> List b
map3L f ( a, b, c ) =
    [ f a, f b, f c ]


map3R : (a -> a -> a -> b) -> ( a, a, a ) -> ( b, b, b )
map3R f ( p, q, r ) =
    ( f p q r, f q r p, f r p q )


isJust : Maybe a -> Bool
isJust a =
    case a of
        Just _ ->
            True

        Nothing ->
            False


{-| Zero-indexed.
-}
allBut : Int -> List a -> List a
allBut n xs =
    (++) (List.take n xs) (List.drop (n + 1) xs)


appendMaybe : Maybe a -> List a -> List a
appendMaybe maybeX xs =
    case maybeX of
        Just x ->
            xs ++ [ x ]

        Nothing ->
            xs


type alias M2 =
    ( Float, Float, Float, Float )


invert2x2 : M2 -> Maybe M2
invert2x2 m =
    let
        ( a, b, c, d ) =
            m

        det =
            a * d - b * c
    in
        if det < 1.0e-3 then
            Nothing
        else
            Just ( d / det, -b / det, -c / det, a / det )
