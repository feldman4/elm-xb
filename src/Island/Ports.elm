port module Island.Ports exposing (..)

{-| (0,1) coordinates in texture, request name
-}


port requestOceanPort : ( ( Float, Float ), String ) -> Cmd msg


port waterIndicator : (( String, ( Float, Float, Float, Float ) ) -> msg) -> Sub msg
