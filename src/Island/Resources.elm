module Island.Resources exposing (..)

{-|
TODO:
simplify Vertex.neighbors to List Int?
python to convert data to list of {p,r,q} dict of vertex records
JSON decoder

stress test mesh size

python3 module for fast conversion

learn about bezier approach
  - how many triangles need to be constructed in realtime?
  - big hassle, needs another collision algorithm

-}

import Math.Vector3 as V3 exposing (vec3, Vec3)
import Math.Vector4 as V4 exposing (vec4, Vec4)
import Vector as V exposing (Vector)
import Island.Types exposing (..)
import Json.Decode as JD exposing (decodeString, list, at, dict, float, int)
import Json.Decode.Pipeline exposing (required, decode)
import Utilities exposing (combine)


decodeMesh : String -> Maybe Mesh
decodeMesh string =
    let
        vector =
            decode vec3
                |> required "x" float
                |> required "y" float
                |> required "z" float

        vertex =
            decode Vertex
                |> required "i" int
                |> required "p" vector
                |> required "n" vector
                |> required "ns" (list (JD.map (\x -> ( x, x, x )) int))

        result =
            decodeString (list (list vertex)) string

        convert t =
            case t of
                p :: q :: r :: _ ->
                    Just ( p, q, r )

                _ ->
                    Nothing
    in
        case result of
            Ok x ->
                let
                    z =
                        Debug.log "success of length" (List.length x)

                    finalResult =
                        x |> List.map convert |> combine
                in
                    finalResult

            Err x ->
                let
                    z =
                        Debug.log "err" x
                in
                    Nothing


initScene : List Object
initScene =
    [ { effects = []
      , velocity = Nothing
      , drawable = Just Clipmap
      , scale = Vector 50.0 50.0 2.0
      , frame =
            { position = Vector -25 -25 1.0
            , orientation =
                { scalar = 1.0
                , vector = Vector 0.0 0.0 0.0
                }
            }
      , material = ClipmapTexture HeightTexMap
      }
    , { effects = []
      , velocity = Nothing
      , drawable = Just TreeLowPoly
      , scale = Vector 0.7584560513496399 0.7584560513496399 0.7584560513496399
      , frame =
            { position = Vector -2.8599436283111572 3.414069652557373 2.2742698192596436
            , orientation =
                { scalar = 1.0
                , vector = Vector 0.0 0.0 0.0
                }
            }
      , material = Color (vec4 1 0.2 0.5 1)
      }
    , { effects = []
      , velocity = Nothing
      , drawable = Just TreeLowPoly
      , scale = Vector 0.384454607963562 0.384454607963562 0.384454607963562
      , frame =
            { position = Vector -2.299999952316284 -4.516265869140625 1.3111214637756348
            , orientation =
                { scalar = 0.9659258127212524
                , vector = Vector 0.0 0.0 0.2588191032409668
                }
            }
      , material = Color (vec4 1 0.2 0.5 1)
      }
    , { effects = []
      , velocity = Nothing
      , drawable = Just TreeLowPoly
      , scale = Vector 0.384454607963562 0.384454607963562 0.384454607963562
      , frame =
            { position = Vector -2.2492892742156982 -4.516265869140625 1.3111214637756348
            , orientation =
                { scalar = 1.0
                , vector = Vector 0.0 0.0 0.0
                }
            }
      , material = Color (vec4 1 0.2 0.5 1)
      }
    , { effects = []
      , velocity = Nothing
      , drawable = Just TreeLowPoly
      , scale = Vector 0.384454607963562 0.384454607963562 0.384454607963562
      , frame =
            { position = Vector -5.006307601928711 -2.578901767730713 1.1591014862060547
            , orientation =
                { scalar = 1.0
                , vector = Vector 0.0 0.0 0.0
                }
            }
      , material = Color (vec4 1 0.2 0.5 1)
      }
    , { effects = []
      , velocity = Nothing
      , drawable = Just TreeLowPoly
      , scale = Vector 0.384454607963562 0.384454607963562 0.384454607963562
      , frame =
            { position = Vector -0.6844954490661621 -5.447690963745117 0.9557718634605408
            , orientation =
                { scalar = 1.0
                , vector = Vector 0.0 0.0 0.0
                }
            }
      , material = Color (vec4 1 0.2 0.5 1)
      }
    , { effects = []
      , velocity = Nothing
      , drawable = Just TreeLowPoly
      , scale = Vector 0.384454607963562 0.384454607963562 0.384454607963562
      , frame =
            { position = Vector -2.100261688232422 -0.9768505096435547 0.9076738953590393
            , orientation =
                { scalar = 1.0
                , vector = Vector 0.0 0.0 0.0
                }
            }
      , material = Color (vec4 1 0.2 0.5 1)
      }
    , { effects = []
      , velocity = Nothing
      , drawable = Just TreeLowPoly
      , scale = Vector 0.384454607963562 0.384454607963562 0.384454607963562
      , frame =
            { position = Vector 2.0453500747680664 4.359618186950684 1.3316922187805176
            , orientation =
                { scalar = 1.0
                , vector = Vector 0.0 0.0 0.0
                }
            }
      , material = Color (vec4 1 0.2 0.5 1)
      }
    , { effects = []
      , velocity = Nothing
      , drawable = Just TreeLowPoly
      , scale = Vector 0.384454607963562 0.384454607963562 0.384454607963562
      , frame =
            { position = Vector 3.5791378021240234 3.0362014770507813 0.9046616554260254
            , orientation =
                { scalar = 1.0
                , vector = Vector 0.0 0.0 0.0
                }
            }
      , material = Color (vec4 1 0.2 0.5 1)
      }
    , { effects = []
      , velocity = Nothing
      , drawable = Just TreeLowPoly
      , scale = Vector 0.384454607963562 0.384454607963562 0.384454607963562
      , frame =
            { position = Vector 3.445768356323242 -0.9741153717041016 -0.009870648384094238
            , orientation =
                { scalar = 1.0
                , vector = Vector 0.0 0.0 0.0
                }
            }
      , material = Color (vec4 1 0.2 0.5 1)
      }
    , { effects = []
      , velocity = Nothing
      , drawable = Just TreeLowPoly
      , scale = Vector 0.384454607963562 0.384454607963562 0.384454607963562
      , frame =
            { position = Vector 4.419713973999023 -2.3926167488098145 0.019851207733154297
            , orientation =
                { scalar = 1.0
                , vector = Vector 0.0 0.0 0.0
                }
            }
      , material = Color (vec4 1 0.2 0.5 1)
      }
    ]


initIsland : Mesh
initIsland =
    Maybe.withDefault [] <|
        decodeMesh <|
            """[]"""
