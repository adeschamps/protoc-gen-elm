port module Main exposing (main)

import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E
import Compile
import Google.Protobuf.Compiler.Plugin as Plugin
import ProtoBuf.Decode as PD
import ProtoBuf.Encode as PE


port stdout : List Int -> Cmd msg


port stderr : String -> Cmd msg


port inputs : (List Int -> msg) -> Sub msg


type alias Model =
    {}


type Msg
    = Input (Maybe Plugin.CodeGeneratorRequest)


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : {} -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


update msg model =
    case msg of
        Input (Just request) ->
            let
                response =
                    Compile.compile request

                responseToBytes bytes =
                    D.decode (D.loop ( Bytes.width bytes, [] ) listStep) bytes |> Maybe.map List.reverse

                listStep ( n, xs ) =
                    if n <= 0 then
                        D.succeed (D.Done xs)

                    else
                        D.map (\x -> D.Loop ( n - 1, x :: xs )) D.unsignedInt8
            in
            ( model
            , Cmd.batch
                [ stderr ("received request: " ++ Debug.toString request)
                , stderr ("generated response: " ++ Debug.toString response)
                , response
                    |> Plugin.encodeCodeGeneratorResponse
                    |> PE.encode
                    |> responseToBytes
                    |> Maybe.map stdout
                    |> Maybe.withDefault (stderr "failed to encode response")
                ]
            )

        Input Nothing ->
            ( model, stderr <| "failed to parse request" )


subscriptions : Model -> Sub Msg
subscriptions _ =
    inputs
        ((List.map E.unsignedInt8 >> E.sequence)
            >> E.encode
            >> PD.decode Plugin.codeGeneratorRequestDecoder
            >> Input
        )
