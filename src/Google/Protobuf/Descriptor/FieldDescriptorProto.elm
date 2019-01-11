module Google.Protobuf.Descriptor.FieldDescriptorProto exposing (Label(..), Type(..), labelDecoder, labelDefault, typeDecoder, typeDefault)

import ProtoBuf.Decode as DP


type Type
    = TypeDouble
    | TypeFloat
    | TypeInt64
    | TypeUint64
    | TypeInt32
    | TypeFixed64
    | TypeFixed32
    | TypeBool
    | TypeString
    | TypeGroup
    | TypeMessage
    | TypeBytes
    | TypeUint32
    | TypeEnum
    | TypeSfixed32
    | TypeSfixed64
    | TypeSint32
    | TypeSint64


type Label
    = LabelOptional
    | LabelRequired
    | LabelRepeated


labelDefault : Maybe Label
labelDefault =
    Nothing


typeDefault : Maybe Type
typeDefault =
    Nothing


labelDecoder : DP.Decoder (Maybe Label)
labelDecoder =
    DP.int32
        |> DP.map
            (\value ->
                case value of
                    1 ->
                        Just LabelOptional

                    2 ->
                        Just LabelRequired

                    3 ->
                        Just LabelRepeated

                    _ ->
                        Nothing
            )


typeDecoder : DP.Decoder (Maybe Type)
typeDecoder =
    DP.int32
        |> DP.map
            (\value ->
                case value of
                    1 ->
                        Just TypeDouble

                    2 ->
                        Just TypeFloat

                    3 ->
                        Just TypeInt64

                    4 ->
                        Just TypeUint64

                    5 ->
                        Just TypeInt32

                    6 ->
                        Just TypeFixed64

                    7 ->
                        Just TypeFixed32

                    8 ->
                        Just TypeBool

                    9 ->
                        Just TypeString

                    10 ->
                        Just TypeGroup

                    11 ->
                        Just TypeMessage

                    12 ->
                        Just TypeBytes

                    13 ->
                        Just TypeUint32

                    14 ->
                        Just TypeEnum

                    15 ->
                        Just TypeSfixed32

                    16 ->
                        Just TypeSfixed64

                    17 ->
                        Just TypeSint32

                    18 ->
                        Just TypeSint64

                    _ ->
                        Nothing
            )
