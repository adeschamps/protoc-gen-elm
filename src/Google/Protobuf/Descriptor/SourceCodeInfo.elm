module Google.Protobuf.Descriptor.SourceCodeInfo exposing (Location, locationDecoder, locationDefault)

import ProtoBuf.Decode as DP


type alias Location =
    { path : List Int
    , span : List Int
    , leadingComments : String
    , trailingComments : String
    , leadingDetachedComments : List String
    }


locationDefault : Location
locationDefault =
    { path = []
    , span = []
    , leadingComments = ""
    , trailingComments = ""
    , leadingDetachedComments = []
    }


locationDecoder : DP.Decoder Location
locationDecoder =
    DP.message locationDefault
        [ DP.repeated 1 DP.int32 .path (\v m -> { m | path = v })
        , DP.repeated 2 DP.int32 .span (\v m -> { m | span = v })
        , DP.optional 3 DP.string (\v m -> { m | leadingComments = v })
        , DP.optional 4 DP.string (\v m -> { m | trailingComments = v })
        , DP.repeated 5 DP.string .leadingDetachedComments (\v m -> { m | leadingDetachedComments = v })
        ]
