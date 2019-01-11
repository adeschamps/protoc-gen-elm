module Google.Protobuf.Descriptor.DescriptorProto exposing (ReservedRange, reservedRangeDecoder, reservedRangeDefault)

import ProtoBuf.Decode as DP


type alias ReservedRange =
    { start : Int
    , end : Int
    }


reservedRangeDefault : ReservedRange
reservedRangeDefault =
    { start = 0
    , end = 0
    }


reservedRangeDecoder : DP.Decoder ReservedRange
reservedRangeDecoder =
    DP.message reservedRangeDefault
        [ DP.optional 1 DP.int32 (\v m -> { m | start = v })
        , DP.optional 2 DP.int32 (\v m -> { m | end = v })
        ]
