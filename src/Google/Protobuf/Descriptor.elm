module Google.Protobuf.Descriptor exposing (DescriptorProto, FieldDescriptorProto, FileDescriptorProto, FileDescriptorSet, fileDescriptorProtoDecoder, fileDescriptorProtoDefault, fileDescriptorSetDecoder, fileDescriptorSetDefault)

import Google.Protobuf.Descriptor.DescriptorProto as DescriptorProto
import Google.Protobuf.Descriptor.FieldDescriptorProto as FieldDescriptorProto
import Google.Protobuf.Descriptor.SourceCodeInfo as SourceCodeInfo
import ProtoBuf.Decode as DP



-- TYPES


type alias FileDescriptorSet =
    { file : List FileDescriptorProto
    }


type alias FileDescriptorProto =
    { name : String
    , package : String
    , dependency : List String
    , publicDependency : List Int
    , weakDependency : List Int
    , messageType : List DescriptorProto

    -- , enumType : List EnumDescriptorProto
    -- , service : List ServiceDescriptorProto
    -- , extension : List FieldDescriptorProto
    , sourceCodeInfo : SourceCodeInfo
    }


type alias DescriptorProto =
    { name : String
    , field : List FieldDescriptorProto
    , extension : List FieldDescriptorProto

    -- , nestedType : List DescriptorProto
    , reservedRange : List DescriptorProto.ReservedRange
    }


type alias FieldDescriptorProto =
    { name : String
    , number : Int
    , label : Maybe FieldDescriptorProto.Label
    , type_ : Maybe FieldDescriptorProto.Type
    , typeName : String
    , jsonString : String
    }


type alias SourceCodeInfo =
    { location : List SourceCodeInfo.Location
    }



-- DEFAULTS


fileDescriptorSetDefault : FileDescriptorSet
fileDescriptorSetDefault =
    { file = []
    }


fileDescriptorProtoDefault : FileDescriptorProto
fileDescriptorProtoDefault =
    { name = ""
    , package = ""
    , dependency = []
    , publicDependency = []
    , weakDependency = []
    , messageType = []

    -- , enumType : []
    -- , service : []
    -- , extension : []
    , sourceCodeInfo = sourceCodeInfoDefault
    }


descriptorProtoDefault : DescriptorProto
descriptorProtoDefault =
    { name = ""
    , field = []
    , extension = []

    -- , nestedType = []
    , reservedRange = []
    }


fieldDescriptorProtoDefault : FieldDescriptorProto
fieldDescriptorProtoDefault =
    { name = ""
    , number = 0
    , label = FieldDescriptorProto.labelDefault
    , type_ = FieldDescriptorProto.typeDefault
    , typeName = ""
    , jsonString = ""
    }


sourceCodeInfoDefault : SourceCodeInfo
sourceCodeInfoDefault =
    { location = []
    }



-- DECODERS


fileDescriptorSetDecoder : DP.Decoder FileDescriptorSet
fileDescriptorSetDecoder =
    DP.message fileDescriptorSetDefault
        [ DP.repeated 1 fileDescriptorProtoDecoder .file (\v m -> { m | file = v })
        ]


fileDescriptorProtoDecoder : DP.Decoder FileDescriptorProto
fileDescriptorProtoDecoder =
    DP.message fileDescriptorProtoDefault
        [ DP.optional 1 DP.string (\v m -> { m | name = v })
        , DP.optional 2 DP.string (\v m -> { m | package = v })
        , DP.repeated 3 DP.string .dependency (\v m -> { m | dependency = v })
        , DP.repeated 10 DP.int32 .publicDependency (\v m -> { m | publicDependency = v })
        , DP.repeated 11 DP.int32 .weakDependency (\v m -> { m | weakDependency = v })
        , DP.repeated 4 descriptorProtoDecoder .messageType (\v m -> { m | messageType = v })
        , DP.optional 9 sourceCodeInfoDecoder (\v m -> { m | sourceCodeInfo = v })
        ]


descriptorProtoDecoder : DP.Decoder DescriptorProto
descriptorProtoDecoder =
    DP.message descriptorProtoDefault
        [ DP.optional 1 DP.string (\v m -> { m | name = v })
        , DP.repeated 2 fieldDescriptorProtoDecoder .field (\v m -> { m | field = v })

        -- , DP.repeated 3 descriptorProtoDecoder .nestedType (\v m -> { m | nestedType = v })
        , DP.repeated 9 DescriptorProto.reservedRangeDecoder .reservedRange (\v m -> { m | reservedRange = v })
        ]


fieldDescriptorProtoDecoder : DP.Decoder FieldDescriptorProto
fieldDescriptorProtoDecoder =
    DP.message fieldDescriptorProtoDefault
        [ DP.optional 1 DP.string (\v m -> { m | name = v })
        , DP.optional 3 DP.int32 (\v m -> { m | number = v })
        , DP.optional 4 FieldDescriptorProto.labelDecoder (\v m -> { m | label = v })
        , DP.optional 5 FieldDescriptorProto.typeDecoder (\v m -> { m | type_ = v })
        , DP.optional 6 DP.string (\v m -> { m | typeName = v })
        , DP.optional 10 DP.string (\v m -> { m | jsonString = v })
        ]


sourceCodeInfoDecoder : DP.Decoder SourceCodeInfo
sourceCodeInfoDecoder =
    DP.message sourceCodeInfoDefault
        [ DP.repeated 1 SourceCodeInfo.locationDecoder .location (\v m -> { m | location = v })
        ]
