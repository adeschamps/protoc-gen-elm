module Google.Protobuf.Compiler.Plugin exposing (CodeGeneratorRequest, CodeGeneratorResponse, codeGeneratorRequestDecoder, codeGeneratorResponseDefault, encodeCodeGeneratorResponse)

import Bytes.Decode as D exposing (Decoder)
import Google.Protobuf.Compiler.Plugin.CodeGeneratorResponse as CodeGeneratorResponse
import Google.Protobuf.Descriptor exposing (..)
import ProtoBuf.Decode as PD
import ProtoBuf.Encode as PE



-- TYPES


type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    , suffix : String
    }


type alias CodeGeneratorRequest =
    { fileToGenerate : List String
    , parameter : String
    , protoFile : List FileDescriptorProto
    , compilerVersion : Version
    }


type alias CodeGeneratorResponse =
    { error : String
    , file : List CodeGeneratorResponse.File
    }



-- DEFAULTS


versionDefault : Version
versionDefault =
    { major = 0
    , minor = 0
    , patch = 0
    , suffix = ""
    }


codeGeneratorRequestDefault : CodeGeneratorRequest
codeGeneratorRequestDefault =
    { fileToGenerate = []
    , parameter = ""
    , protoFile = []
    , compilerVersion = versionDefault
    }


codeGeneratorResponseDefault : CodeGeneratorResponse
codeGeneratorResponseDefault =
    { error = ""
    , file = []
    }



-- DECODERS


versionDecoder : PD.Decoder Version
versionDecoder =
    PD.message versionDefault
        [ PD.optional 1 PD.int32 (\v m -> { m | major = v })
        , PD.optional 2 PD.int32 (\v m -> { m | minor = v })
        , PD.optional 3 PD.int32 (\v m -> { m | patch = v })
        , PD.optional 4 PD.string (\v m -> { m | suffix = v })
        ]


codeGeneratorRequestDecoder : PD.Decoder CodeGeneratorRequest
codeGeneratorRequestDecoder =
    PD.message codeGeneratorRequestDefault
        [ PD.repeated 1 PD.string .fileToGenerate (\v m -> { m | fileToGenerate = v })
        , PD.optional 2 PD.string (\v m -> { m | parameter = v })
        , PD.repeated 15 fileDescriptorProtoDecoder .protoFile (\v m -> { m | protoFile = v })
        , PD.optional 3 versionDecoder (\v m -> { m | compilerVersion = v })
        ]



-- ENCODERS


encodeCodeGeneratorResponse : CodeGeneratorResponse -> PE.Encoder
encodeCodeGeneratorResponse msg =
    PE.message
        [ ( 1, PE.string msg.error )
        , ( 15, PE.list CodeGeneratorResponse.encodeFile msg.file )
        ]
