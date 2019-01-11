module Google.Protobuf.Compiler.Plugin.CodeGeneratorResponse exposing (File, encodeFile, fileDefault)

import ProtoBuf.Encode as PE


type alias File =
    { name : String
    , insertionPoint : String
    , content : String
    }


fileDefault : File
fileDefault =
    { name = ""
    , insertionPoint = ""
    , content = ""
    }


encodeFile : File -> PE.Encoder
encodeFile msg =
    PE.message
        [ ( 1, PE.string msg.name )
        , ( 2, PE.string msg.insertionPoint )
        , ( 15, PE.string msg.content )
        ]
