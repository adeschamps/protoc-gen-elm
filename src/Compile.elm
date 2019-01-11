module Compile exposing (compile)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Writer
import Google.Protobuf.Compiler.Plugin as Plugin
import Google.Protobuf.Compiler.Plugin.CodeGeneratorResponse as CodeGeneratorResponse
import Google.Protobuf.Descriptor as Descriptor
import Google.Protobuf.Descriptor.FieldDescriptorProto as FieldDescriptorProto
import String.Extra as String


compile : Plugin.CodeGeneratorRequest -> Plugin.CodeGeneratorResponse
compile request =
    { error = ""
    , file = request.protoFile |> List.concatMap compileFile |> List.map writeFile
    }


writeFile : File -> CodeGeneratorResponse.File
writeFile file =
    { name = fileName file
    , insertionPoint = ""
    , content = file |> Elm.Writer.writeFile |> Elm.Writer.write
    }


compileFile : Descriptor.FileDescriptorProto -> List File
compileFile file =
    [ { moduleDefinition =
            Module.NormalModule
                { moduleName = moduleName file |> node
                , exposingList = Exposing.All emptyRange |> node
                }
                |> node
      , imports =
            [ { moduleName = [ "ProtoBuf", "Decode" ] |> node
              , moduleAlias = Just ([ "PD" ] |> node)
              , exposingList = Nothing
              }
            , { moduleName = [ "ProtoBuf", "Encode" ] |> node
              , moduleAlias = Just ([ "PE" ] |> node)
              , exposingList = Nothing
              }
            ]
                |> List.map node
      , declarations =
            file.messageType
                |> List.concatMap compileMessage
                |> List.map node
      , comments = []
      }
    ]


compileMessage : Descriptor.DescriptorProto -> List Declaration
compileMessage message =
    [ Declaration.AliasDeclaration (messageDeclaration message)
    , Declaration.FunctionDeclaration (messageDefault message)
    , Declaration.FunctionDeclaration (messageDecoder message)
    , Declaration.FunctionDeclaration (messageEncoder message)
    ]


messageDeclaration : Descriptor.DescriptorProto -> TypeAlias
messageDeclaration message =
    { documentation = Nothing
    , name = node message.name
    , generics = []
    , typeAnnotation = TypeAnnotation.Record (messageRecordDefinition message) |> node
    }


messageDefault : Descriptor.DescriptorProto -> Function
messageDefault message =
    { documentation = Nothing
    , signature = Just (defaultSignature message |> node)
    , declaration =
        { name = defaultName message |> node
        , arguments = []
        , expression = Expression.RecordExpr (message.field |> List.map (fieldDefaultRecord >> node)) |> node
        }
            |> node
    }


fieldDefaultRecord : Descriptor.FieldDescriptorProto -> Expression.RecordSetter
fieldDefaultRecord field =
    let
        defaultFail =
            Expression.Application ([ Expression.FunctionOrValue [ "Debug" ] "todo", Expression.Literal "no appropriate default value" ] |> List.map node)

        defaultNumber =
            Expression.Integer 0

        defaultString =
            Expression.Literal ""

        defaultBool =
            Expression.FunctionOrValue [] "False"

        defaultMessage =
            Expression.FunctionOrValue [] ("default" ++ String.replace "." "" field.typeName)

        defaultList =
            Expression.ListExpr []

        defaultValue =
            case field.label |> Maybe.withDefault FieldDescriptorProto.LabelOptional of
                FieldDescriptorProto.LabelOptional ->
                    field.type_ |> Maybe.map toDefault |> Maybe.withDefault defaultFail

                FieldDescriptorProto.LabelRequired ->
                    field.type_ |> Maybe.map toDefault |> Maybe.withDefault defaultFail

                FieldDescriptorProto.LabelRepeated ->
                    defaultList

        toDefault t =
            case t of
                FieldDescriptorProto.TypeDouble ->
                    defaultNumber

                FieldDescriptorProto.TypeFloat ->
                    defaultNumber

                FieldDescriptorProto.TypeInt64 ->
                    defaultNumber

                FieldDescriptorProto.TypeUint64 ->
                    defaultNumber

                FieldDescriptorProto.TypeInt32 ->
                    defaultNumber

                FieldDescriptorProto.TypeFixed64 ->
                    defaultNumber

                FieldDescriptorProto.TypeFixed32 ->
                    defaultNumber

                FieldDescriptorProto.TypeBool ->
                    defaultBool

                FieldDescriptorProto.TypeString ->
                    defaultString

                FieldDescriptorProto.TypeGroup ->
                    defaultFail

                FieldDescriptorProto.TypeMessage ->
                    defaultMessage

                FieldDescriptorProto.TypeBytes ->
                    defaultFail

                FieldDescriptorProto.TypeUint32 ->
                    defaultNumber

                FieldDescriptorProto.TypeEnum ->
                    defaultFail

                FieldDescriptorProto.TypeSfixed32 ->
                    defaultNumber

                FieldDescriptorProto.TypeSfixed64 ->
                    defaultNumber

                FieldDescriptorProto.TypeSint32 ->
                    defaultNumber

                FieldDescriptorProto.TypeSint64 ->
                    defaultNumber
    in
    ( fieldName field |> node, defaultValue |> node )


messageDecoder : Descriptor.DescriptorProto -> Function
messageDecoder message =
    { documentation = Nothing
    , signature = Just (decoderSignature message |> node)
    , declaration =
        { name = decoderName message |> node
        , arguments = []
        , expression =
            Expression.Application
                [ Expression.FunctionOrValue [ "PD" ] "message" |> node
                , Expression.FunctionOrValue [] (defaultName message) |> node
                , Expression.ListExpr (List.map (fieldDecoder >> node) message.field) |> node

                -- , Expression.ListExpr [] |> node
                ]
                |> node
        }
            |> node
    }


fieldDecoder : Descriptor.FieldDescriptorProto -> Expression.Expression
fieldDecoder field =
    let
        todo label =
            Expression.Application
                [ Expression.FunctionOrValue [ "Debug" ] "todo" |> node
                , Expression.Literal label |> node
                ]

        typeDecoder =
            case field.type_ of
                Just FieldDescriptorProto.TypeDouble ->
                    Expression.FunctionOrValue [ "PD" ] "double"

                Just FieldDescriptorProto.TypeFloat ->
                    Expression.FunctionOrValue [ "PD" ] "double"

                Just FieldDescriptorProto.TypeInt64 ->
                    Expression.FunctionOrValue [ "PD" ] "int64"

                Just FieldDescriptorProto.TypeUint64 ->
                    Expression.FunctionOrValue [ "PD" ] "uint64"

                Just FieldDescriptorProto.TypeInt32 ->
                    Expression.FunctionOrValue [ "PD" ] "int32"

                Just FieldDescriptorProto.TypeFixed64 ->
                    Expression.FunctionOrValue [ "PD" ] "fixed64"

                Just FieldDescriptorProto.TypeFixed32 ->
                    Expression.FunctionOrValue [ "PD" ] "fixed32"

                Just FieldDescriptorProto.TypeBool ->
                    Expression.FunctionOrValue [ "PD" ] "bool"

                Just FieldDescriptorProto.TypeString ->
                    Expression.FunctionOrValue [ "PD" ] "string"

                Just FieldDescriptorProto.TypeGroup ->
                    todo "group"

                Just FieldDescriptorProto.TypeMessage ->
                    Expression.FunctionOrValue [] ("decode" ++ String.replace "." "" field.typeName)

                Just FieldDescriptorProto.TypeBytes ->
                    todo "bytes"

                Just FieldDescriptorProto.TypeUint32 ->
                    Expression.FunctionOrValue [ "PD" ] "uint32"

                Just FieldDescriptorProto.TypeEnum ->
                    todo "enum"

                Just FieldDescriptorProto.TypeSfixed32 ->
                    Expression.FunctionOrValue [ "PD" ] "sfixed32"

                Just FieldDescriptorProto.TypeSfixed64 ->
                    Expression.FunctionOrValue [ "PD" ] "sfixed64"

                Just FieldDescriptorProto.TypeSint32 ->
                    Expression.FunctionOrValue [ "PD" ] "sint32"

                Just FieldDescriptorProto.TypeSint64 ->
                    Expression.FunctionOrValue [ "PD" ] "sint64"

                Nothing ->
                    todo "nothing"

        decodeField =
            case field.label of
                Just FieldDescriptorProto.LabelRepeated ->
                    Expression.FunctionOrValue [ "PD" ] "repeated"

                Just FieldDescriptorProto.LabelRequired ->
                    Expression.FunctionOrValue [ "PD" ] "required"

                _ ->
                    Expression.FunctionOrValue [ "PD" ] "optional"

        decodeType =
            Expression.Application [ typeDecoder |> node ] |> node

        getterIfRepeated =
            case field.label of
                Just FieldDescriptorProto.LabelRepeated ->
                    [ Expression.RecordAccessFunction (fieldName field) |> node
                    ]

                _ ->
                    []

        setter =
            (node << Expression.ParenthesizedExpression << node << Expression.LambdaExpression)
                { args =
                    [ Pattern.VarPattern "v" |> node
                    , Pattern.VarPattern "m" |> node
                    ]
                , expression =
                    Expression.RecordUpdateExpression ("m" |> node)
                        [ ( fieldName field |> node
                          , Expression.FunctionOrValue [] "v" |> node
                          )
                            |> node
                        ]
                        |> node
                }
    in
    (Expression.Application << List.concat)
        [ [ decodeField |> node
          , Expression.Integer field.number |> node
          , decodeType
          ]
        , getterIfRepeated
        , [ setter ]
        ]


messageEncoder : Descriptor.DescriptorProto -> Function
messageEncoder message =
    { documentation = Nothing
    , signature = Just (encoderSignature message |> node)
    , declaration =
        { name = encoderName message |> node
        , arguments = [ Pattern.VarPattern "msg" ] |> List.map node
        , expression =
            Expression.Application
                [ Expression.FunctionOrValue [ "PE" ] "message" |> node
                , Expression.ListExpr (List.map (fieldEncoder >> node) message.field) |> node
                ]
                |> node
        }
            |> node
    }


fieldEncoder : Descriptor.FieldDescriptorProto -> Expression.Expression
fieldEncoder field =
    let
        todo label =
            Expression.Application
                [ Expression.FunctionOrValue [ "Debug" ] "todo" |> node
                , Expression.Literal label |> node
                ]

        typeEncoder =
            case field.type_ of
                Just FieldDescriptorProto.TypeDouble ->
                    Expression.FunctionOrValue [ "PE" ] "double"

                Just FieldDescriptorProto.TypeFloat ->
                    Expression.FunctionOrValue [ "PE" ] "double"

                Just FieldDescriptorProto.TypeInt64 ->
                    Expression.FunctionOrValue [ "PE" ] "int64"

                Just FieldDescriptorProto.TypeUint64 ->
                    Expression.FunctionOrValue [ "PE" ] "uint64"

                Just FieldDescriptorProto.TypeInt32 ->
                    Expression.FunctionOrValue [ "PE" ] "int32"

                Just FieldDescriptorProto.TypeFixed64 ->
                    Expression.FunctionOrValue [ "PE" ] "fixed64"

                Just FieldDescriptorProto.TypeFixed32 ->
                    Expression.FunctionOrValue [ "PE" ] "fixed32"

                Just FieldDescriptorProto.TypeBool ->
                    Expression.FunctionOrValue [ "PE" ] "bool"

                Just FieldDescriptorProto.TypeString ->
                    Expression.FunctionOrValue [ "PE" ] "string"

                Just FieldDescriptorProto.TypeGroup ->
                    todo "group"

                Just FieldDescriptorProto.TypeMessage ->
                    Expression.FunctionOrValue [] ("encode" ++ String.replace "." "" field.typeName)

                Just FieldDescriptorProto.TypeBytes ->
                    todo "bytes"

                Just FieldDescriptorProto.TypeUint32 ->
                    Expression.FunctionOrValue [ "PE" ] "uint32"

                Just FieldDescriptorProto.TypeEnum ->
                    todo "enum"

                Just FieldDescriptorProto.TypeSfixed32 ->
                    Expression.FunctionOrValue [ "PE" ] "sfixed32"

                Just FieldDescriptorProto.TypeSfixed64 ->
                    Expression.FunctionOrValue [ "PE" ] "sfixed64"

                Just FieldDescriptorProto.TypeSint32 ->
                    Expression.FunctionOrValue [ "PE" ] "sint32"

                Just FieldDescriptorProto.TypeSint64 ->
                    Expression.FunctionOrValue [ "PE" ] "sint64"

                Nothing ->
                    todo "nothing"

        maybeEncodeList =
            case field.label of
                Just FieldDescriptorProto.LabelRepeated ->
                    [ Expression.FunctionOrValue [ "PE" ] "list" |> node ]

                _ ->
                    []

        encodeType =
            Expression.Application [ typeEncoder |> node ] |> node

        getter =
            Expression.RecordAccess
                (Expression.FunctionOrValue [] "msg" |> node)
                (fieldName field |> node)
                |> node
    in
    Expression.TupledExpression <|
        [ Expression.Integer field.number |> node
        , Expression.Application
            (List.concat [ maybeEncodeList, [ encodeType, getter ] ])
            |> node
        ]


defaultSignature : Descriptor.DescriptorProto -> Signature
defaultSignature message =
    { name = defaultName message |> node
    , typeAnnotation = messageType message |> node
    }


decoderSignature : Descriptor.DescriptorProto -> Signature
decoderSignature message =
    { name = decoderName message |> node
    , typeAnnotation =
        TypeAnnotation.Typed
            (( [ "PD" ], "Decoder" ) |> node)
            [ messageType message |> node ]
            |> node
    }


encoderSignature : Descriptor.DescriptorProto -> Signature
encoderSignature message =
    { name = encoderName message |> node
    , typeAnnotation =
        TypeAnnotation.FunctionTypeAnnotation
            (messageType message |> node)
            (TypeAnnotation.Typed (( [ "PE" ], "Encoder" ) |> node) [] |> node)
            |> node
    }


messageType : Descriptor.DescriptorProto -> TypeAnnotation
messageType message =
    TypeAnnotation.Typed (( [], message.name ) |> node) []


messageRecordDefinition : Descriptor.DescriptorProto -> TypeAnnotation.RecordDefinition
messageRecordDefinition message =
    let
        field f =
            ( fieldName f |> node, fieldType f |> node )
    in
    message.field |> List.map (field >> node)


fieldType : Descriptor.FieldDescriptorProto -> TypeAnnotation
fieldType field =
    let
        toType t =
            case t of
                FieldDescriptorProto.TypeDouble ->
                    TypeAnnotation.Typed (( [], "Float" ) |> node) []

                FieldDescriptorProto.TypeFloat ->
                    TypeAnnotation.Typed (( [], "Float" ) |> node) []

                FieldDescriptorProto.TypeInt64 ->
                    TypeAnnotation.Typed (( [], "Int" ) |> node) []

                FieldDescriptorProto.TypeUint64 ->
                    TypeAnnotation.Typed (( [], "Int" ) |> node) []

                FieldDescriptorProto.TypeInt32 ->
                    TypeAnnotation.Typed (( [], "Int" ) |> node) []

                FieldDescriptorProto.TypeFixed64 ->
                    TypeAnnotation.Typed (( [], "Int" ) |> node) []

                FieldDescriptorProto.TypeFixed32 ->
                    TypeAnnotation.Typed (( [], "Int" ) |> node) []

                FieldDescriptorProto.TypeBool ->
                    TypeAnnotation.Typed (( [], "Bool" ) |> node) []

                FieldDescriptorProto.TypeString ->
                    TypeAnnotation.Typed (( [], "String" ) |> node) []

                FieldDescriptorProto.TypeGroup ->
                    TypeAnnotation.Typed (( [], "Never" ) |> node) []

                FieldDescriptorProto.TypeMessage ->
                    TypeAnnotation.Typed (( [], String.replace "." "" field.typeName ) |> node) []

                FieldDescriptorProto.TypeBytes ->
                    TypeAnnotation.Typed (( [], "Never" ) |> node) []

                FieldDescriptorProto.TypeUint32 ->
                    TypeAnnotation.Typed (( [], "Int" ) |> node) []

                FieldDescriptorProto.TypeEnum ->
                    TypeAnnotation.Typed (( [], "Never" ) |> node) []

                FieldDescriptorProto.TypeSfixed32 ->
                    TypeAnnotation.Typed (( [], "Int" ) |> node) []

                FieldDescriptorProto.TypeSfixed64 ->
                    TypeAnnotation.Typed (( [], "Int" ) |> node) []

                FieldDescriptorProto.TypeSint32 ->
                    TypeAnnotation.Typed (( [], "Int" ) |> node) []

                FieldDescriptorProto.TypeSint64 ->
                    TypeAnnotation.Typed (( [], "Int" ) |> node) []

        type_ =
            field.type_
                |> Maybe.map toType
                |> Maybe.withDefault (TypeAnnotation.GenericType "Never")
    in
    case field.label |> Maybe.withDefault FieldDescriptorProto.LabelOptional of
        FieldDescriptorProto.LabelOptional ->
            type_

        FieldDescriptorProto.LabelRequired ->
            type_

        FieldDescriptorProto.LabelRepeated ->
            TypeAnnotation.Typed (( [], "List" ) |> node) [ type_ |> node ]



-- NAMES


defaultName : Descriptor.DescriptorProto -> String
defaultName message =
    ("default_" ++ message.name) |> String.camelize


decoderName : Descriptor.DescriptorProto -> String
decoderName message =
    ("decode_" ++ message.name) |> String.camelize


encoderName : Descriptor.DescriptorProto -> String
encoderName message =
    ("encode_" ++ message.name) |> String.camelize


moduleName : Descriptor.FileDescriptorProto -> ModuleName
moduleName file =
    [ file.name |> String.replace ".proto" "" |> String.classify ]


fieldName : Descriptor.FieldDescriptorProto -> String
fieldName field =
    field.name |> String.camelize


fileName : File -> String
fileName file =
    file.moduleDefinition
        |> Node.value
        |> Module.moduleName
        |> String.join "/"
        |> (\name -> name ++ ".elm")



-- HELPERS


node : a -> Node a
node x =
    Node emptyRange x
