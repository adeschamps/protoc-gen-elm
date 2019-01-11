This is a work in progress Protocol Buffer compiler plugin for Elm.

It generates Elm type definitions from a `.proto` file, as well as
binary encoders and decoders using the APIs and wire format
implemented by <https://github.com/eriktim/elm-protocol-buffers>.

# Quick start

Since it's currently unpublished, clone
https://github.com/eriktim/elm-protocol-buffers and modify `elm.json`
so that it appears in the `source-directories` list.

```bash
# Build the plugin and run the protobuf compiler:
$ make generator
$ mkdir test_elm
$ protoc --plugin=protoc-gen-elm --elm-out=test_elm test.proto

# Or use this shortcut:
$ make elm-defs
```

# Status

This plugin can currently generate valid type definitions, encoders,
and decoders for simple `.proto` files. It doesn't yet handle multiple
files well, or nested types, enums, oneofs, or a variety of other
protobuf features.
