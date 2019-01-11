NODE_BINARIES=./node_modules/.bin
ELM=$(NODE_BINARIES)/elm

help: ## Prints a help guide
	@echo "Available tasks:"
	@grep -E '^[\%a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

elm-defs: generator ## Generate Elm code from test.proto
	mkdir -p test_elm
	@echo "Generating Elm code from .proto file..."
	protoc --plugin=protoc-gen-elm --elm_out=test_elm test.proto
	@echo "Compiling generated Elm code..."
	$(ELM) make test_elm/Test.elm

elm-plugin: generator ## Generate protobuf elm definitions
	mkdir -p generated_plugin_api
	protoc --plugin=protoc-gen-elm --elm_out=generated_plugin_api /usr/include/google/protobuf/compiler/plugin.proto

generator: ## Compile the elm generator
	$(ELM) make src/Main.elm --output=elm.js

generator-optimized: ## Compile the elm generator with optimizations
	$(ELM) make src/Main.elm --output=elm.js --optimize
