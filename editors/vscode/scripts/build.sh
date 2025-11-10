set -e

echo "Building JSLT LSP server ..."

# Setup command line arguments
# 1. dev or release build default dev
BUILD_TYPE=$1

cd "../.."


if [[ "$BUILD_TYPE" == "release" ]]; then
    cargo build --release -p jslt-lsp
    BUILD_DIR="target/release"
else
  cargo build  -p jslt-lsp
  BUILD_DIR="target/debug"
fi

# Create bin directory in the extension
mkdir -p "editors/vscode/bin"

echo "COPYING from $(pwd)"
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
  cp "$BUILD_DIR/jslt-lsp.exe" editors/vscode/bin/
else
  cp "$BUILD_DIR/jslt-lsp" editors/vscode/bin/
fi

echo "Binary copied to editors/vscode/bin/"