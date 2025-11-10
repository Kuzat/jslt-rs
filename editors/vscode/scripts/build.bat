@echo off
echo Building JSLT LSP server...

cd ..\..
cargo build --release -p jslt-lsp

if not exist editors\vscode\bin mkdir editors\vscode\bin
copy target\release\jslt-lsp.exe editors\vscode\bin\

echo Binary copied to editors\vscode\bin\