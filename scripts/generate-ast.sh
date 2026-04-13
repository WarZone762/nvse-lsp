#!/usr/bin/env sh

BIN=./target/release/nvse-lsp

for file in $(find test_data/new_compiler/cases -type f -name "*.gek"); do
    echo "Generating AST for $file"
    ast_file="${file/cases/ast}"
    ast_file="${ast_file/\.gek/\.ast}"
    ${BIN} generate-ast "$file" -o "$ast_file"
done
