#!/bin/bash
# Minifies syntaxhighlighter.js

cd "../../third-party/syntaxhighlighter"

SRC_FILE="./dist/syntaxhighlighter.js"
MIN_FILE="./dist/syntaxhighlighter.min.js"

# Append license header
head -16 "${SRC_FILE}" >"${MIN_FILE}"

# Minify the script
./node_modules/uglify-js/bin/uglifyjs -m <"${SRC_FILE}" >>"${MIN_FILE}"
