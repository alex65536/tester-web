#!/bin/bash
# Builds Tester Web from sources

TIMERLIB_PATH="../tester/timerlib"

cd "../tester/tsrun"

# Build and copy TsRun
lazbuild -B "tsrun.lpi"
cp "tsrun" "tsrun.exe" "../../src" >&/dev/null

cd "../../src"

# Copy libs
rm libtimer*.so libtimer*.dll >&/dev/null
cp "${TIMERLIB_PATH}"/libtimer*.so "${TIMERLIB_PATH}"/libtimer*.dll . >&/dev/null

# Build the project
lazbuild -B tsweb.lpi
