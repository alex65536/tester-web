#!/bin/bash
# Builds Tester Web release package

cd ..

SEED="${RANDOM}"
TMP_DIR="${PWD}/tsweb-build-${SEED}"
TARGET_FILE="${PWD}/package.zip"

if [ -d "${TMP_DIR}" ]
then
	echo "Build conflict. Please try to re-launch the script"
	exit
fi

# Remove old archive
rm -f "${TARGET_FILE}" >&/dev/null

# Create temp directory
mkdir "${TMP_DIR}"
mkdir "${TMP_DIR}/tsweb"
mkdir "${TMP_DIR}/tsweb/bin"

# Copy "data" and "templates"
cp -rP "data" "templates" "${TMP_DIR}/tsweb"
find "${TMP_DIR}/tsweb" -name "~dataRoot;" -delete

# Copy binaries
cp src/tsrun{,.exe} src/tsweb{,.exe} src/tsweb-mkcfg{,.exe} src/*.so src/*.dll "${TMP_DIR}/tsweb/bin" >&/dev/null

# Build zip archive
cd "${TMP_DIR}"
zip -9rT "${TARGET_FILE}" ./*

# Remove temp directory
rm -rf "${TMP_DIR}"
