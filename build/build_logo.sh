#!/bin/bash
# Builds logo from SVG (inkscape required)

LOGOS=()

function build_logo {
	SIZE=$1
	inkscape "logo.svg" -e "logo${SIZE}.png" -w "${SIZE}"
	LOGOS+=( "logo${SIZE}.png" )
}

cd "../logo"

inkscape "logoWithText.svg" -e "logoWithText.png" -w 192
rm -f "../data/logoWithText.png"
cp "logoWithText.png" "../data"

build_logo 16
build_logo 32
convert "${LOGOS[@]}" "logo.ico"
cp "logo.ico" "../data/logo.ico"
