#!/bin/bash
# Builds logo from SVG (inkscape required)

cd "../logo"

inkscape "logoWithText.svg" -e "logoWithText.png" -w 192
rm -f "../data/logoWithText.png"
cp "logoWithText.png" "../data"
