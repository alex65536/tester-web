#!/bin/bash

inkscape logoWithText.svg -e logoWithText.png -w 192
rm -f ../data/logoWithText.png
cp logoWithText.png ../data
