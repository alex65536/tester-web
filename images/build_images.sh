#!/bin/bash

function build_one {
	inkscape "$1.svg" -e "$1.png" -w "$2"
	rm -f "../data/$1.png"
	cp "$1.png" "../data"
}

build_one "userAdmin"     64
build_one "userOwner"     64
build_one "userSimple"    64
build_one "userBlocked"   64
build_one "userEditor"    64

build_one "tickMark"      32
build_one "crossMark"     32
build_one "crossMarkGray" 32
