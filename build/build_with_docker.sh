#!/bin/sh

docker build -t tsweb-builder builder

docker run --rm -v "$(realpath ..)":/var/build tsweb-builder
