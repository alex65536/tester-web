#!/bin/sh

docker build -t tsweb-builder builder

docker run -v "$(realpath ..)":/var/build tsweb-builder
