#!/bin/sh

cp ../package.zip image/files/tsweb.zip

docker build -t tsweb image
