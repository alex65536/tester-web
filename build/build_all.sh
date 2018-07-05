#!/bin/bash
# Builds everything that is necessary after cloning Tester Web from a git repo

cd ../tester &&
make build-timerlib &&
cd ../build &&
./build_project.sh &&
./build_package.sh
