#!/bin/sh

cd tester/timerlib \
	&& bash build-linux.sh \
	&& cd ../../build \
	&& bash build_all.sh
