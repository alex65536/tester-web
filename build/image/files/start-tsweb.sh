#!/bin/sh

if [ -z "$(ls /var/tsweb --hide='lost+found')" ]; then
	cd /opt/tsweb/bin && ./tsweb-mkcfg
	sed -i -E -e 's/^port=[0-9]+$/port=80/' /var/tsweb/data/config.ini
fi

trap 'kill -INT $CHILD && wait $CHILD' INT TERM

cd /opt/tsweb/bin || exit 1
./tsweb &
CHILD=$!
wait $CHILD
