#!/bin/sh

if [ -z "$(ls /var/tsweb --hide='lost+found')" ]; then
	cd /opt/tsweb/bin && ./tsweb-mkcfg
	sed -i -E -e 's/^port=[0-9]+$/port=80/' /var/tsweb/data/config.ini
fi

cd /opt/tsweb/bin || exit 1
./tsweb &

CHILD=$!

trap 'kill -s INT "$CHILD"' INT TERM

wait $CHILD
