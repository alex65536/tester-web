#!/bin/sh

if [ -z "$(ls /var/tsweb --hide='lost+found')" ]; then
	# TODO: Add support for auto-generating passwords on first run!
	echo 'Admin account created!'
	echo 'Login: admin'
	echo 'Password: password'
	
	rsync -a /etc/.copy/var/tsweb/* /var/tsweb
fi

cd /opt/tsweb/bin && ./tsweb-x86_64-linux
