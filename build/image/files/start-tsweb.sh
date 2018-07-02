#!/bin/sh

if [ -z "`ls /var/tsweb --hide='lost+found'`" ] 
then
	echo 'Admin account created!'
	echo 'Login: admin'
	echo 'Password: password'

	rsync -a /etc/.copy/var/tsweb/* /var/tsweb
fi

cd /etc/tsweb/bin && ./tsweb-x86_64-linux
