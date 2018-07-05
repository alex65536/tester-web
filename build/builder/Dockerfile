FROM debian:9.4

RUN	echo 'deb http://ftp.debian.org/debian stretch-backports main' >> /etc/apt/sources.list \
	&& apt-get update \
	&& apt-get install -y --no-install-recommends -t stretch-backports \
		lazarus fpc gcc zip unzip

VOLUME /var/build

WORKDIR /var/build

COPY files/start-build.sh /bin/start-build.sh

RUN chmod 0744 /bin/start-build.sh

ENTRYPOINT ["/bin/start-build.sh"]
