FROM debian:9.4

RUN	apt-get update \
	&& apt-get install -y --no-install-recommends \
		rsync \
		fp-compiler \
		gcc \
		g++ \
		unzip

COPY files/tsweb.zip /tmp/tsweb.zip

RUN	unzip /tmp/tsweb.zip -d /opt \
	&& rm -f /tmp/tsweb.zip \
	&& apt-get --purge remove -y \
		unzip \
	&& ln -s /var/tsweb /root/tsweb

VOLUME /var/tsweb

EXPOSE 80

COPY files/start-tsweb.sh /bin/start-tsweb.sh

RUN chmod 0744 /bin/start-tsweb.sh

ENTRYPOINT ["/bin/start-tsweb.sh"]
