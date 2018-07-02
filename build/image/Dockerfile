FROM debian:9.4

RUN	apt-get update \
	&& apt-get install -y --no-install-recommends \
		rsync \
		fp-compiler \
		gcc \
		g++ \
		unzip

COPY files/tsweb.zip /tmp/tsweb.zip

RUN	unzip /tmp/tsweb.zip -d /etc \
	&& rm -f /tmp/tsweb.zip \
	&& apt-get --purge remove -y \
		unzip \
	&& ln -s /var/tsweb /root/tsweb

COPY files/config.ini /etc/.copy/var/tsweb/data/config.ini

VOLUME /var/tsweb

EXPOSE 80

COPY files/start-tsweb.sh /start-tsweb.sh

RUN chmod 0744 /start-tsweb.sh

ENTRYPOINT ["/start-tsweb.sh"]
