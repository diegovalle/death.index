FROM rocker/geospatial:3.5.2

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
        ca-certificates \
        libxt-dev \
 && rm -rf /var/lib/apt/lists/*

COPY lib/load_libraries.R .

RUN Rscript load_libraries.R
