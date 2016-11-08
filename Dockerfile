FROM haskell:8.0.1

ENV AG_VERSION 0.33.0

RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y \
                    wget \
                    automake \
                    pkg-config \
                    libpcre3-dev \
                    zlib1g-dev \
                    liblzma-dev && \
                    rm -rf /var/lib/apt/lists/*

RUN gpg --keyserver keyserver.ubuntu.com --recv 3F0A04B6 && \
  wget http://geoff.greer.fm/ag/releases/the_silver_searcher-$AG_VERSION.tar.gz -O /tmp/ag.tar.gz && \
  wget http://geoff.greer.fm/ag/releases/the_silver_searcher-$AG_VERSION.tar.gz.asc -O /tmp/ag.tar.gz.asc && \
  gpg --verify  /tmp/ag.tar.gz.asc && \
  tar --directory /tmp -xvzf /tmp/ag.tar.gz && \
  cd /tmp/the_silver_searcher* && ./configure && make && make install && \
  rm -r /tmp/*

COPY . /app

WORKDIR /app
RUN stack setup && stack install

WORKDIR /code
