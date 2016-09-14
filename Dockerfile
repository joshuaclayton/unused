FROM haskell:8.0.1

RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y silversearcher-ag && rm -rf /var/lib/apt/lists/*

COPY . /app

WORKDIR /app
RUN stack setup && stack install

WORKDIR /code
