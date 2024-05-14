# This is meant to be invoked while
# the CWD is the swarm repository root.

FROM quay.io/benz0li/ghc-musl:9.6.4 as hs

LABEL org.opencontainers.image.authors="Karl Ostmo <kostmo@gmail.com>"
ENV TZ=America/Los_Angeles

RUN \
	apk add --no-cache git curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl && \
	apk add --no-cache zlib zlib-dev zlib-static ncurses-static

# install ghcup
RUN \
    curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

ARG GHC=9.6.4

# install GHC and cabal
RUN ghcup -v install ghc --isolate /usr/local --force ${GHC}

RUN mkdir -p /opt/swarm

FROM hs as system-build-deps

WORKDIR /opt/swarm

COPY ./swarm.cabal /opt/swarm/swarm.cabal
RUN cabal update

# Must manually list the transitive closure of "internal" dependencies (sublibraries)
# of our executable.
# Note that we avoid simply listing all sublibraries here
# (i.e. scripts/gen/list-sublibraries.sh) because that
# includes 'swarm:swarm-web' and will build pandoc.
RUN cabal build --only-dependencies \
  swarm:swarm-host-tournament \
  swarm:swarm-tournament \
  swarm:swarm-engine \
  swarm:swarm-lang \
  swarm:swarm-scenario \
  swarm:swarm-util

COPY ./src /opt/swarm/src
COPY ./app /opt/swarm/app

# The following are not strictly needed for compiling the
# selected dependencies, but 'cabal build' spews warnings
# when they are absent
COPY ./test /opt/swarm/test
COPY ./CHANGELOG.md /opt/swarm/CHANGELOG.md
COPY ./LICENSE /opt/swarm/LICENSE

COPY ./tournament/scripts/docker/build-server-executable.sh /opt/swarm/build-server-executable.sh

RUN cd /opt/swarm && ./build-server-executable.sh /opt/swarm/tournament-bin
