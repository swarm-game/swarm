# This is meant to be invoked while
# the CWD is the swarm repository root.

FROM amazonlinux:latest as amz
LABEL org.opencontainers.image.authors="Karl Ostmo <kostmo@gmail.com>"

ENV TZ=America/Los_Angeles

# OS dependencies
RUN yum -y update && yum -y install \
    postgresql-devel

# The 'python' executable is required to run the AWS CLI installer
#RUN yum -y install python
RUN ln -s /usr/bin/python3 /usr/bin/python

RUN curl https://s3.amazonaws.com/aws-cli/awscli-bundle.zip -o awscli-bundle.zip
RUN unzip awscli-bundle.zip
RUN ./awscli-bundle/install -i /usr/local/aws -b /usr/local/bin/aws

RUN mkdir -p /opt/swarm

FROM amz as system-build-deps

# These packages are only needed at build time, not at runtime.
RUN yum -y install \
    ncurses-compat-libs \
    gmp \
    gmp-devel \
    zlib-devel \
    fftw3-devel \
    xz-devel \
    nss \
    nss-devel \
    openssl-devel \
    gcc \
    gcc-c++ \
    make \
    tar

FROM system-build-deps as haskell-compilation-layer

# install ghcup
RUN \
    curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

ARG GHC=9.6.4

# install GHC and cabal
RUN \
    ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin


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

COPY tournament/scripts/docker/build-server-executable.sh /opt/swarm/build-server-executable.sh
RUN /opt/swarm/build-server-executable.sh /opt/swarm/tournament-bin

FROM amz

COPY --from=haskell-compilation-layer /opt/swarm/tournament-bin /opt/swarm/tournament-bin
COPY ./data /root/.local/share/swarm/data
COPY ./tournament/web /root/tournament/web

# This was produced by the parent script, 'build-image.sh'.
COPY ./git-hash.txt /root/git-hash.txt

EXPOSE 8080

# We begin initially with CWD as the filesystem root, "/".
# We first 'cd' into the home directory, which is "/root", so we
# have access to the static web files.
CMD cd && /opt/swarm/tournament-bin --port 8080 --version $(cat git-hash.txt)
