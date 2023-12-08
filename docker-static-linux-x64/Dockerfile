# Copied and adapted from HLedger -- https://github.com/simonmichael/hledger
#
FROM debian:latest

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get -qq update && apt-get install -y --no-install-recommends tzdata && apt-get install -y gcc g++ make xz-utils wget vim libtinfo5 git python3 llvm clang ghc cabal-install libffi-dev libgmp-dev libtinfo-dev zlib1g-dev locales && rm -rf /var/lib/apt/lists/*

# Timezone is also in docker-compose file.
ENV HOME /root
ENV TZ Europe/Amsterdam
ENV SHELL /bin/bash

# Create a non-root account to run Haskell tooling with.
RUN useradd -ms /bin/bash --uid 1000 --gid 100 haskell; \
    usermod -G audio,video,sudo haskell; \
    echo "haskell:1Jd2XuAnjk1RydzsDsR0RAqCu05lNrxsX7JvpchPcXk" | chpasswd
    # echo "haskell:$ENV_DOCKER_HASKELL_PASSWORD" | chpasswd

USER haskell
WORKDIR /home/haskell

RUN mkdir -p /home/haskell/bin
ENV HOME /home/haskell
ENV PATH="${PATH}:/home/haskell/bin:/home/haskell/.local/bin"
ENV LC_ALL=C.UTF-8

# Update cabal packages.
RUN cabal update

COPY ./build.sh /home/haskell/bin/.
ENV DISPLAY=":0"

# Start the container and run build.sh to build the hugoincr static executable.
