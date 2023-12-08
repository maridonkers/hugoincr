CONTAINER="docker-static-linux-x64-hugoincr-1"

define usage =
@echo ""
@echo "Start the container and run build.sh to build the hugoincr static executable:"
@echo ""
@echo "make up"
@echo "make shell"
@echo "build.sh"
@echo "exit"
@echo ""
endef

all: help

up:
	xhost +LOCAL:
	mkdir -p ~/lib/haskell/.cabal ~/lib/haskell/.local ~/lib/haskell/.stack ~/lib/haskell/usr/local
	docker-compose up --detach

down:
	sync
	docker-compose down

# If problems persist after a force-down then manually restart Docker daemon.
force-down:
	sync
	docker rm -f $(CONTAINER)

ls:
	docker ps -a

images:
	docker images

rebuild:
	docker-compose build --no-cache
	$(usage)

build:
	docker-compose build
	$(usage)

attach:
	xhost +LOCAL:
	docker attach $(CONTAINER)

shell:
	xhost +LOCAL:
	docker exec -it $(CONTAINER) /bin/bash

help:
	@grep '^[^ 	#:]\+:' Makefile | sed -e 's/:[^:]*//g'
	@echo -e "\nRun make shell prior to building site.hs.\n"
