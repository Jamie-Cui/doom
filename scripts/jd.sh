#!/bin/bash

DOCKER_USER_NAME=sz
DOCKER_PATH=$PWD
DOCKER_CORES=8
DOCKER_IMAGE=ubuntu

PRINT_HELP() {
        echo "======================================================="
        echo "A tool for managing jamie's dev docker containers"
        echo "======================================================="
        echo "Usage:"
        echo "  jd [Commands]"
        echo "Commands:"
        echo "  build + [cpu core]      Build docker container"
        echo "  in + [cpu core]         Enter docker container"

}

if [ "$1" = "build" ]; then
        echo "Docker cpu cores: $DOCKER_CORES"
        echo "Docker name: $DOCKER_USER_NAME-dev-$DOCKER_CORES"
        echo "Docker path: $DOCKER_PATH"
        docker run -d -it\
                --name $DOCKER_USER_NAME-dev-$DOCKER_CORES\
                --mount type=bind,source="$DOCKER_PATH",target=/home/admin/dev/\
                -w /home/admin/dev\
                --cap-add=SYS_PTRACE\
                --security-opt seccomp=unconfined\
                --cap-add=NET_ADMIN\
                --privileged=true\
                --cpus=$DOCKER_CORES\
                $DOCKER_IMAGE
        exit 0
elif [ "$1" = "in" ]; then
        echo "Docker cpu cores: $DOCKER_CORES"
        echo "Docker name: $DOCKER_USER_NAME-dev-$DOCKER_CORES"
        docker exec -it $DOCKER_USER_NAME-dev-$DOCKER_CORES /bin/bash
        exit 0
elif [ "$1" = "pull" ]; then
        docker pull ${DOCKER_IMAGE}
else
        PRINT_HELP
        exit 0
fi
