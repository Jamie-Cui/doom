#!/bin/bash

# $2 : cpu cores

YOUR_NAME=sz
YOUR_DOCKER_PATH=$HOME/Desktop/docker
DOCKER_IMAGE=secretflow/ubuntu-base-ci:latest

PRINT_HELP() {
        echo "-------------------------------------------------------"
        echo "A tool for managing jamie's benchmark docker containers"
        echo "-------------------------------------------------------"
        echo "Usage:"
        echo "  jd [Commands]"
        echo "Commands:"
        echo "  build + [cpu core]      Build docker container"
        echo "  in + [cpu core]         Enter docker container"

}

if [ "$1" = "build" ]; then
        if [ $# -eq 1 ]; then
                echo "Docker cpu cores: 16 (default)"
                echo "Docker name: $YOUR_NAME-benchmark-16"
                echo "Docker path: $YOUR_DOCKER_PATH"
                docker run -d -it --name $YOUR_NAME-benchmark-16 --mount type=bind,source="$YOUR_DOCKER_PATH",target=/home/admin/dev/ -w /home/admin/dev --cap-add=SYS_PTRACE --security-opt seccomp=unconfined --cap-add=NET_ADMIN --privileged=true --cpus=16 $DOCKER_IMAGE
        else
                echo "Docker cpu cores: $2"
                echo "Docker name: $YOUR_NAME-benchmark-$2"
                echo "Docker path: $YOUR_DOCKER_PATH"
                docker run -d -it --name sz-benchmark-$2 --mount type=bind,source="$YOUR_DOCKER_PATH",target=/home/admin/dev/ -w /home/admin/dev --cap-add=SYS_PTRACE --security-opt seccomp=unconfined --cap-add=NET_ADMIN --privileged=true --cpus=$2 $DOCKER_IMAGE
        fi
        exit 0
elif [ "$1" = "in" ]; then
        if [ $# -eq 1 ]; then
                echo "Docker cpu cores: 16 (default)"
                echo "Docker name: $YOUR_NAME-benchmark-16"
                docker exec -it $YOUR_NAME-benchmark-16 /bin/bash
        else
                echo "Docker cpu cores: $2"
                echo "Docker name: $YOUR_NAME-benchmark-$2"
                docker exec -it $YOUR_NAME-benchmark-$2 /bin/bash
        fi

        exit 0
elif [ "$1" = "pull" ]; then
        docker pull ${DOCKER_IMAGE}
else
        PRINT_HELP
        exit 0
fi
