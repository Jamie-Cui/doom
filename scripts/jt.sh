#!/bin/bash

RED="\e[31m"
GREEN="\e[32m"
ENDCOLOR="\e[0m"
TIMESTAMP=$(date +%s)
FILENAME=${TIMESTAMP}\_$1

# please config the following
DOCKER_NAME="sz-benchmark-32"
REMOTE_MACHINE="test"
REMOTE_TMP_PATH="/home/shanzhu.cjm/benchmark/bin/tmp/${FILENAME}"

RELATIVE_PATH=$(git rev-parse --show-prefix)
ROOT_PATH=$(git rev-parse --show-toplevel)

echo -e "$ROOT_PATH"
echo -e "$RELATIVE_PATH"

# Define functions
PRINT_HELP() {
    echo "-------------------------------------------"
    echo "A tool for remote-benchmarking bazel target"
    echo "-------------------------------------------"
    echo "Usage:"
    echo "  jt [target]"
}

# first build the target with "-c opt" flag
echo -e "${GREEN}=============================${ENDCOLOR}"
echo -e "${GREEN}Bazel: Build target $1 ${ENDCOLOR}"
echo -e "${GREEN}=============================${ENDCOLOR}"
bazel build $1 -c opt

# then send the binary to remote "test" machine
echo -e "${GREEN}=============================${ENDCOLOR}"
echo -e "${GREEN}Remote copy binary ${ENDCOLOR}"
echo -e "${GREEN}=============================${ENDCOLOR}"
BIN_PATH=${ROOT_PATH}/bazel-bin/${RELATIVE_PATH}$1
echo -e "${RED}$BIN_PATH => ${REMOTE_MACHINE}:${REMOTE_TMP_PATH}}${ENDCOLOR}"
scp ${BIN_PATH} ${REMOTE_MACHINE}:${REMOTE_TMP_PATH} 1>/dev/null

# remote run docker command
echo -e "${GREEN}=============================${ENDCOLOR}"
echo -e "${GREEN}Remote run test in docker ${ENDCOLOR}"
echo -e "${GREEN}=============================${ENDCOLOR}"
CMD="docker exec ${DOCKER_NAME} ./tmp/${FILENAME}"
echo -e "${RED}${CMD}${ENDCOLOR}"
ssh ${REMOTE_MACHINE} $CMD
