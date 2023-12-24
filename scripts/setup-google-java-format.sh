#!/bin/bash

RED="\033[0;31m"
GREEN="\033[0;32m"  # <-- [0 means not bold
YELLOW="\033[1;33m" # <-- [1 means bold
CYAN="\033[1;36m"
# ... Add more colors if you like
NC="\033[0m" # No Color



# this checks if a shell command returns successfully
# usage: check_command type/your/command
check_command() {
    command
    if [ $? -eq 0 ]; then
        return 0
    else
        return 1
    fi
}

cecho(){
    printf "${!1}${2} ${NC}\n" # <-- bash
}

if [ -f /usr/local/bin/google-java-format ]
then
   cecho "YELLOW" "[warn] File /usr/local/bin/google-java-format already exists, exiting..."
   exit 0
fi

# define the location of google-java-format-*.jar
JAR_FILE=$PWD/../data/"google-java-format-1.19.1-all-deps.jar"

# setup the command into system path
echo "Since ./setup-google-java-format is to setup a \"google-java-format\" command into your system path, therefore you may be promoted to enter your password"
echo "java -jar $JAR_FILE" | sudo tee /usr/local/bin/google-java-format

# check is the command has succeed
check_command google-java-format

# $? = the exit status of the last executed command.
if [ $? -eq 0 ]; then
    echo "Setup succeeded"
else
    sudo rm -rf /usr/local/bin/google-java-format
    echo "Setup failed, removed /usr/local/bin/google-java-format"
fi
