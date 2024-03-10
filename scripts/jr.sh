#!/bin/bash

# ------------------------------------------------------------------
# Useful information about this script
# ------------------------------------------------------------------
# VERSION=0.1.0

# # sha256 of "jr"
# SUBJECT=e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855

# # usage helper
# USAGE="Usage: jr {list, edit, help, [HOSTNAME]}"

# # include shflag
# # -> % ./thirdparty/shell-script-template/shflags/shflags
# . ../src/shflags

# #define the parameters for this script
# DEFINE_string 'option' 'help' 'options to call jr, example: {list, edit, help}' o

# # parse command line
# FLAGS "$@" || exit 1
# eval set -- "${FLAGS_ARGV}"
# shift $(($OPTIND - 1))
# option=$1

# # ------------------------------------------------------------------
# # Locks, in case of multiple invocations
# # ------------------------------------------------------------------
# LOCK_FILE=/tmp/${SUBJECT}.lock

# if [ -f "$LOCK_FILE" ]; then
# echo "Script is already running"
# exit
# fi

# trap "rm -f $LOCK_FILE" EXIT
# touch $LOCK_FILE

# # ------------------------------------------------------------------
# # Body: the main logic of this script
# # ------------------------------------------------------------------

# case $FLAGS_option in
#     list)
#         cat ~/.ssh/config | sed -n 's/Host//p' | sed 's/Name//g' | sed 'N;s/\n//'
#         ;;
#     *)
#         echo "Param A: $FLAGS_option"
#         ;;
# esac


# Define functions
PRINT_HELP() {
    echo "-------------------------------------------"
    echo "A tool for managing jamie's remote machines"
    echo "-------------------------------------------"
    echo "Avaliable Machines:"
    cat ~/.ssh/config | sed -n 's/Host//p' | sed 's/Name//g' | sed 'N;s/\n//'
}


if [ "$1" = "list" ]; then
    PRINT_LIST
    exit 0
elif [ "$1" = "help" ]; then
    PRINT_HELP
    exit 0
elif [ "$1" = "edit" ]; then
    vim ~/.ssh/config
else
    ssh $1 -t zsh
fi
