#!/bin/bash
# Advent of Code 2018 - Åsmund Ødegård

#IFS=', ' read -r -a values <<< $*
IFS=$'\n' read -d '' -r -a values < $1

declare -i freq=0
declare -A seen

output() {
    curfreq=$1
    val=$2
    freq=$3
    theend=$4
    echo "Current frquency ${curfreq}, change of ${val}; resulting frequency ${freq}${theend}"
}

while : ; do
    for val in "${values[@]}" ; do
        curfreq=$freq
        freq=$((freq "$val"))
        theend="."
        if [ ${seen[$freq]+_} ] ; then
            output $curfreq $val $freq ", which has already been seen."
            exit 0
        else
            seen[$freq]='y'
        fi
        output $curfreq $val $freq "."
    done
done
