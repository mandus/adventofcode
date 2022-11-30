#!/bin/bash
# Advent of Code 2018 - Åsmund Ødegård

IFS=$'\n' read -d '' -r -a values <<< $(sort $1)

printsame() {
    declare -a w1
    while read -n 1 c ; do w1+=($c); done <<< "$1"
    declare -a w2
    while read -n 1 c ; do w2+=($c); done <<< "$2"

    for ((i=0; i<${#w1[@]}; i++)) ; do
        if [[ ${w1[$i]} == ${w2[$i]} ]] ; then
            echo -ne ${w1[$i]}
        fi
    done
    echo ''
}

prev=''
for val in "${values[@]}" ; do 
    if [[ -z $prev ]] ; then 
        prev=$val
        echo "prev $prev"
        continue
    fi

    declare -a prevarray
    while read -n 1 c ; do prevarray+=($c); done <<< "$prev"
    declare -a valarray
    while read -n 1 c; do valarray+=($c); done <<< "$val"

    declare -i diffby=0
    for ((i=0; i<${#prevarray[@]}; i++)) ; do
        if [[ ${prevarray[$i]} != ${valarray[$i]} ]] ; then 
            diffby+=1
        fi
        if [[ $diffby>1 ]] ; then
            continue
        fi
    done

    if [[ $diffby == 1 ]] ; then
        echo "$prev is almost $val"
        printsame $prev $val
        break
    fi

    prev=$val
    unset prevarray
    unset valarray
done

