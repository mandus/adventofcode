#!/bin/bash
# Advent of Code 2018 - Åsmund Ødegård

IFS=$'\n' read -d '' -r -a values < $1

declare -i counttwo=0
declare -i countthree=0
declare -i dup
declare -i trip

for val in "${values[@]}" ; do 

    # count chars in an assoc array
    declare -A seen
    for c in $(grep -o . <<< "$val") ; do
        seen[$c]=$((${seen[$c]:-0} + 1))
    done

    # find 2's and 3's - stop if both found
    dup=0
    trip=0
    for key in "${!seen[@]}" ; do 
        if [ ${seen[$key]} = 2 ] ; then 
            dup=1
        else 
            if [ ${seen[$key]} = 3 ] ; then
                trip=1
            fi
        fi
        if [[ $dup>0 && $trip>0 ]] ; then
            break
        fi
    done

    # increment global checksum counters
    counttwo+=dup
    countthree+=trip
    unset seen
done

echo "Checksum: $((counttwo * countthree))"
