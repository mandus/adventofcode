#!/bin/bash
# Advent of Code 2018 - Åsmund Ødegård

IFS=$'\n' read -d '' -r -a values < $1

declare -A mark
declare -i left top width height leftoffset topoffset
declare -i nummarked=0 

for line in "${values[@]}" ; do

    echo "${line}"

    read -r left top <<< $(sed 's/\(.*\),\(.*\):/\1 \2/' <<< $(awk '{print $3}' <<< $line))
    read -r width height <<< $(sed 's/\(.*\)x\(.*\)/\1 \2/' <<< $(awk '{print $4}' <<< $line))
    for ((i=0; i<width; i++)) ; do
        for ((j=0; j<height; j++)) ; do
            leftoffset=left+i
            topoffset=top+j
            idx="${leftoffset}x${topoffset}"
            mark[$idx]=$((${mark[$idx]:-0} + 1))
            if [ ${mark[$idx]} = 2 ] ; then 
                # only increment first time hit twice
                nummarked+=1
            fi
        done
    done
done

echo "Squars with two or more claims: ${nummarked}"
