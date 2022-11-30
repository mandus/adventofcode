#!/bin/bash
# Advent of Code 2018 - Åsmund Ødegård

IFS=$'\n' read -d '' -r -a values < $1

declare -A mark
declare -A claimmap
declare -A claimcells
declare -i left top width height leftoffset topoffset
declare -i nummarked=0 

for line in "${values[@]}" ; do
    echo $line
    read -r claimid left top width height <<< $(sed 's/\(.*\) \(.*\),\(.*\): \(.*\)x\(.*\)/\1 \2 \3 \4 \5/' <<< $(awk '{print $1 " " $3 " " $4}' <<< $line))

    # map all by left top corner
    claimidx="${left} ${top}"
    claimmap[$claimidx]=$claimid
    claimcells[$claimidx]="$width $height"

    for ((i=0; i<width; i++)) ; do
        for ((j=0; j<height; j++)) ; do
            leftoffset=left+i
            topoffset=top+j
            idx="${leftoffset} ${topoffset}"
            mark[$idx]=$((${mark[$idx]:-0} + 1))
            if [ ${mark[$idx]} = 2 ] ; then 
                # only increment first time hit twice
                nummarked+=1
            fi
        done
    done
done

echo "Squares with two or more claims: ${nummarked}"

declare -i breakit=0
for key in "${!claimmap[@]}" ; do
    # Find the one key both in the marks with just '1' in all its cells
    breakit=0
    if [[ ${mark[$key]} = 1 ]] ; then
        # candidate - check all cells
        read -r left top <<< $key
        read -r width height <<< ${claimcells[$key]}
        for ((i=0; i<width; i++)) ; do
            for ((j=0; j<height; j++)) ; do
                leftoffset=left+i
                topoffset=top+j
                idx="${leftoffset} ${topoffset}"
                if [ ${mark[$idx]:-0} -ne 1 ] ; then
                    breakit=1
                    break
                fi
            done
            if [ $breakit = 1 ] ; then
                break
            fi
        done
        if [ $breakit = 1 ] ; then
            continue
        fi
        echo "$key: ${claimmap[$key]} does not overlap with any other claim"
    fi
done
