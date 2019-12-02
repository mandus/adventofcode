#!/bin/bash
# AoC 2019 - Åsmund Ødegård

declare -i pos=0
declare -i op left right savepos

IFS=$',' read -d '' -r -a data < $1
values=("${data[@]}") 

noun=12
verb=2

values[1]=$noun
values[2]=$verb


# part 1

while : ; do 
    op=${values[pos]}
    left=${values[${values[pos+1]}]}
    right=${values[${values[pos+2]}]}
    savepos=${values[pos+3]}
    if [[ ${op} = 99 ]] ; then
        break
    fi
    if [[ ${op} = 1 ]] ; then
        values[$savepos]=$(($left+$right))
    fi
    if [[ ${op} = 2 ]]; then
        values[$savepos]=$(($left*$right))
    fi
    pos=pos+4
done
echo ${values[0]}


# part 2

declare -i goal=19690720

for i in {0..99}; do
    for j in {0..99}; do 
        values=("${data[@]}")
        values[1]=$i
        values[2]=$j
        pos=0
        while : ; do 
            op=${values[pos]}
            left=${values[${values[pos+1]}]}
            right=${values[${values[pos+2]}]}
            savepos=${values[pos+3]}
            if [[ ${op} = 99 ]] ; then
                break
            fi
            if [[ ${op} = 1 ]] ; then
                values[$savepos]=$(($left+$right))
            fi
            if [[ ${op} = 2 ]]; then
                values[$savepos]=$(($left*$right))
            fi
            if [[ ${values[0]} -gt ${goal} ]] ; then
                echo "************** to big for goal: ${values[0]}"
                break
            fi
            pos=pos+4
        done
        if [[ ${values[0]} == ${goal} ]] ; then
            echo $((100*i+j));
            exit 0;
        fi
    done
done
