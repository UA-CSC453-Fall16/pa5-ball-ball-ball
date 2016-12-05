#!/bin/bash

function cmp() {
    SOURCE="$1"

    stack exec MJCPA5 "$SOURCE"  >/dev/null 2>/dev/null # can possibly use suppress

    if [ $? == 0 ]; then 
        mv "$SOURCE.s" ./ours.s
        ours=0
    else
        ours=1
    fi

    java -jar MJ.jar "$SOURCE"  >/dev/null 2>/dev/null
    if [ $? == 0 ]; then 
        mv "$SOURCE.s" ./ref.s
        theirs=0
    else
        theirs=1
    fi

    if [ $theirs -o $ours ]; then
        if [ $theirs -ne $ours ]; then
            echo ""
            res="different"
        else
            res=""
        fi
    else
        java -jar MJSIM.jar -b -f ref.s  > ./ref.batch.out
        java -jar MJSIM.jar -b -f ours.s > ./ours.batch.out
        res=`diff ours.batch.out ref.batch.out`
    fi
}

FOLDER="test/$1"

clear
echo "Usage: bash test.sh <FOLDER>"
stack build MJCPA5 

if [ $? == 0 ]; then
    # don't use spaces around '=' ?
    rm "$FOLDER"/*.dot 2> /dev/null
    rm "$FOLDER"/*.png 2> /dev/null
    rm "$FOLDER"/*.s 2> /dev/null
    FILES=$(ls $FOLDER) # `` surrounding a command gets its result as a string

    res=""
    for x in $FILES
    do
        cmp "$FOLDER/$x"
        if [$res == ""]; then
            echo -e "$x\tpassed"
        else
            echo "--------------"
            echo -e "$x\tFAILED"
            echo "--------------"
        fi
    done
    rm "$FOLDER"/*.dot 2> /dev/null
    rm "$FOLDER"/*.s 2> /dev/null
fi
