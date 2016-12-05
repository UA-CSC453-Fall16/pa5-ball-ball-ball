#!/bin/bash

function cmp() {
    PARAM="$1"
    SOURCE="test/$PARAM"

    output=`stack exec MJCPA5 "$SOURCE"` # can possibly use suppress

    if [ $? == 0 ]; then 
        mv "$SOURCE.s" ./ours.s
        ours=0
    else
        ours=1
    fi

    outpus=`java -jar MJ.jar "$SOURCE"`
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

clear

echo "Usage: bash test.sh"

stack build MJCPA5 

if [ $? == 0 ]; then
    # don't use spaces around '=' ?
    rm test/Passing/*.dot
    rm test/Passing/*.png
    FILES=$(ls test/Passing) # `` surrounding a command gets its result as a string

    res=""
    for x in $FILES
    do
        cmp "Passing/$x"
        if [$res == ""]; then
            echo "test $x passed"
        else
            echo "--------------"
            echo "TEST $x FAILED"
            echo "--------------"
        fi
    done
    rm test/Passing/*.dot
fi
