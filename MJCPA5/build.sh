#!/bin/bash

#Script template for running and testing the project without having to type commands

clear

echo "Usage: bash build.sh <filename.java>"

PARAM="$1"
SOURCE="test/$PARAM"

stack build MJCPA5 

if [ $? == 0 ]; then 
    stack exec MJCPA5 "$SOURCE" #INESRT SOURCE FILE NAME HERE 
    
    if [ $? == 0 ]; then 
        mv "$SOURCE.s" ./ours.s
    fi

    java -jar MJ.jar "$SOURCE"
    if [ $? == 0 ]; then 
        mv "$SOURCE.s" ./ref.s
    fi

    echo "Running MJSim:"
    java -jar MJSIM.jar -b -f ref.s  > ./ref.batch.out
    java -jar MJSIM.jar -b -f ours.s > ./ours.batch.out
    
    if [ $? == 0 ]; then
        echo "Diff results:"
        diff ours.batch.out ref.batch.out
    fi
fi
#if [ $? == 0 ]; then
#    dot -Tpng "${SOURCE}.AST.dot" > "${SOURCE}.png"
    dot -Tpng "${SOURCE}.AST.dot" > "${SOURCE}.png"
#else
#    echo "Skipping generating the dot file."
#fi
