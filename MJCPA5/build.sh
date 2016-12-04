#!/usr/bin/bash

#Script template for running and testing the project without having to type commands

clear

echo "Usage: bash build.sh <filename.java>"

PARAM="$1"
SOURCE="test/$PARAM"

stack build MJCPA5 

if [ $? == 0 ]; then 
    stack exec MJCPA5 "$SOURCE" #INESRT SOURCE FILE NAME HERE 
    mv "$SOURCE.s" ./ours.s


    java -jar MJ.jar "$SOURCE"
    mv "$SOURCE.s" ./ref.s

    java -jar MJSIM.jar -b -f ours.s > ./ours.batch.out
    java -jar MJSIM.jar -b -f ref.s  > ./ref.batch.out

fi
#if [ $? == 0 ]; then
#    dot -Tpng "${SOURCE}.AST.dot" > "${SOURCE}.png"
#else
#    echo "Skipping generating the dot file."
#fi
