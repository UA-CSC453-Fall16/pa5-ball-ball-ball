#!/bin/bash

PARAM="$1"
SOURCE="test/$PARAM"
echo "$SOURCE"
stack exec MJCPA5 "$SOURCE" #INESRT SOURCE FILE NAME HERE 

if [ $? == 0 ]; then 
    mv "$SOURCE.s" ./ours.s
fi

java -jar MJ.jar "$SOURCE"
if [ $? == 0 ]; then 
    mv "$SOURCE.s" ./ref.s
fi

java -jar MJSIM.jar -b -f ref.s  > ./ref.batch.out
java -jar MJSIM.jar -b -f ours.s > ./ours.batch.out

if [ $? == 0 ]; then
    diff ours.batch.out ref.batch.out
fi

#if [ $? == 0 ]; then
#    dot -Tpng "${SOURCE}.AST.dot" > "${SOURCE}.png"
#    dot -Tpng "${SOURCE}.AST.dot" > "${SOURCE}.png"
#else
#    echo "Skipping generating the dot file."
#fi
