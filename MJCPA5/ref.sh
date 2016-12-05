#!/bin/bash

#Script template for running and testing the project without having to type commands

clear

echo ""
echo ""
echo "Usage: bash run.sh <filename.java>"

PARAM="$1"
SOURCE="test/$PARAM"

java -jar MJ.jar $SOURCE #INESRT SOURCE FILE NAME HERE 

#echo "producing AST"
if [ $? == 0 ]; then
    dot -Tpng "${SOURCE}.ast.dot" > "${SOURCE}.ref.png"
else
    echo "Skipping generating the dot file."
fi
