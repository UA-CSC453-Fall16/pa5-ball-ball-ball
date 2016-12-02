#!/usr/bin/bash

#Script template for running and testing the project without having to type commands

clear

echo ""
echo ""
echo "Usage: bash run.sh <filename.java>"

PARAM="$1"
SOURCE="test/$PARAM"

stack build MJCPA5 

#echo "Producing .s output - $SOURCE$PARAM"

stack exec MJCPA5 "$SOURCE" #INESRT SOURCE FILE NAME HERE 

#echo "producing AST"

dot -Tpng "${SOURCE}.AST.dot" > "${SOURCE}.png"
