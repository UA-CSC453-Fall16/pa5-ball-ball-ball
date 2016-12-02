#!/usr/bin/bash

#Script template for running and testing the project without having to type commands

clear

echo ""
echo ""

PARAM="$2"
DIRPARAM="$1"
DIR="test/sources/$DIRPARAM/"

echo "Producing .s output - $DIR$PARAM"

java -jar test/MJ.jar "$DIR$PARAM" #INESRT SOURCE FILE NAME HERE 

