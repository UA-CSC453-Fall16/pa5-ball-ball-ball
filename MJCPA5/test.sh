#!/usr/bin/bash

#Script template for running and testing the project without having to type commands

clear

echo "Usage: bash test.sh"

# don't use spaces around = ?
FILES=`ls test/Passing` # ` surrounding a command gets it's result as a string

echo "$FILES"
