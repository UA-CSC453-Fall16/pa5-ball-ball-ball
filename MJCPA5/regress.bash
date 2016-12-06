#!/bin/bash

# The shell script that compares a MeggyJava reference compiler that generates
#  .s files run through the Meggy Simulator (MJSIM.jar) against a Java-only 
# version of the Meggy run-time library. 

# The regress.sh script also compares student's compiler that generates 
# .s files run through the Meggy Simulator (MJSIM.jar) against a Java-only 
# version of the Meggy run-time library.

# The Java only version and MJSIM print out text messages about what is 
# happening in the program so that diffs can be performed.

# This script assumes that MJSIM.jar, MJ.jar, and student's compiler are 
# in the same directory as regress.bash is being run. 
# Additionally, there needs to be a meggy/ subdirectory with the Java-only 
# meggy package.
#
# This script will by default do the comparison for all .java files in the 
# WorkingTestCases/ and WorkingErrorTestCases/ subdirectories.
# However, if one specific file is given as input, then the test will only
# be done on that file.

# usage examples: 
#   ./regress.bash
#   ./regress.bash file.java

# Note that test cases in .java files can have corresponding .arg_opt
# files.  Such files indicate button presses and other user interaction
# with the device.  Both the meggy simulator and the Java-only Meggy
# library use these files when they exist.

# After you run this script, there will be a lot of .class, .dot, and .s
# files in this directory.  They can be safely deleted. 

# This function actually does take a parameter, the input file name.
function compareProgram
{
    echo "========================================================="
    echo "Regression testing using $filename file"
    echo "MJ.jar vs. Java-Only and MJCPA5Stack vs. Java-Only"

    # Copy the source file to this directory.
    # This is so that everything works with the meggy/ Java package.
    filenameForProcess=`basename $1 .java`

    cp $1 .

    # compile the input file with our compiler
    rm -rf $filenameForProcess.java.s
    rm -rf t1
    ./MJ.jar $filenameForProcess.java >& t1

    # check if there is an arg_opts file to go with it
    # and copy that into meggy/arg_opts
    if test -f $1.arg_opts 
    then
        cp -f $1.arg_opts meggy/arg_opts
    fi
   
    # run the simulator on the generated .s file
    size=`wc -c < $filenameForProcess.java.s`
    if  [ "$size" -gt 0 ];
    then
        java -jar MJSIM.jar -b -f $filenameForProcess.java.s &> t1
    fi

    # compile the input file with our compiler
    rm -rf $filenameForProcess
    rm -rf t3
    ./MJCPA5 $filenameForProcess.java >& t3

    # run the simulator on the generated .s file
    size=`wc -c < $filenameForProcess.java.s`
    if  [ "$size" -gt 0 ];
    then
        java -jar MJSIM.jar -b -f $filenameForProcess.java.s &> t3
    fi

    # remove any previous versions of the file
    rm *.class t2  > /dev/null 2>&1
    # Compile the java only version of the program.
    javac $filenameForProcess.java >& t2
    if [ $? -eq 0 ] 
    then
        # run java on Java bytecode
        java $filenameForProcess &> t2
    fi

    echo "diff between MJ.jar with MJSIM.jar output and Java only output"
    diff t1 t2
    
    echo "diff between MJCPA5Stack with MJSIM.jar output and Java only output"
    diff t3 t2

    echo "DONE with MJ.jar, MJCPA5Stack and Java Only for $filenameForProcess"
    echo "========================================================="
}

##############################################################
# Main routine for the regression testing.
# Will either do the comparison on a single file or
# on all the files in certain subdirectories.

# Check if we got a file name on the command line.
if [ $# -gt 0 ]
then
    # Test the single file provided
    compareProgram $1
    echo $1
else
    echo
    echo "#### Testing with the files in WorkingTestCases ####"
    for filename in `ls WorkingTestCases/*.java`
    do
        compareProgram $filename
    done

    echo
    echo "#### Testing with the files in WorkingErrorTestCases ####"
    for filename in `ls WorkingErrorTestCases/*.java`
    do
        compareProgram $filename
    done

fi
    rm *java* t* *.class   
echo


