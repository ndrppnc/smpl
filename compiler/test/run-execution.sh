#!/bin/sh

SMPL="../smplc -c "

files="tests/execution_cases/*.smpl"

CleanUp() {
	rm test.out test.null test.c result.out a.out
}

GetError() {
	eval "$SMPL $1" 2> test.out 1> test.null
	wc -l test.out | awk '{print $1}'
}

GenerateCode() {
    $SMPL $1 > test.c
    gcc -pthread test.c
    ./a.out > result.out
    BASEFILE=`echo $1 | sed -e "s/smpl/out/g"`
    diff -b "$BASEFILE" result.out 2>&1 || {
	echo "FAILED $BASEFILE differs for $1" 1>&2
        echo $1 " failed to pass."
        exit 1
    }
}

for file in $files
do
	errors=$(GetError $file)
	GetError $file
	if [ $errors -eq 0 ]
	then 
            GenerateCode $file
	    echo "Test: " $file " passed."
	else
	    echo $file " failed to pass."
	    exit 1
	fi
done
CleanUp
echo "All tests passed.\n"
