#!/bin/sh

SMPL="../smplc -s "

files="tests/semantic_pass_cases/*.smpl"

CleanUp() {
	rm test.out test.null
}

GetError() {
	eval "$SMPL $1" 2> test.out 1> test.null
	wc -l test.out | awk '{print $1}'
}

for file in $files
do
	errors=$(GetError $file)
	GetError $file
	if [ $errors -eq 0 ]
	then 
		echo "Test: " $file " passed."
	else
		echo $file " failed to pass."
		exit 1
	fi
done
CleanUp
echo "All tests passed.\n"
