#!/bin/sh

SMPL="./smplc "

files="test/*.smpl"

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
		CleanUp
		exit 1
	fi
done
CleanUp
echo "All tests passed.\n"
