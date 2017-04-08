#!/bin/bash
ret=0

cargo build 
if [ $? != 0 ]; then
	echo "Failed to build the compiler"
	exit 1
fi

for file in testcode/*.cobra; do
	name=$(basename -s .cobra ${file})
	echo "Testing ${name}"
	if ! cargo run -- build ${file} &> /dev/null; then
		ret=1
		echo "  Compile failed"
	else
		build/${name}
		test_ret_value=$?
		test_expected_ret_value=$(head -n 1 $file | cut -b 6-)
		if [ "$?" -eq "$test_expected_ret_value" ]; then
			ret=1
			echo "  Run failed, expected $test_ret_value, got $test_expected_ret_value"
		else
			echo "  Run succeeded"
		fi
	fi
done 


exit $ret
