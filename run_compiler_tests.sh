#!/bin/bash
cargo build 
if [ $? != 0 ]; then
	echo "Failed to build the compiler"
	exit 1
fi

fail_count=0
success_count=0

for file in testcode/*.cobra; do
	name=$(basename -s .cobra ${file})
	echo "Testing ${name}"
	if ! cargo run -- build ${file} &> /dev/null; then
		echo "  Compile failed"
		fail_count=$((fail_count + 1))
	else
		build/${name}
		test_ret_value=$?
		test_expected_ret_value=$(head -n 1 $file | cut -b 6-)
		if [ "$?" -eq "$test_expected_ret_value" ]; then
			fail_count=$((fail_count + 1))
			echo "  Run failed, expected $test_ret_value, got $test_expected_ret_value"
		else
			success_count=$((success_count + 1))
			echo "  Run succeeded"
		fi
	fi
done 

echo "Tests:"
echo "  fail:    ${fail_count}"
echo "  success: ${success_count}"
exit ${fail_count}
