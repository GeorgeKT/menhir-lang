#!/bin/bash 
if [ "$1" == "release" ]; then
	mode="--release"
else
	mode=""
fi

cargo build ${mode} 
if [ $? != 0 ]; then
	echo "Failed to build the compiler"
	exit 1
fi

triplet=$(cargo run ${mode} -- info --triplet 2> /dev/null)
if [ -z "${triplet}" ]; then
    echo "Failed to determine the target triplet"
    exit 1
fi
echo "Current target triplet: ${triplet}"
fail_count=0
success_count=0
fail_list=()

for file in testcode/*.mhr; do
	name=$(basename -s .mhr ${file})
	echo "Testing ${name}"
	if ! cargo run ${mode} -- build ${file} &> /tmp/compile_output.log; then
		echo "*********************"
		echo "  Compile failed"
		cat /tmp/compile_output.log
		echo "---------------------"
		fail_count=$((fail_count + 1))
        fail_list+=( "$name" )
	else
		build/${triplet}/${name}/${name}
		test_ret_value=$?
		test_expected_ret_value=$(head -n 1 $file | cut -b 6-)
		if [ "$test_ret_value" -ne "$test_expected_ret_value" ]; then
			fail_count=$((fail_count + 1))
            fail_list+=( $name )
			echo "  Run failed, expected $test_expected_ret_value, got $test_ret_value"
		else
			success_count=$((success_count + 1))
			echo "  Run succeeded"
		fi
	fi
done 

echo "Tests:"
echo "  fail:    ${fail_count} (failed: ${fail_list[@]})"
echo "  success: ${success_count}"
exit ${fail_count}
