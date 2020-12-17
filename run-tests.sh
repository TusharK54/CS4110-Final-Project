#!/usr/bin/env bash

# check that the executable interpreter is built
if [[ ! -f "./crispy" ]]; then
    printf "ERROR: ./crispy executable not found"
    exit 1
fi

# run tests and print how many of them pass
tests_passed=0
tests_total=0

for f in tests/*.crspy tests/**/*.crspy; do
    # run test file $f w/o showing output or error
    printf "Running test %s ... " $f
    ./crispy $f > /dev/null 2>&1

    # print result and increment counters
    if [[ $? != 0 ]]; then
        printf "FAIL\n"
    else
        printf "PASS\n"
        (( tests_passed+=1 ))
    fi
    (( tests_total+=1 ))

done

printf "\n%d/%d tests passed" $tests_passed $tests_total