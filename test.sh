#!/bin/bash

set -u

targets=$*

res=0

for target in ${targets[@]}; do
    ros use ${target}
    ros run -- --version
    ros -e '(ql:update-all-dists :prompt nil)'
    run-prove *-test.asd

    temp=$?
    if [ ${temp} -ne 0 ]; then
        res=${temp}
    fi
done

exit ${res}
