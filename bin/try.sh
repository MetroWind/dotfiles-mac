#!/bin/zsh

Count=-1
if [[ $1 =~ ^[0-9]+$ ]]; then
    Count=$1
    shift    
fi
while [[ ${Count} -ne 0 ]]; do
    if [[ ${Count} -gt 0 ]]; then
        (( Count -= 1 ))
    fi
    $*
    RtnVal=$?
    if [[ ${RtnVal} -eq 0 ]]; then
        exit 0
    fi
done
exit ${RtnVal}
