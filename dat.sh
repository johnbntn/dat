#! /bin/bash

if [ $# -gt 1 ]; then
    echo "Only the path, please"
elif [ $# -eq 0 ]; then
    echo "Needs a filename"
else
    bap ./$1 --pass=gen-info
fi