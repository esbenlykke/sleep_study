#!/usr/bin/env bash

cd "$1" || exit

# create temp dir
rm -rf temp && mkdir temp || exit
mkdir ~/sleep_study/data/temp/

# split files to temp
for file in *; do
    if [ -f "$file" ]; then
        split --verbose -b 20M --numeric-suffixes "$file" temp/"$file"
        ~/sleep_study/code/aggregate_cwa_to_feather_screens.R "$2" "$1"
        rm -rf temp/*
    fi
done

~/sleep_study/code/merge_feathers.R "$3"

rm -rf temp