#!/usr/bin/env bash

cd "$1" || exit

# create temp dir
rm -rf temp && mkdir temp || exit
mkdir -p ~/sleep_study/data/temp/

# split files to temp
for file in *; do
    if [ -f "$file" ]; then
        split --verbose -b 25M --numeric-suffixes "$file" temp/"$file"
        ~/sleep_study/code/resample_and_extract_features_screens.R "$2" "$1"
        rm -rf temp/*
    fi
done

~/sleep_study/code/merge_feathers.R "$3"