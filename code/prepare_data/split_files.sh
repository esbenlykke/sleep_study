#!/usr/bin/env bash

# $1 path to children cwa files
# $2 destination file

cd "$1" || exit

# create temp dir
rm -rf temp && mkdir temp || exit

echo "Execute the whole thing in parallel. ETA ~ 4-5 hrs"

# split files to temp
for file in *; do
    if [ -f "$file" ]; then
        split --verbose -b 25M --numeric-suffixes "$file" temp/"$file"
        ~/projects/sleep_study/code/prepare_data/resample_and_extract_features_screens.R "$1" #"$2"
        # rm -rf temp/*
    fi
done

~/projects/sleep_study/code/merge_feathers.R "$2"