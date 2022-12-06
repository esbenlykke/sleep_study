#!/usr/bin/env bash

cd "$1" || exit

# create temp dir
rm -rf temp && mkdir temp || exit

# split files to temp
for file in *; do
    if [ -f "$file" ]; then
        split --verbose -b 100M --numeric-suffixes "$file" temp/"$file"
    fi
done

cd /home/esbenlykke/sleep_study

# process files in temp
code/aggregate_cwa_to_feather_screens.R "$2" "$3" "$1/temp" "$4"

rm -rf "$1/temp"