#!/usr/bin/env bash

# If an argument is given then it is the name of the directory containing the
# files to split.  Otherwise, the files in the working directory are split.
if [ $# -gt 0 ]; then
  dir=$1
else
  echo Please provide arguments.;
    exit;
fi

# create temp dir
mkdir -p "$1/temp"
tmp="$1/temp"
     
for file in "$dir"/*; do
  # Details of the split command are up to you.  This one splits each file
  # into pieces named by appending a sequence number to the original file's
  # name. The original file is left in place.
  split --verbose -b 10M --numeric-suffixes "$file" "$file"
  mv -t "$tmp" "$1/"*[0-99]
done