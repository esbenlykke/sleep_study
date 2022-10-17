#!/usr/bin/env bash

# If an argument is given then it is the name of the directory containing the
# files to split.  Otherwise, the files in the working directory are split.
if [ $# -gt 0 ]; then
  dir=$1
  epoch_length=$2
  dest=$3
else
  echo Please provide additional arguments.;
    exit;
fi

# Prompt yes/no before splitting
# read -p "Do you want to proceed? (yes/no) " yn
# 
# case $yn in 
# 	yes ) echo ok, we will proceed;;
# 	no ) echo exiting...;
# 		exit;;
# 	* ) echo invalid response;
# 		exit 1;;
# esac

# create temp dir
mkdir -p "$1/temp"

tmp="$1/temp"

# The shell glob expands to all the files in the target directory; a different
# glob pattern could be used if you want to restrict splitting to a subset,
# or if you want to include dotfiles.

for file in "$dir"/*; do
  # Details of the split command are up to you.  This one splits each file
  # into pieces named by appending a sequence number to the original file's
  # name. The original file is left in place.
  split --verbose -b 10M --numeric-suffixes "$file" "$file"
  mv -t "$tmp" "$1/"*[0-99]
done

code/testing_aggregate_screens_cwa.R "$2" "$3" "$tmp"

rm -rf "$tmp"