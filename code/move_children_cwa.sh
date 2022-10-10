#!/usr/bin/env bash

mkdir -p data/temp
mkdir -p /media/esbenlykke/My\ Passport/screens_cwa_children/baseline
mkdir -p /media/esbenlykke/My\ Passport/screens_cwa_children/followup


# xargs
cat data/processed/bsl_children_cwa.txt | xargs -d'\n' -I {} cp -v /media/esbenlykke/My\ Passport/screens_all_cwa_files/baseline/{} /media/esbenlykke/My\ Passport/screens_cwa_children/baseline
cat data/processed/fup_children_cwa.txt | xargs -d'\n' -I {} cp -v /media/esbenlykke/My\ Passport/screens_all_cwa_files/followup/{} /media/esbenlykke/My\ Passport/screens_cwa_children/followup

# while loop
# while read filename; do mv source/${filename} target/; done < data/processed/bsl_children_cwa.txt
# for loop
# for file in $(cat data/processed/bsl_children_cwa.txt); do mv "$file" /media/esbenlykke/My\ Passport/temp/; done
