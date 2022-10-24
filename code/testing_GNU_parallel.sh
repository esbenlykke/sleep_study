#!/usr/bin/env bash

cd /media/esbenlykke/My Passport/screens_cwa_children/baseline_test

parallel --dry-run -N10 code/split_files.sh output.{#} {} ::: {1..150}.dat

# seq 100000 | parallel --block 15k --pipe wc -l
