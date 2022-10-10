#!/usr/bin/env bash

mkdir -p data/temp

 split -b 10000000 data/40841_0002627305.cwa --filter 'gzip -f > data/temp/$FILE.gz'

# rm -rf data/temp