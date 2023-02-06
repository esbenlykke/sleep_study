#!/usr/bin/env bash

cd "$1" || exit

for f in *; do mv "$f" "${f// /_}"; done
