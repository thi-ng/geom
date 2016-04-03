#!/bin/sh

./tangle.sh README.org examples/all.org

readonly MODULES="core mesh physics svg types utils viz voxel gl"

for m in $MODULES; do
    ./tangle-module.sh $m
done
