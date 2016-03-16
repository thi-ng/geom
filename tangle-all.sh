#!/bin/sh

./tangle.sh README.org

readonly MODULES="core types meshops physics svg viz voxel webgl"

for m in $MODULES; do
    ./tangle-module.sh $m
done
