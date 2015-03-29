#!/bin/sh

readonly MODULES="core types meshops physics svg voxel webgl"
FILES="src/index.org"

for m in $MODULES; do
    MODULE="geom-$m"
    SRC="$MODULE/src/*.org $MODULE/test/*.org $MODULE/bench/*.org"
    for f in `ls $SRC`; do
        FILES="$FILES $f"
    done
done

#echo $FILES
./tangle.sh $FILES
