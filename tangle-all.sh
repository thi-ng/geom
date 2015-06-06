#!/bin/sh

readonly MODULES="core types meshops physics svg viz voxel webgl"
FILES="src/index.org"

for m in $MODULES; do
    MODULE="geom-$m"
    SRC="$MODULE/src/*.org $MODULE/test/*.org $MODULE/bench/*.org"
    rm -rf $MODULE/babel/src $MODULE/babel/test
    for f in `ls $SRC`; do
        FILES="$FILES $f"
    done
done

#echo $FILES
./tangle.sh $FILES
