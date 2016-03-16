#!/bin/sh

FILES=""
readonly SRC="src/$1/*.org test/$1/*.org bench/$1/*.org examples/$1/*.org"

rm -rf babel/src/thi/ng/geom/$1 babel/test/thi/ng/geom/test/$1 babel/examples/$1

for f in `ls $SRC`; do
    FILES="$FILES $f"
done

./tangle.sh $FILES
