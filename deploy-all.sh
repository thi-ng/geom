#!/bin/bash

readonly PROGNAME=$(basename $0)
readonly PROGDIR=$(cd "$(dirname "$0")"; pwd)
readonly ARGS="$@"
readonly PREFIX="geom-"

main() {
    local modules="core types meshops physics webgl svg voxel"

    for m in $modules
    do
        echo "deploying module $PREFIX$m..."
        echo "----------------------------------------"
        cd $PROGDIR/$PREFIX$m/babel
        lein deploy clojars
    done

    cd $PROGDIR/babel
    lein deploy clojars
}

main
