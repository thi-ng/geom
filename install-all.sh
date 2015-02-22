#!/bin/bash

readonly PROGNAME=$(basename $0)
readonly PROGDIR=$(cd "$(dirname "$0")"; pwd)
readonly ARGS="$@"
readonly PREFIX="geom-"

main() {
    local modules="core types meshops physics svg"

    for m in $modules
    do
        echo "installing module $PREFIX$m..."
        echo "----------------------------------------"
        cd $PROGDIR/$PREFIX$m/babel
        lein do clean, cljx once, install
    done

    cd $PROGDIR/geom-webgl/babel
    lein do clean, install

    cd $PROGDIR/babel
    lein do clean, install
}

main
