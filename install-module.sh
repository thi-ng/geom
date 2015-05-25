#!/bin/bash

readonly PROGNAME=$(basename $0)
readonly PROGDIR=$(cd "$(dirname "$0")"; pwd)
readonly ARGS="$@"
readonly PREFIX="geom-"

main() {
    for m in $ARGS
    do
        echo "installing module $PREFIX$m..."
        echo "----------------------------------------"
        cd $PROGDIR/$PREFIX$m/babel
        lein do clean, install
    done
}

main
