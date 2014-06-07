#!/bin/bash

readonly PROGNAME=$(basename $0)
readonly PROGDIR=$(cd "$(dirname "$0")"; pwd)
readonly ARGS="$@"

main() {
    local modules="core types meshops webgl"

    for m in $modules
    do
        echo "deploying module $m..."
        echo "----------------------------"
        cd $PROGDIR/geom-$m/babel
        lein deploy clojars
    done
}

main
