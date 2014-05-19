#!/bin/sh
./tangle.sh \
  src/*.org \
  geom-core/src/*.org geom-core/test/*.org \
  geom-meshops/src/*.org geom-meshops/test/*.org \
  geom-types/src/*.org geom-types/test/*.org
