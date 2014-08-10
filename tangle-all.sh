#!/bin/sh
./tangle.sh \
  src/*.org \
  geom-core/src/*.org geom-core/test/*.org \
  geom-types/src/*.org geom-types/test/*.org \
  geom-meshops/src/*.org geom-meshops/test/*.org \
  geom-webgl/src/*.org geom-webgl/test/*.org \
  geom-svg/src/*.org geom-svg/test/*.org
