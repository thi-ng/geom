#!/bin/sh

DIR=`pwd`
FILES=""
SRC="geom-$1/src/*.org geom-$1/test/*.org geom-$1/bench/*.org"

rm -rf geom-$1/babel/src geom-$1/babel/test

# wrap each argument in the code required to call tangle on it
for i in `ls $SRC`; do
    FILES="$FILES \"$i\""
done

emacs -Q --batch \
    --eval \
    "(progn
     (require 'org)(require 'ob)(require 'ob-tangle)(require 'ob-lob)
     (org-babel-lob-ingest \"src/library-of-babel.org\")
     (org-babel-lob-ingest \"src/config.org\")
     (setq org-confirm-babel-evaluate nil)
     (mapc (lambda (file)
            (find-file (expand-file-name file \"$DIR\"))
            (org-babel-tangle)
            (kill-buffer)) '($FILES)))" \
#2>&1 | grep Tangled
