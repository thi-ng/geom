git log $1...$2 --pretty=format:'* [[https://github.com/thi-ng/geom/commit/%H][view commit]]: %s' --reverse | grep "#changelog"
