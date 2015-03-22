echo -e "** $2\n" > CHANGELOG.$$
git log $1...$2 --pretty=format:'- [[https://github.com/thi-ng/geom/commit/%H][%h]]: %s' --reverse >> CHANGELOG.$$
echo -e "\n" >> CHANGELOG.$$
cat CHANGELOG.org >> CHANGELOG.$$
mv CHANGELOG.$$ CHANGELOG.org
