echo -e "** $2 - `date +%Y-%m-%d`\n" > CHANGELOG.$$
git log $1...$2 --pretty=format:'| [[https://github.com/thi-ng/geom/commit/%H][%h]] | %s |' >> CHANGELOG.$$
echo -e "\n" >> CHANGELOG.$$
cat CHANGELOG.org >> CHANGELOG.$$
mv CHANGELOG.$$ CHANGELOG.org
