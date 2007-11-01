# Put the Happy-generated .hs files in the right place in the source dist.
set -e
rm -f dist/alex-*.tar.gz
rm -rf dist/alex-*/
./Setup sdist
cd dist
tar xvzf alex-*.tar.gz
cd alex-*/
mkdir dist
mkdir dist/build
mv alex dist/build
cd ..
tar cvzf alex-*.tar.gz alex-*/
