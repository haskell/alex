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

# Steps for doing a release:
#  * Source:
#    - do the above
#    - upload the dist to haskell.org:alex/dist/${version}
#  * Documentation:
#    - cd doc
#    - make html
#    - mv alex alex-html
#    - tar cvzf alex-doc-html-${version}.tar.gz alex-html
#    - scp alex-doc-html-${version}.tar.gz haskell.org:alex/doc
#    - ssh haskell.org
#        - cd alex/doc
#        - tar xvzf alex-doc-html-${version}.tar.gz
#        - rm -rf html-OLD
#        - mv html html-OLD && mv alex-html html
#  * Update the web page (~/darcs/www/alex/index.html), and push it
