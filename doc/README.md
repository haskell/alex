# Building the Alex documentation

In this directory, run:
```
autoconf
./configure
make
```
On success, you should find the documentation in the
subdirectory `alex/`.

## Troubleshooting

Running `./configure` might report:
```
checking for DocBook XSL stylesheet directory... no
configure: WARNING: cannot find DocBook XSL stylesheets, you will not be able to build the documentation
```
Extending the list `FP_DIR_DOCBOOK_XSL` in file `configure.ac` might
help.  E.g., on Mac OS X with homebrew-installed `docbook`, the path
```
/usr/local/Cellar/docbook-xsl/*/docbook-xsl
```
worked.  Inside this directory (pattern), `configure` looks for a file
`/html/docbook.xsl`, see `aclocal.m4`.
