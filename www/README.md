How to update the homepage of alex
==================================

You need SFTP access to haskell.org granted by the
[haskell.org admins](https://github.com/haskell-infra/haskell-admins).
In the following, it is assumed that such access is facilitated via an
SSH key for user `alex`.

Update https://haskell.org/alex from this directory (`/www/`):
```
www$ sftp alex@webhost.haskell.org:alex
Connected to webhost.haskell.org.
Changing to: /alex

sftp> put index.html
Uploading index.html to /alex/index.html

sftp> quit
```
Instructions as of 2022-04-06, Andreas Abel.
