%define version    2.0
%define patchlevel 1
%define prefix     /usr

Vendor:       Simon Marlow
Distribution: Softies
Name:         alex
Version:      %{version}
Release:      %{patchlevel}
Copyright:    BSD w/o adv. clause
Group:        Development/Languages/Haskell
Packager:     simonmar@microsoft.com
URL:          http://www.haskell.org/alex/
Source:       http://www.haskell.org/alex/dist/%{version}/alex-%{version}-src.tar.bz2
Summary:      The lexer generator for Haskell
%description
Alex is a tool for generating lexical analysers in Haskell, given a
description of the tokens to be recognised in the form of regular
expressions.  It is similar to the tool <quote>lex</quote> or
<quote>flex</quote> for C/C++.
%prep
%setup -n alex-%{version}

%build
autoconf
./configure --prefix=%{prefix}
make
( cd alex/doc ; make alex.{dvi,ps,html} ; gzip -9 *.dvi *.ps )

%install
make install

%files
%doc alex/ANNOUNCE
%doc alex/LICENSE
%doc alex/README
%doc alex/TODO
%doc alex/doc/alex
%doc alex/doc/alex.dvi.gz
%doc alex/doc/alex.ps.gz
%doc alex/examples
%{prefix}/bin/alex
%{prefix}/bin/alex-%{version}
%{prefix}/lib/alex-%{version}
