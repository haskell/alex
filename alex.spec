%define name    alex
%define version 2.0
%define release 1

Name:           %{name}
Version:        %{version}
Release:        %{release}
License:        BSD-like
Group:          Development/Languages/Haskell
URL:            http://www.haskell.org/alex/
Source:         http://www.haskell.org/alex/dist/%{version}/alex-%{version}-src.tar.gz
Packager:       Simon Marlow <simonmar@microsoft.com>
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
Prefix:         %{_prefix}
Summary:        The lexer generator for Haskell

%description
Alex is a tool for generating lexical analysers in Haskell, given a
description of the tokens to be recognised in the form of regular
expressions.  It is similar to the tool <quote>lex</quote> or
<quote>flex</quote> for C/C++.

%prep
%setup -n alex-%{version}

%build
test -f configure || autoreconf
./configure --prefix=%{prefix}
make
( cd alex/doc ; make dvi ps html ; gzip -f -9 *.dvi *.ps )

%install
make prefix=${RPM_BUILD_ROOT}%{prefix} install

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)
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
