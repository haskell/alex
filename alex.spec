%define name    alex
%define version 2.2
%define release 1

Name:           %{name}
Version:        %{version}
Release:        %{release}
License:        BSD-like
Group:          Development/Languages/Haskell
URL:            http://haskell.org/alex/
Source:         http://haskell.org/alex/dist/%{version}/alex-%{version}.tar.gz
Packager:       Sven Panne <sven.panne@aedion.de>
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
Prefix:         %{_prefix}
BuildRequires:  happy, ghc, docbook-dtd, docbook-xsl-stylesheets, libxslt, libxml2, fop, xmltex, dvips
Summary:        The lexer generator for Haskell

%description
Alex is a tool for generating lexical analysers in Haskell, given a
description of the tokens to be recognised in the form of regular
expressions.  It is similar to the tool lex or flex for C/C++.

%prep
%setup -n alex-%{version}

%build
runhaskell Setup.lhs configure --prefix=%{_prefix} --docdir=%{_datadir}/doc/packages/%{name}
runhaskell Setup.lhs build
cd doc
test -f configure || autoreconf
./configure
make html

%install
runhaskell Setup.lhs copy --destdir=${RPM_BUILD_ROOT}

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)
%doc ANNOUNCE
%doc LICENSE
%doc README
%doc TODO
%doc doc/alex
%doc examples
%{prefix}/bin/alex
%{prefix}/share/alex-%{version}
