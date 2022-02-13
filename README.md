# Overview
This repo contains a tools for creating template erlang applications and
releases usnig GNU Autotools as the build system.

This git repo is inspired by:
1. **Autotools 2nd Edition** *A Practitioner's Guide To GNU Autoconf, GNU
   Automake and Libtool* **by John Calcote**
2. **[Romain Lenglet's blog](http://www.berabera.info/2009/08/02/eunit-integration-into-gnu-autotest/)**

# Installation

## Dependencies

The `neptune` tool depends upon a recent installation of Erlang OTP. Boot
strapping the build system for the `neptune` tool requires the installation of
GNU Autotools (autoconf and automake). Boot strapping the applications created
using `neptune` also requires the complete GNU Autotools, namely libtool.

## Fetch and Build

To fetch and build do the following (assumming the Erlang/OTP installation is
under /usr, not /usr/local. If not set the prefix accordingly):
```bash
git clone https://github.com/grahamcrowe77/neptune.git
cd neptune
./bootstrap.sh --prefix=/usr
./configure
make
sudo make install
```

# Using Neptune

## Erlang Applications
By default the neptune creates an Erlang application template in the current
working directory. The following creates an Erlang application named `uranus`:
```bash
neptune uranus
cd uranus
find . -type f
```

The template application doesn't do anything useful but it compiles code,
assembles documentation and includes some test examples. Note that build
system of the created Erlang application needs to be boot strapped and thus
depends on GNU Autotools being installed on your system:
```bash
./bootstrap.sh
./configure
make
make check
make install DESTDIR=$PWD/inst
make installcheck DESTDIR=$PWD/inst
make dist
make distcheck
```

The template application supports out of source tree builds:
```bash
make maintainer-clean
mkdir -pv ../build
cd ../build
../uranus/configure
make
make check
make install DESTDIR=$PWD/inst
make installcheck DESTDIR=$PWD/inst
make dist
make distcheck
```

Linux Distro Erlang installations are usually installed with prefix=/usr. By
default the configure script sets `prefix` to `/usr/local` but allows this
variable (and others) to be set accordingly:
```bash
./configure --prefix=/usr
make
sudo make install
make installcheck
```

Tests can be selected or run in verbose mode:
```bash
make check                              # All tests
make check TESTSUITEFLAGS=-v            # All tests verbose
make check TESTSUITEFLAGS="-k dialyzer" # Dialyzer test
make check TESTSUITEFLAGS="-k eunit"    # Eunit tests
make check TESTSUITEFLAGS="-k ct"       # Common tests
make check TESTSUITEFLAGS="-k blackbox" # Blackbox tests
```
