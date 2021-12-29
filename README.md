This repo contains a trivial Erlang project using Autotools and its built in
portability features.

This git repo is inspired by:
1. **Autotools 2nd Edition** *A Practitioner's Guide To GNU Autoconf, GNU
   Automake and Libtool* **by John Calcote**
2. **[Romain Lenglet's blog](http://www.berabera.info/2009/08/02/eunit-integration-into-gnu-autotest/)**

Bootstrapping the build system requires the installation of GNU Autotools
(autoconf, automake and libtool).To bootstrap the build system run the
following:
```bash
git clone https://github.com/grahamcrowe77/autotools-neptune.git
cd autotools-neptune
./bootstrap.sh
```

Alternatively a distribution tarball can be fetched. These can be configured
and built without the need to install GNU Autotools:
```bash
wget https://github.com/grahamcrowe77/autotools-neptune/releases/download/0.1.0/neptune-0.1.0.tar.gz
tar xf neptune-0.1.0.tar.gz
cd neptune-0.1.0
```

To test building in the source tree:
```bash
./configure
make
make check
make install DESTDIR=$PWD/inst
make installcheck DESTDIR=$PWD/inst
make dist
make distcheck
make maintainer-clean
```

To test building out of source tree:
```
mkdir -pv ../build
cd ../build
./configure ../neptune/configure
make
make distcheck
```

Linux Distro Erlang installations are usually installed with prefix=/usr. To
ensure the correct installation run configure as follows:
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
