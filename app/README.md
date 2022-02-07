Bootstrapping the build system requires the installation of GNU Autotools
(autoconf, automake and libtool). To bootstrap the build system run the
following:
```bash
./bootstrap.sh
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
make mostlyclean
make
sudo make install
make installcheck
make maintainer-clean
```

To test building out of source tree:
```
mkdir -pv ../build
cd ../build
./configure ../%LC_APP_NAME%/configure
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
