Bootstrapping the build system requires the installation of GNU Autotools
(autoconf, automake and libtool).To bootstrap the build system run the
following:
```bash
git clone @GIT_URL@
cd %LC_PACKAGE_NAME%
./bootstrap.sh
```

Alternatively a distribution tarball can be fetched. These can be configured
and built without the need to install GNU Autotools:
```bash
wget @DOWNLOAD_URL@/0.1.0/%LC_PACKAGE_NAME%-%PACKAGE_VERSION%.tar.gz
tar xf %LC_PACKAGE_NAME%-%PACKAGE_VERSION%.tar.gz
cd %LC_PACKAGE_NAME%-%PACKAGE_VERSION%
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
