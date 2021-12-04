This repo contains a trivial Erlang project using Autotools and its built in
portability features.

This git repo is inspired by:
**Autotools 2nd Edition**
*A Practinioner's Guide To GNU Autoconf, GNU Automake and Libtool*
**by John Calcote**

To bootstrap the build system run the following:
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
```
