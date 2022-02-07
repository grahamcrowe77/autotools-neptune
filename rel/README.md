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
wip make installcheck DESTDIR=$PWD/inst
make maintainer-clean
```

Temporary test on staged installation:
```
make install DESTDIR=$PWD/inst
ERL_ROOTDIR=$PWD/inst/usr/local/bin/%LC_REL_NAME% inst/usr/local/bin/%LC_REL_NAME%-rel.sh
pgrep erl_child_setup | xargs kill -9
```

Linux Distro Erlang installations are usually installed with prefix=/usr. To
ensure the correct installation run configure as follows:
```bash
./configure --prefix=/usr
make
sudo make install
```
