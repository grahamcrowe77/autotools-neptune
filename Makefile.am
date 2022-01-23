ACLOCAL_AMFLAGS = -I m4

MODULES = @MODULES@

ESCRIPTS = neptune

SUBDIRS = . doc tests

# --------------------------------------
# Directory Structure
# --------------------------------------
neptune_srcdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/src
neptune_ebindir=$(ERLANG_INSTALL_LIB_DIR_neptune)/ebin
neptune_incdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/include
neptune_privdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/priv

# --------------------------------------
# Escripts
# --------------------------------------
dist_bin_SCRIPTS = $(addprefix $(builddir)/bin/, $(ESCRIPTS))

$(builddir)/bin/neptune: $(srcdir)/bin/neptune.in
	$(MKDIR_P) $(@D)
	$(AM_V_GEN)$(SED) \
	  -e "s#%NEPTUNE_EBINDIR%#$(DESTDIR)$(neptune_ebindir)#g" \
	  -e "s#%PACKAGE_VERSION%#$(PACKAGE_VERSION)#g" $< > $@
	chmod +x $@

# --------------------------------------
# NIF
# --------------------------------------
neptune_priv_LTLIBRARIES = libneptune_nif.la
libneptune_nif_la_CFLAGS = -I@ERLANG_ROOT_DIR@/usr/include

libneptune_nif_la_SOURCES = \
  $(srcdir)/c_src/neptune.c

if APPLE
neptune_priv_DATA = \
  .libs/libneptune_nif.so

.libs/libneptune_nif.so: .libs/libneptune_nif.dylib
	(cd $(@D) && $(LN_S) $(<F) $(@F))
endif #APPLE

# --------------------------------------
# Public Erlang Header Files
# --------------------------------------
dist_neptune_inc_DATA = \
  $(srcdir)/include/neptune.hrl

# --------------------------------------
# Build beam files
# --------------------------------------
AM_V_ERLC = $(am__v_ERLC_@AM_V@)
am__v_ERLC_ = $(am__v_ERLC_@AM_DEFAULT_V@)
am__v_ERLC_0 = @echo "  ERLC    " $@;
am__v_ERLC_1 = 

SOURCES = $(addprefix $(srcdir)/src/, $(addsuffix .erl, $(MODULES)))
BEAMS   = $(addprefix $(builddir)/ebin/, $(addsuffix .beam, $(MODULES)))

dist_neptune_src_DATA = \
  $(neptune_priv_headers) \
  $(SOURCES)

neptune_priv_headers = \
  $(srcdir)/src/neptune_limits.hrl

neptune_ebin_DATA = \
  $(builddir)/ebin/neptune.app \
  $(BEAMS)

$(builddir)/ebin/%.beam: $(srcdir)/src/%.erl
	$(AM_V_ERLC)$(ERLC) $(ERLCFLAGS) \
           -I$(srcdir)/src \
           -I$(srcdir)/include \
           -o $(@D) $<

# --------------------------------------
# Clean
# --------------------------------------
MOSTLYCLEANFILES = \
  $(BEAMS) \
  $(builddir)/bin/neptune \
  $(builddir)/ebin/neptune.app

if APPLE
MOSTLYCLEANFILES += .libs/libneptune_nif.so
endif #APPLE

# --------------------------------------
# Distribution
# --------------------------------------
EXTRA_DIST = \
  $(srcdir)/bootstrap.sh \
  $(builddir)/bin/neptune.in
