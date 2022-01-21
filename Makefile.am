ACLOCAL_AMFLAGS = -I m4

SUBDIRS = . doc tests

######################################################################
# Silent builds
######################################################################

AM_V_ERLC = $(am__v_ERLC_@AM_V@)
am__v_ERLC_ = $(am__v_ERLC_@AM_DEFAULT_V@)
am__v_ERLC_0 = @echo "  ERLC    " $@;
am__v_ERLC_1 = 

######################################################################

######################################################################
# Application Structure
######################################################################

neptune_srcdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/src
neptune_ebindir=$(ERLANG_INSTALL_LIB_DIR_neptune)/ebin
neptune_incdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/include
neptune_privdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/priv

######################################################################

######################################################################
# Application Escripts
######################################################################

dist_bin_SCRIPTS = \
  $(top_builddir)/bin/neptune

$(top_builddir)/bin/neptune: $(top_srcdir)/bin/neptune.in
	$(MKDIR_P) $(@D)
	sed -e s#NEPTUNE_EBINDIR#$(DESTDIR)$(neptune_ebindir)#g $< > $@
	chmod +x $@

######################################################################

######################################################################
# Application NIF
######################################################################

neptune_priv_LTLIBRARIES = libneptune_nif.la
libneptune_nif_la_CFLAGS = -I@ERLANG_ROOT_DIR@/usr/include

libneptune_nif_la_SOURCES = \
  $(top_srcdir)/c_src/neptune.c

######################################################################

######################################################################
# Application Public Erlang Header Files
######################################################################

dist_neptune_inc_DATA = \
  $(top_srcdir)/include/neptune.hrl

######################################################################

######################################################################
# Manual Pages for Application Escripts
######################################################################

dist_man1_MANS = \
  $(top_srcdir)/man1/neptune.1

######################################################################

######################################################################
# Application Build
######################################################################

SOURCES = $(addprefix $(top_srcdir)/src/, $(addsuffix .erl, @MODULES@))
BEAMS   = $(addprefix $(top_builddir)/ebin/, $(addsuffix .beam, @MODULES@))

dist_neptune_src_DATA = \
  $(neptune_priv_headers) \
  $(SOURCES)

neptune_priv_headers = \
  $(top_srcdir)/src/neptune_limits.hrl

neptune_ebin_DATA = \
  $(top_builddir)/ebin/neptune.app \
  $(BEAMS)

$(top_builddir)/ebin/%.beam: $(top_srcdir)/src/%.erl
	$(AM_V_ERLC)$(ERLC) $(ERLCFLAGS) \
           -I$(top_srcdir)/src \
           -I$(top_srcdir)/include \
           -o $(@D) $<

######################################################################
# Clean
######################################################################

MOSTLYCLEANFILES = \
  $(BEAMS) \
  $(top_builddir)/bin/neptune \
  $(top_builddir)/ebin/neptune.app

######################################################################

######################################################################
# Distribution
######################################################################

EXTRA_DIST = \
  $(top_builddir)/bin/neptune.in

######################################################################
