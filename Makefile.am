ACLOCAL_AMFLAGS = -I m4

SUBDIRS = . tests

######################################################################
# bin directory containing Escripts
######################################################################
dist_bin_SCRIPTS = \
  $(top_builddir)/bin/neptune

######################################################################

######################################################################
# c_src directory containing C sources
######################################################################
neptune_privdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/priv

neptune_priv_LTLIBRARIES = libneptune_nif.la
libneptune_nif_la_CFLAGS = -I@ERLANG_ROOT_DIR@/usr/include

libneptune_nif_la_SOURCES = \
  $(top_srcdir)/c_src/neptune.c

neptune_incdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/include

######################################################################

######################################################################
# include directory containing public Erlang header files
######################################################################
dist_neptune_inc_DATA = \
  $(top_srcdir)/include/neptune.hrl

######################################################################

######################################################################
# man directory containing manual pages for Escripts
######################################################################
dist_man1_MANS = \
  $(top_srcdir)/man/neptune.1

######################################################################

######################################################################
# src directory containing Erlang sources and private header files
######################################################################
AM_V_ERLC = $(am__v_ERLC_@AM_V@)
am__v_ERLC_ = $(am__v_ERLC_@AM_DEFAULT_V@)
am__v_ERLC_0 = @echo "  ERLC    " $@;
am__v_ERLC_1 = 

MODULES = \
  neptune

SOURCES = $(addprefix $(top_srcdir)/src/, $(addsuffix .erl, $(MODULES)))
BEAMS   = $(addprefix $(top_builddir)/ebin/, $(addsuffix .beam, $(MODULES)))

neptune_srcdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/src
neptune_ebindir=$(ERLANG_INSTALL_LIB_DIR_neptune)/ebin

dist_neptune_src_DATA = \
  $(neptune_priv_headers) \
  $(SOURCES)

neptune_priv_headers = \
  $(top_srcdir)/src/neptune_limits.hrl

neptune_ebin_DATA = \
  $(top_builddir)/ebin/neptune.app \
  $(BEAMS)

$(top_builddir)/ebin/%.beam: $(SOURCES)
	$(AM_V_ERLC)$(ERLC) $(ERLCFLAGS) \
           -I$(top_srcdir)/src \
           -I$(top_srcdir)/include \
           -o $(@D) $<

MOSTLYCLEANFILES = \
  $(BEAMS)

######################################################################
