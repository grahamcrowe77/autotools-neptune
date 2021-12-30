ACLOCAL_AMFLAGS = -I m4

SUBDIRS = . tests

neptune_srcdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/src
neptune_ebindir=$(ERLANG_INSTALL_LIB_DIR_neptune)/ebin
neptune_incdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/include
neptune_privdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/priv
neptune_docdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/doc

######################################################################
# bin directory containing Escripts
######################################################################
dist_bin_SCRIPTS = \
  $(top_builddir)/bin/neptune

$(top_builddir)/bin/neptune: $(top_srcdir)/bin/neptune.in
	$(MKDIR_P) $(@D)
	sed -e s#NEPTUNE_EBINDIR#$(DESTDIR)$(neptune_ebindir)#g $< > $@
	chmod +x $@

######################################################################

######################################################################
# c_src directory containing C sources
######################################################################
neptune_priv_LTLIBRARIES = libneptune_nif.la
libneptune_nif_la_CFLAGS = -I@ERLANG_ROOT_DIR@/usr/include

libneptune_nif_la_SOURCES = \
  $(top_srcdir)/c_src/neptune.c

######################################################################

######################################################################
# include directory containing public Erlang header files
######################################################################
dist_neptune_inc_DATA = \
  $(top_srcdir)/include/neptune.hrl

######################################################################

######################################################################
# man1 directory containing manual pages for Escripts
######################################################################
dist_man1_MANS = \
  $(top_srcdir)/man1/neptune.1

######################################################################

######################################################################
# man3 directory containing manual pages for application
######################################################################
AM_V_DOC = $(am__v_DOC_@AM_V@)
am__v_DOC_ = $(am__v_DOC_@AM_DEFAULT_V@)
am__v_DOC_0 = @echo "  GENDOC  " $@;
am__v_DOC_1 = 

dist_man3_MANS = \
  $(top_builddir)/man3/neptune.3

$(top_builddir)/man3/neptune.3: $(top_builddir)/neptune.xml
	$(AM_V_DOC)$(XSLTPROC) --output $@ \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --xinclude \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd_html_entities \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_man.xsl $<

$(top_builddir)/neptune.xml: $(top_srcdir)/src/neptune.erl
	$(AM_V_DOC)$(ESCRIPT) \
	  $(ERLANG_LIB_DIR_erl_docgen)/priv/bin/xml_from_edoc.escript $<

######################################################################

######################################################################
# doc directory containing html pages application
######################################################################
neptune_doc_DATA = \
  $(top_builddir)/doc/modules-frame.html \
  $(top_builddir)/doc/overview-summary.html \
  $(top_builddir)/doc/edoc-info \
  $(top_builddir)/doc/stylesheet.css \
  $(top_builddir)/doc/erlang.png \
  $(top_builddir)/doc/neptune.html \
  $(top_builddir)/doc/index.html

$(top_builddir)/doc/modules-frame.html:
$(top_builddir)/doc/overview-summary.html:
$(top_builddir)/doc/edoc-info:
$(top_builddir)/doc/stylesheet.css:
$(top_builddir)/doc/erlang.png:
$(top_builddir)/doc/neptune.html:
$(top_builddir)/doc/index.html:
	$(AM_V_DOC) ERL_LIBS="$(top_srcdir)/.." \
	  $(ERL) -noshell -pa $(top_srcdir)/ebin \
	  -run edoc_run application $(PACKAGE_NAME) \
	  "[{dir,\"$(top_builddir)/doc\"},{source_path,\"$(top_srcdir)/src\"}]" \
	  -s erlang halt

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
  $(BEAMS) \
  $(top_builddir)/bin/neptune \
  $(top_builddir)/man3/neptune.3 \
  $(top_builddir)/neptune.xml \
  $(top_builddir)/doc/neptune.html \
  $(top_builddir)/doc/modules-frame.html \
  $(top_builddir)/doc/overview-summary.html \
  $(top_builddir)/doc/index.html \
  $(top_builddir)/doc/edoc-info \
  $(top_builddir)/doc/stylesheet.css \
  $(top_builddir)/doc/erlang.png

######################################################################
# Distribution
######################################################################

EXTRA_DIST = \
  $(top_builddir)/bin/neptune.in

######################################################################
