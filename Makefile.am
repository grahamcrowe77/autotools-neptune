ACLOCAL_AMFLAGS = -I m4

SUBDIRS = . tests

######################################################################
# Application Structure
######################################################################

neptune_srcdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/src
neptune_ebindir=$(ERLANG_INSTALL_LIB_DIR_neptune)/ebin
neptune_incdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/include
neptune_privdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/priv
neptune_docdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/doc

MODULES = \
  neptune

######################################################################

######################################################################
# Silent builds
######################################################################

AM_V_DOC = $(am__v_DOC_@AM_V@)
am__v_DOC_ = $(am__v_DOC_@AM_DEFAULT_V@)
am__v_DOC_0 = @echo "  GENDOC  " $@;
am__v_DOC_1 = 

AM_V_ERLC = $(am__v_ERLC_@AM_V@)
am__v_ERLC_ = $(am__v_ERLC_@AM_DEFAULT_V@)
am__v_ERLC_0 = @echo "  ERLC    " $@;
am__v_ERLC_1 = 

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
# Manual Pages for Application Modules
######################################################################

MAN3S = $(addprefix $(top_builddir)/man3/, $(addsuffix .3, $(MODULES)))

dist_man3_MANS = \
  $(MAN3S)

$(top_builddir)/man3/%.3: $(top_builddir)/%.xml
	$(AM_V_DOC)$(XSLTPROC) --output $@ \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --xinclude \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd_html_entities \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_man.xsl $<

######################################################################
# HTML Pages for Application Modules
######################################################################

HTMLS = $(addprefix $(top_builddir)/doc/, $(addsuffix .html, $(MODULES)))

neptune_doc_DATA = \
  $(HTMLS)

$(top_builddir)/doc/%.html: $(top_builddir)/%.xml
	$(AM_V_DOC)$(XSLTPROC) --output $@ \
	  --stringparam outdir $(@D) \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam topdocdir $(@D) \
	  --stringparam pdfdir $(@D) \
	  --xinclude \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --stringparam stylesheet \
	    $(ERLANG_LIB_DIR_erl_docgen)/priv/css/otp_doc.css \
	  --stringparam winprefix Erlang \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd_html_entities \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_html.xsl $<

######################################################################
# Generated XMLs used for building HTML and Manual Pages
######################################################################

XMLS = $(addprefix $(top_builddir)/, $(addsuffix .xml, $(MODULES)))

$(top_builddir)/%.xml: $(top_srcdir)/src/%.erl
	$(AM_V_DOC)$(ESCRIPT) \
	  $(ERLANG_LIB_DIR_erl_docgen)/priv/bin/xml_from_edoc.escript $<

######################################################################

######################################################################
# Application Build
######################################################################

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

######################################################################
# Clean
######################################################################

MOSTLYCLEANFILES = \
  $(BEAMS) \
  $(top_builddir)/bin/neptune \
  $(MAN3S) \
  $(XMLS) \
  $(HTMLS)

######################################################################
# Distribution
######################################################################

EXTRA_DIST = \
  $(top_builddir)/bin/neptune.in

######################################################################
