neptune_docdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/doc

include ../common.mk

######################################################################
# Manual Pages for Application Modules
######################################################################

MAN3S = $(addprefix $(top_builddir)/man3/, $(addsuffix .3, $(MODULES)))

man3_MANS = \
  $(MAN3S)

$(top_builddir)/man3/%.3: $(top_builddir)/doc/%.xml
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
  $(HTMLS) \
  $(top_builddir)/doc/neptune.pdf

$(top_builddir)/doc/%.html: $(top_builddir)/doc/%.xml
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
# PDF for Application
######################################################################

$(top_builddir)/doc/neptune.pdf: $(top_builddir)/doc/neptune.fo
	$(FOP) -fo $< -pdf $@

$(top_builddir)/doc/neptune.fo: $(HTMLS)
	$(AM_V_DOC)$(XSLTPROC)  --output neptune.fo \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --stringparam logo \
	    $(ERLANG_LIB_DIR_erl_docgen)/priv/images/erlang-logo.png \
	  --stringparam pdfcolor "#960003" \
	  --stringparam extra_front_page_info "$(PACKAGE_NAME)" \
	  --xinclude \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd_html_entities \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_pdf.xsl \
	  $(top_srcdir)/doc/book.xml > $@

######################################################################
# Generated XMLs used for building HTML and Manual Pages
######################################################################

XMLS = $(addprefix $(top_builddir)/, $(addsuffix .xml, $(MODULES)))

$(top_builddir)/doc/%.xml: $(top_srcdir)/src/%.erl
	$(AM_V_DOC)$(ESCRIPT) \
	  $(ERLANG_LIB_DIR_erl_docgen)/priv/bin/xml_from_edoc.escript $<

######################################################################

######################################################################
# Clean
######################################################################

MOSTLYCLEANFILES = \
  $(MAN3S) \
  $(XMLS) \
  $(HTMLS) \
  $(top_builddir)/doc/neptune.fo \
  $(top_builddir)/doc/neptune.pdf

######################################################################

######################################################################
# Distribution
######################################################################

EXTRA_DIST = \
  $(top_srcdir)/doc/book.xml \
  $(top_srcdir)/doc/overview.xml \
  $(top_srcdir)/doc/part.xml \
  $(top_srcdir)/doc/ref_man.xml

######################################################################
