if ERL_DOCGEN

neptune_docdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/doc

include ../common.mk

######################################################################
# Manual Pages for Application Modules
######################################################################

if XSLTPROC

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

endif #XSLTPROC

######################################################################
# HTML Pages for Application Modules
######################################################################

if XSLTPROC

HTMLS = $(addprefix $(top_builddir)/doc/, $(addsuffix .html, $(MODULES)))

HTML_EXTRAS = \
  $(top_builddir)/doc/erlang.png \
  $(top_builddir)/doc/stylesheet.css

neptune_doc_DATA = \
  $(HTMLS) \
  $(top_builddir)/doc/modules-frame.html \
  $(HTML_EXTRAS)

if FOP

neptune_doc_DATA += $(top_builddir)/doc/neptune.pdf

endif #FOP

$(top_builddir)/doc/modules-frame.html: $(HTMLS) $(HTML_EXTRAS)
	modules=$(am__modules_frame); \
	sed -e "s#@MODULES@#$$modules#g" $(top_srcdir)/doc/modules-frame.html.in > $@

$(top_builddir)/doc/erlang.png:
	cp $(ERLANG_LIB_DIR_erl_docgen)/priv/images/erlang-logo.png $@

$(top_builddir)/doc/stylesheet.css:
	cp $(ERLANG_LIB_DIR_erl_docgen)/priv/css/otp_doc.css $@

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
	  --stringparam stylesheet stylesheet.css \
	  --stringparam winprefix Erlang \
          --stringparam logo erlang.png \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd_html_entities \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_html.xsl $<

endif #XSLTPROC

######################################################################
# PDF for Application
######################################################################

if FOP

$(top_builddir)/doc/neptune.pdf: $(top_builddir)/doc/neptune.fo
	$(FOP) -fo $< -pdf $@

endif #FOP

if XSLTPROC

$(top_builddir)/doc/neptune.fo: $(HTMLS) $(top_builddir)/doc/ref_man.xml
	$(AM_V_DOC)$(XSLTPROC) \
	  --output $(@F) \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
          --stringparam logo erlang.png \
	  --stringparam pdfcolor "#960003" \
	  --stringparam extra_front_page_info "$(PACKAGE_NAME)" \
	  --xinclude \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd_html_entities \
	  -path $(top_builddir)/doc \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_pdf.xsl \
	  $(top_srcdir)/doc/book.xml > $@

endif #XSLTPROC

######################################################################
# XMLs used for building HTML and Manual Pages
######################################################################

$(top_builddir)/doc/ref_man.xml: $(top_srcdir)/doc/ref_man.xml.in
	modules=$(am__xi_modules); \
	sed -e "s#@XI_MODULES@#$$modules#g" $< > $@

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
  $(top_builddir)/doc/neptune.pdf \
  $(top_builddir)/doc/ref_man.xml \
  $(top_builddir)/doc/modules-frame.html \
  $(top_builddir)/doc/erlang.png \
  $(top_builddir)/doc/stylesheet.css

######################################################################

######################################################################
# Distribution
######################################################################

EXTRA_DIST = \
  $(top_srcdir)/doc/book.xml \
  $(top_srcdir)/doc/overview.xml \
  $(top_srcdir)/doc/part.xml \
  $(top_srcdir)/doc/make-targets.xml \
  $(top_srcdir)/doc/ref_man.xml.in \
  $(top_srcdir)/doc/modules-frame.html.in \
  $(top_srcdir)/doc/index.html \
  $(top_srcdir)/doc/overview-summary.html

######################################################################

endif #ERL_DOCGEN
