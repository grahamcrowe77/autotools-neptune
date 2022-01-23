ACLOCAL_AMFLAGS = -I m4

MODULES = @MODULES@

ESCRIPTS = neptune

if ERL_DOCGEN
neptune_docdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/doc

if XSLTPROC
# --------------------------------------
# man1 pages
# --------------------------------------
MAN1S = $(addprefix $(top_builddir)/man1/, $(addsuffix .1, $(ESCRIPTS)))

man1_MANS = \
  $(MAN1S)

$(top_builddir)/man1/%.1: $(srcdir)/%.xml
	$(AM_V_GEN)$(XSLTPROC) --output $@ \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --xinclude \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd_html_entities \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_man.xsl $< 2> /dev/null

# --------------------------------------
# man3 pages
# --------------------------------------
MAN3S = $(addprefix $(top_builddir)/man3/, $(addsuffix .3, $(MODULES)))

man3_MANS = \
  $(MAN3S)

$(top_builddir)/man3/%.3: $(builddir)/%.xml
	$(AM_V_GEN)$(XSLTPROC) --output $@ \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --xinclude \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd_html_entities \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_man.xsl $< 2> /dev/null
endif #XSLTPROC

if XSLTPROC
# --------------------------------------
# html pages
# --------------------------------------
HTMLS = $(addprefix $(builddir)/, $(addsuffix .html, $(MODULES)))
HTMLS += \
  $(builddir)/neptune.html \
  $(builddir)/overview.html \
  $(builddir)/make-targets.html \
  $(builddir)/users_guide.html

HTML_EXTRAS = \
  $(srcdir)/neptune.jpg \
  $(builddir)/stylesheet.css

neptune_doc_DATA = \
  $(builddir)/index.html \
  $(HTMLS) \
  $(HTML_EXTRAS)

if FOP
neptune_doc_DATA += $(builddir)/neptune.pdf
endif #FOP

$(builddir)/stylesheet.css:
	$(AM_V_GEN)cp $(ERLANG_LIB_DIR_erl_docgen)/priv/css/otp_doc.css $@

$(HTMLS): $(builddir)/index.html

$(builddir)/index.html: $(builddir)/book.xml $(XMLS)
	$(AM_V_GEN)$(XSLTPROC) --output $@ \
	  --stringparam outdir $(@D) \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam topdocdir $(@D) \
	  --stringparam pdfdir $(@D) \
	  --stringparam pdfname neptune \
	  --xinclude \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --stringparam stylesheet $(srcdir)/stylesheet.css \
	  --stringparam winprefix Erlang \
	  --stringparam logo $(srcdir)/neptune.jpg \
	  --stringparam extra_front_page_info "" \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd_html_entities \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_html.xsl $< 2> /dev/null
endif #XSLTPROC

if FOP
# --------------------------------------
# pdf pages
# --------------------------------------
$(builddir)/neptune.pdf: $(builddir)/neptune.fo
	$(AM_V_GEN)$(FOP) -fo $< -pdf $@ 2> /dev/null

endif #FOP
if XSLTPROC

$(builddir)/neptune.fo: $(HTMLS) $(XMLS)
	$(AM_V_GEN)$(XSLTPROC) \
	  --output $@ \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --stringparam logo $(srcdir)/neptune.jpg \
	  --stringparam pdfcolor "#960003" \
	  --stringparam extra_front_page_info "$(PACKAGE_NAME)" \
	  --xinclude \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd_html_entities \
	  -path $(builddir)/doc \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_pdf.xsl \
	  $(builddir)/book.xml 2> /dev/null
endif #XSLTPROC

# --------------------------------------
# Generated xml files
# --------------------------------------
XMLS = $(addprefix $(builddir)/, $(addsuffix .xml, $(MODULES)))

$(builddir)/ref_man.xml: $(builddir)/ref_man.xml.in
	$(AM_V_GEN)$(SED) \
	  -e "s#@TOP_BUILDDIR@#$(top_builddir)#g" \
	  -e "s#@TOP_SRCDIR@#$(top_srcdir)#g" $< > $@

XMLS += $(builddir)/ref_man.xml

$(builddir)/book.xml: $(srcdir)/book.xml.in
	$(AM_V_GEN)$(SED) \
	  -e "s#@TOP_BUILDDIR@#$(top_builddir)#g" \
	  -e "s#@TOP_SRCDIR@#$(top_srcdir)#g" $< > $@

$(builddir)/%.xml: $(top_srcdir)/src/%.erl
	$(AM_V_GEN)$(ESCRIPT) \
	  $(ERLANG_LIB_DIR_erl_docgen)/priv/bin/xml_from_edoc.escript $<

# --------------------------------------
# Clean
# --------------------------------------
MOSTLYCLEANFILES = \
  $(MAN1S) \
  $(MAN3S) \
  $(XMLS) \
  $(HTMLS) \
  $(builddir)/index.html \
  $(builddir)/neptune.fo \
  $(builddir)/neptune.pdf \
  $(builddir)/ref_man.xml \
  $(builddir)/book.xml \
  $(builddir)/stylesheet.css

# --------------------------------------
# Distribution
# --------------------------------------
EXTRA_DIST = \
  $(srcdir)/neptune.xml \
  $(srcdir)/overview.xml \
  $(srcdir)/part.xml \
  $(srcdir)/make-targets.xml \
  $(srcdir)/ref_man.xml.in.in \
  $(srcdir)/book.xml.in \
  $(srcdir)/neptune.jpg
endif #ERL_DOCGEN
