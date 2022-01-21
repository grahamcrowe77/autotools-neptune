ACLOCAL_AMFLAGS = -I m4

if ERL_DOCGEN

neptune_docdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/doc

######################################################################
# Silent builds
######################################################################

AM_V_DOC = $(am__v_DOC_@AM_V@)
am__v_DOC_ = $(am__v_DOC_@AM_DEFAULT_V@)
am__v_DOC_0 = @echo "  GENDOC  " $@;
am__v_DOC_1 = 

######################################################################

######################################################################
# Manual Pages for Application Modules
######################################################################

if XSLTPROC

MAN3S = $(addprefix $(top_builddir)/man3/, $(addsuffix .3, @MODULES@))

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

HTMLS = $(addprefix $(top_builddir)/doc/, $(addsuffix .html, @MODULES@))
HTMLS += \
  $(top_builddir)/doc/overview.html \
  $(top_builddir)/doc/make-targets.html \
  $(top_builddir)/doc/users_guide.html

HTML_EXTRAS = \
  $(top_srcdir)/doc/neptune.jpg \
  $(top_builddir)/doc/stylesheet.css

neptune_doc_DATA = \
  $(top_builddir)/doc/index.html \
  $(HTMLS) \
  $(HTML_EXTRAS)

if FOP

neptune_doc_DATA += $(top_builddir)/doc/neptune.pdf

endif #FOP

$(top_builddir)/doc/stylesheet.css:
	cp $(ERLANG_LIB_DIR_erl_docgen)/priv/css/otp_doc.css $@

$(HTMLS): $(top_builddir)/doc/index.html

$(top_builddir)/doc/index.html: $(top_builddir)/doc/book.xml $(XMLS)
	$(AM_V_DOC)$(XSLTPROC) --output $@ \
	  --stringparam outdir $(@D) \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam topdocdir $(@D) \
	  --stringparam pdfdir $(@D) \
	  --stringparam pdfname neptune \
	  --xinclude \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --stringparam stylesheet $(top_srcdir)/doc/stylesheet.css \
	  --stringparam winprefix Erlang \
	  --stringparam logo $(top_srcdir)/doc/neptune.jpg \
	  --stringparam extra_front_page_info "" \
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

$(top_builddir)/doc/neptune.fo: $(HTMLS) $(XMLS)
	$(AM_V_DOC)$(XSLTPROC) \
	  --output $(@F) \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --stringparam logo $(top_srcdir)/doc/neptune.jpg \
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

XMLS = $(addprefix $(top_builddir)/doc/, $(addsuffix .xml, @MODULES@))

$(top_builddir)/doc/ref_man.xml: $(top_builddir)/doc/ref_man.xml.in
	$(SED) -e "s#@TOP_BUILDDIR@#$(top_builddir)#g" $< > $@

XMLS += $(top_builddir)/doc/ref_man.xml

$(top_builddir)/doc/book.xml: $(top_srcdir)/doc/book.xml.in
	$(SED) \
	  -e "s#@TOP_BUILDDIR@#$(top_builddir)#g" \
	  -e "s#@TOP_SRCDIR@#$(top_srcdir)#g" $< > $@

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
  $(top_builddir)/doc/index.html \
  $(top_builddir)/doc/neptune.fo \
  $(top_builddir)/doc/neptune.pdf \
  $(top_builddir)/doc/ref_man.xml.in \
  $(top_builddir)/doc/ref_man.xml \
  $(top_builddir)/doc/book.xml \
  $(top_builddir)/doc/stylesheet.css

######################################################################

######################################################################
# Distribution
######################################################################

EXTRA_DIST = \
  $(top_srcdir)/doc/overview.xml \
  $(top_srcdir)/doc/part.xml \
  $(top_srcdir)/doc/make-targets.xml \
  $(top_srcdir)/doc/ref_man.xml.in.in \
  $(top_srcdir)/doc/book.xml.in \
  $(top_srcdir)/doc/neptune.jpg

######################################################################

endif #ERL_DOCGEN
