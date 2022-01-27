ACLOCAL_AMFLAGS = -I m4

ESCRIPTS = neptune

ATFILES = @ATFILES@

TESTSUITE = $(builddir)/tests/testsuite

# --------------------------------------
# Escripts
# --------------------------------------
dist_bin_SCRIPTS = $(addprefix $(builddir)/bin/, $(ESCRIPTS))

# --------------------------------------
# Autotests
# --------------------------------------
TESTSOURCES = $(addprefix $(srcdir)/tests/, $(ATFILES))

AUTOM4TE = $(SHELL) $(srcdir)/build-aux/missing --run autom4te

AUTOTEST = $(AUTOM4TE) --language=autotest

check-local: $(builddir)/tests/atconfig $(builddir)/tests/atlocal $(TESTSUITE)
	cd $(builddir)/tests && $(SHELL) testsuite $(TESTSUITEFLAGS)

installcheck-local: $(builddir)/tests/atconfig $(builddir)/tests/atlocal $(TESTSUITE)
	cd $(builddir)/tests && $(SHELL) testsuite \
           'AUTOTEST_PATH=$(DESTDIR)$(bindir)' \
           $(TESTSUITEFLAGS)

uninstall-local:
	rm -f  $(DESTDIR)$(bindir)/neptune_test

clean-local:
	test ! -f '$(TESTSUITE)' || (cd $(builddir)/tests && $(SHELL) testsuite --clean)
	rm -rf $(builddir)/tests/atconfig $(builddir)/src/*.beam $(TESTSUITE)

$(builddir)/tests/atconfig: $(builddir)/config.status
	cd $(builddir) && $(SHELL) ./config.status $@

$(builddir)/tests/package.m4: $(srcdir)/configure.ac
	$(AM_V_GEN) :;{ \
          echo '# Signature of the current package.' && \
          echo 'm4_define([AT_PACKAGE_NAME], [$(PACKAGE_NAME)])' && \
          echo 'm4_define([AT_PACKAGE_TARNAME], [$(PACKAGE_TARNAME)])' && \
          echo 'm4_define([AT_PACKAGE_VERSION], [$(PACKAGE_VERSION)])' && \
          echo 'm4_define([AT_PACKAGE_STRING], [$(PACKAGE_STRING)])' && \
          echo 'm4_define([AT_PACKAGE_BUGREPORT], [$(PACKAGE_BUGREPORT)])'; \
          echo 'm4_define([AT_PACKAGE_URL], [$(PACKAGE_URL)])'; \
          } > $@

$(TESTSUITE): $(TESTSOURCES) $(builddir)/tests/package.m4
	$(AM_V_GEN) $(AUTOTEST) -I '$(srcdir)/tests' \
	  -o $@.tmp $(top_srcdir)/tests/$(@F).at; mv $@.tmp $@

# --------------------------------------
# Documentation
# --------------------------------------
if ERL_DOCGEN
#neptune_docdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/doc

if XSLTPROC
# --------------------------------------
# man1 pages
# --------------------------------------
MAN1S = $(addprefix $(builddir)/man1/, $(addsuffix .1, $(ESCRIPTS)))

man1_MANS = \
  $(MAN1S)

$(builddir)/man1/%.1: $(srcdir)/doc/%.xml
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
MAN3S = $(addprefix $(builddir)/man3/, $(addsuffix .3, $(MODULES)))

man3_MANS = \
  $(MAN3S)

$(builddir)/man3/%.3: $(builddir)/doc/%.xml
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
HTMLS = $(addprefix $(builddir)/doc/, $(addsuffix .html, $(MODULES)))
HTMLS += \
  $(builddir)/doc/neptune.html \
  $(builddir)/doc/overview.html \
  $(builddir)/doc/users_guide.html

HTML_EXTRAS = \
  $(srcdir)/doc/neptune.jpg \
  $(builddir)/doc/stylesheet.css

pkgdata_DATA = \
  $(builddir)/doc/index.html \
  $(HTMLS) \
  $(HTML_EXTRAS)

if FOP
pkgdata_DATA += $(builddir)/doc/neptune.pdf
endif #FOP

$(builddir)/doc/stylesheet.css:
	$(AM_V_GEN)cp $(ERLANG_LIB_DIR_erl_docgen)/priv/css/otp_doc.css $@

$(HTMLS): $(builddir)/doc/index.html

$(builddir)/doc/index.html: $(builddir)/doc/book.xml $(XMLS)
	$(AM_V_GEN)$(XSLTPROC) --output $@ \
	  --stringparam outdir $(builddir) \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam topdocdir . \
	  --stringparam pdfdir . \
	  --stringparam pdfname neptune \
	  --xinclude \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --stringparam stylesheet ./stylesheet.css \
	  --stringparam winprefix Erlang \
	  --stringparam logo ./neptune.jpg \
	  --stringparam extra_front_page_info "" \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd \
	  -path $(ERLANG_LIB_DIR_erl_docgen)/priv/dtd_html_entities \
	  -path $(top_srcdir)/doc \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_html.xsl $< 2> /dev/null
endif #XSLTPROC

if FOP
# --------------------------------------
# pdf pages
# --------------------------------------
$(builddir)/doc/neptune.pdf: $(builddir)/doc/neptune.fo
	$(AM_V_GEN)$(FOP) -fo $< -pdf $@ 2> /dev/null

endif #FOP
if XSLTPROC

$(builddir)/doc/neptune.fo: $(HTMLS) $(XMLS)
	$(AM_V_GEN)$(XSLTPROC) \
	  --output $@ \
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
	  -path $(top_srcdir)/doc \
	  -path $(top_builddir)/doc \
          $(ERLANG_LIB_DIR_erl_docgen)/priv/xsl/db_pdf.xsl \
	  $(builddir)/book.xml 2> /dev/null
endif #XSLTPROC
endif #ERL_DOCGEN

# --------------------------------------
# Clean
# --------------------------------------
if ERL_DOCGEN
CLEANFILES = \
  $(MAN1S) \
  $(MAN3S) \
  $(XMLS) \
  $(HTMLS) \
  $(builddir)/doc/index.html \
  $(builddir)/doc/neptune.fo \
  $(builddir)/doc/neptune.pdf \
  $(builddir)/doc/stylesheet.css
endif #ERL_DOCGEN

# --------------------------------------
# Distribution
# --------------------------------------
EXTRA_DIST = \
  $(srcdir)/bootstrap.sh \
  $(builddir)/bin/neptune.in \
  $(builddir)/tests/atconfig \
  $(builddir)/tests/package.m4 \
  $(TESTSOURCES)

if ERL_DOCGEN
EXTRA_DIST += \
  $(srcdir)/doc/neptune.xml \
  $(srcdir)/doc/overview.xml \
  $(srcdir)/doc/part.xml \
  $(srcdir)/doc/ref_man.xml \
  $(srcdir)/doc/book.xml \
  $(srcdir)/doc/neptune.jpg
endif #ERL_DOCGEN
