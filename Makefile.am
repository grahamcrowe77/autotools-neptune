ACLOCAL_AMFLAGS = -I m4

ESCRIPTS = neptune

ATFILES = @ATFILES@

TESTSUITE = $(builddir)/tests/testsuite

# --------------------------------------
# Directory Structure
# --------------------------------------
neptune_srcdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/src
neptune_ebindir=$(ERLANG_INSTALL_LIB_DIR_neptune)/ebin

# --------------------------------------
# Escripts
# --------------------------------------
dist_bin_SCRIPTS = $(addprefix $(builddir)/bin/, $(ESCRIPTS))

install-exec-hook:
	$(AM_V_GEN)$(SED) -i.tmp \
	  -e "s#%%! -pa ebin#%%! -pa $(DESTDIR)$(neptune_ebindir)#g" \
	  -e "s#<<\".\">>#<<\"$(DESTDIR)$(neptune_sysconfdir)\">>#g" \
	  $(DESTDIR)$(bindir)/neptune
	rm $(DESTDIR)$(bindir)/neptune.tmp

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

neptune_priv_headers =

neptune_ebin_DATA = \
  $(builddir)/ebin/neptune.app \
  $(BEAMS)

$(builddir)/ebin/%.beam: $(srcdir)/src/%.erl
	$(AM_V_ERLC)$(ERLC) $(ERLCFLAGS) \
           -I$(srcdir)/src \
           -I$(srcdir)/include \
           -o $(@D) $<

# --------------------------------------
# Dialyzer PLT
# --------------------------------------
DIALYZER_APPS = \
  erts \
  kernel \
  stdlib \
  compiler \
  crypto

$(builddir)/tests/neptune.plt:
	$(AM_V_GEN)$(DIALYZER) --build_plt --output_plt $@ \
          --apps $(DIALYZER_APPS)

# --------------------------------------
# Autotests
# --------------------------------------
TESTSOURCES = $(addprefix $(srcdir)/tests/, $(ATFILES))

AUTOM4TE = $(SHELL) $(srcdir)/build-aux/missing --run autom4te

AUTOTEST = $(AUTOM4TE) --language=autotest

check_dependencies = \
  $(TESTSUITE) \
  $(builddir)/tests/atconfig \
  $(builddir)/tests/atlocal \
  $(builddir)/tests/neptune.plt

check-local: $(check_dependencies)
	cd $(builddir)/tests && $(SHELL) testsuite $(TESTSUITEFLAGS)

installcheck-local: $(check_dependencies)
	cd $(builddir)/tests && $(SHELL) testsuite \
           'AUTOTEST_PATH=$(DESTDIR)$(bindir)' \
           $(TESTSUITEFLAGS)

uninstall-local:
	rm -f  $(DESTDIR)$(bindir)/neptune_test

clean-local:
	test ! -f '$(TESTSUITE)' || \
          (cd $(builddir)/tests && $(SHELL) testsuite --clean)
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
# Templates
# --------------------------------------
neptune_sysconfdir = $(sysconfdir)/neptune

neptune_appdir = $(neptune_sysconfdir)/app
neptune_appbindir = $(neptune_sysconfdir)/app/bin
neptune_appcsrcdir = $(neptune_sysconfdir)/app/c_src
neptune_appdocdir = $(neptune_sysconfdir)/app/doc
neptune_appebindir = $(neptune_sysconfdir)/app/ebin
neptune_appincludedir = $(neptune_sysconfdir)/app/include
neptune_appm4dir = $(neptune_sysconfdir)/app/m4
neptune_appsrcdir = $(neptune_sysconfdir)/app/src
neptune_apptestsdir = $(neptune_sysconfdir)/app/tests

neptune_app_DATA = \
  app/Makefile.am \
  app/README.md \
  app/bootstrap.sh \
  app/configure.ac \
  app/.gitignore

neptune_appbin_DATA = \
  app/bin/name.in

neptune_appcsrc_DATA = \
  app/c_src/name.c

neptune_appdoc_DATA = \
  app/doc/book.xml.in \
  app/doc/name_cmd.xml \
  app/doc/overview.xml \
  app/doc/part.xml \
  app/doc/ref_man.xml.in.in

neptune_appebin_DATA = \
  app/ebin/name.app.in

neptune_appinclude_DATA = \
  app/include/name.hrl

neptune_appm4_DATA = \
  app/m4/erlang-app.m4

neptune_appsrc_DATA = \
  app/src/name_app.erl \
  app/src/name_nif.erl \
  app/src/name_priv.hrl \
  app/src/name_server.erl \
  app/src/name_sup.erl

neptune_apptests_DATA = \
  app/tests/atlocal.in \
  app/tests/local.at \
  app/tests/name-ct.at \
  app/tests/name-dialyzer.at \
  app/tests/name-error.at \
  app/tests/name-square.at \
  app/tests/name-version.at \
  app/tests/name_SUITE.erl \
  app/tests/name_nif-eunit.at \
  app/tests/testsuite.at

neptune_rel_sysconfdir = $(neptune_sysconfdir)/rel

# --------------------------------------
# Documentation
# --------------------------------------
if ERL_DOCGEN
neptune_docdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/doc

if XSLTPROC
# --------------------------------------
# man1 pages
# --------------------------------------
MAN1S = $(addprefix $(builddir)/man1/, $(addsuffix .1, $(ESCRIPTS)))

man1_MANS = \
  $(MAN1S)

$(builddir)/man1/%.1: $(srcdir)/doc/%_cmd.xml
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
  $(builddir)/doc/app.html \
  $(builddir)/doc/users_guide.html \
  $(builddir)/doc/overview.html \
  $(builddir)/doc/applications.html \
  $(builddir)/doc/releases.html

HTML_EXTRAS = \
  $(srcdir)/doc/neptune.jpg \
  $(builddir)/doc/stylesheet.css

neptune_doc_DATA = \
  $(builddir)/doc/index.html \
  $(HTMLS) \
  $(HTML_EXTRAS)

if FOP
neptune_doc_DATA += $(builddir)/doc/neptune.pdf
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
	  -path $(top_builddir)/doc \
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

# --------------------------------------
# Generated xml files
# --------------------------------------
XMLS = $(addprefix $(builddir)/doc/, $(addsuffix .xml, $(MODULES)))

$(builddir)/doc/ref_man.xml: $(builddir)/doc/ref_man.xml.in
	$(AM_V_GEN)$(SED) \
	  -e "s#@TOP_BUILDDIR@#$(top_srcdir)#g" \
	  -e "s#@TOP_SRCDIR@#$(top_srcdir)#g" $< > $@

XMLS += $(builddir)/doc/ref_man.xml

$(builddir)/doc/%.xml: $(srcdir)/src/%.erl
	$(AM_V_GEN)$(ESCRIPT) \
	  $(ERLANG_LIB_DIR_erl_docgen)/priv/bin/xml_from_edoc.escript $<
	@mv $(builddir)/$(@F) $@

endif #ERL_DOCGEN

# --------------------------------------
# Clean
# --------------------------------------
CLEANFILES = \
  $(builddir)/tests/neptune.plt

MOSTLYCLEANFILES = \
  $(BEAMS) \
  $(builddir)/bin/neptune \
  $(builddir)/ebin/neptune.app

if ERL_DOCGEN
MOSTLYCLEANFILES += \
  $(MAN1S) \
  $(MAN3S) \
  $(XMLS) \
  $(HTMLS) \
  $(builddir)/doc/index.html \
  $(builddir)/doc/neptune.fo \
  $(builddir)/doc/neptune.pdf \
  $(builddir)/doc/ref_man.xml \
  $(builddir)/doc/stylesheet.css
endif #ERL_DOCGEN

# --------------------------------------
# Distribution
# --------------------------------------
EXTRA_DIST = \
  $(srcdir)/bootstrap.sh \
  $(neptune_app_DATA) \
  $(neptune_appbin_DATA) \
  $(neptune_appcsrc_DATA) \
  $(neptune_appdoc_DATA) \
  $(neptune_appebin_DATA) \
  $(neptune_appinclude_DATA) \
  $(neptune_appm4_DATA) \
  $(neptune_appsrc_DATA) \
  $(neptune_apptests_DATA) \
  $(builddir)/bin/neptune.in \
  $(builddir)/tests/atconfig \
  $(builddir)/tests/package.m4 \
  $(TESTSOURCES)

if ERL_DOCGEN
EXTRA_DIST += \
  $(srcdir)/doc/neptune_cmd.xml \
  $(srcdir)/doc/app.xml \
  $(srcdir)/doc/overview.xml \
  $(srcdir)/doc/applications.xml \
  $(srcdir)/doc/releases.xml \
  $(srcdir)/doc/part.xml \
  $(srcdir)/doc/ref_man.xml.in.in \
  $(srcdir)/doc/book.xml \
  $(srcdir)/doc/neptune.jpg
endif #ERL_DOCGEN
