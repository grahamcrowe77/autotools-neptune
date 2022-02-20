ACLOCAL_AMFLAGS = -I m4

MODULES = @MODULES@

ESCRIPTS = @ESCRIPTS@

ATFILES = @ATFILES@

TESTSUITE = $(builddir)/tests/testsuite

# --------------------------------------
# Directory Structure
# --------------------------------------
%LC_APP_NAME%_srcdir=$(ERLANG_INSTALL_LIB_DIR_%LC_APP_NAME%)/src
%LC_APP_NAME%_ebindir=$(ERLANG_INSTALL_LIB_DIR_%LC_APP_NAME%)/ebin
%LC_APP_NAME%_incdir=$(ERLANG_INSTALL_LIB_DIR_%LC_APP_NAME%)/include
%LC_APP_NAME%_privdir=$(ERLANG_INSTALL_LIB_DIR_%LC_APP_NAME%)/priv

# --------------------------------------
# Escripts
# --------------------------------------
dist_bin_SCRIPTS = $(addprefix $(builddir)/bin/, $(ESCRIPTS))

install-exec-hook:
	for escript in $(ESCRIPTS); do \
	  $(SED) -i.tmp \
	    -e "s#%%! -pa ebin#%%! -pa $(DESTDIR)$(%LC_PACKAGE_NAME%_ebindir)#g" \
	    $(DESTDIR)$(bindir)/$$escript; \
	  rm $(DESTDIR)$(bindir)/$$escript.tmp; \
	done;

# --------------------------------------
# NIF
# --------------------------------------
%LC_APP_NAME%_priv_LTLIBRARIES = lib%LC_APP_NAME%_nif.la
lib%LC_APP_NAME%_nif_la_CFLAGS = -I@ERLANG_ROOT_DIR@/usr/include

lib%LC_APP_NAME%_nif_la_SOURCES = \
  $(srcdir)/c_src/%LC_APP_NAME%.c

BUILT_SOURCES = $(builddir)/priv

$(builddir)/priv:
	$(LN_S) $(builddir)/.libs $@

if APPLE
%LC_APP_NAME%_priv_DATA = \
  $(builddir)/.libs/lib%LC_APP_NAME%_nif.so

$(builddir)/.libs/lib%LC_APP_NAME%_nif.so: $(builddir)/.libs/lib%LC_APP_NAME%_nif.dylib
	(cd $(@D) && $(LN_S) $(<F) $(@F))
endif #APPLE

# --------------------------------------
# Public Erlang Header Files
# --------------------------------------
dist_%LC_APP_NAME%_inc_DATA = \
  $(srcdir)/include/%LC_APP_NAME%.hrl

# --------------------------------------
# Build beam files
# --------------------------------------
AM_V_ERLC = $(am__v_ERLC_@AM_V@)
am__v_ERLC_ = $(am__v_ERLC_@AM_DEFAULT_V@)
am__v_ERLC_0 = @echo "  ERLC    " $@;
am__v_ERLC_1 = 

SOURCES = $(addprefix $(srcdir)/src/, $(addsuffix .erl, $(MODULES)))
BEAMS   = $(addprefix $(builddir)/ebin/, $(addsuffix .beam, $(MODULES)))

dist_%LC_APP_NAME%_src_DATA = \
  $(%LC_APP_NAME%_priv_headers) \
  $(SOURCES)

%LC_APP_NAME%_priv_headers = \
  $(srcdir)/src/%LC_APP_NAME%_priv.hrl

%LC_APP_NAME%_ebin_DATA = \
  $(builddir)/ebin/%LC_APP_NAME%.app \
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

$(builddir)/tests/%LC_APP_NAME%.plt:
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
  $(builddir)/tests/%LC_APP_NAME%.plt

check-local: $(check_dependencies)
	cd $(builddir)/tests && $(SHELL) testsuite $(TESTSUITEFLAGS)

installcheck-local: $(check_dependencies)
	cd $(builddir)/tests && $(SHELL) testsuite \
           'AUTOTEST_PATH=$(DESTDIR)$(bindir)' \
           $(TESTSUITEFLAGS)

uninstall-local:
	rm -f  $(DESTDIR)$(bindir)/%LC_APP_NAME%_test

clean-local:
	test ! -f '$(TESTSUITE)' || \
	  (cd $(builddir)/tests && $(SHELL) testsuite --clean)
	rm -f $(builddir)/tests/atconfig
	rm -f $(TESTSUITE)
	rm -f $(builddir)/src/*.beam
	rm -f $(builddir)/priv

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
%LC_APP_NAME%_docdir=$(ERLANG_INSTALL_LIB_DIR_%LC_APP_NAME%)/doc
%LC_APP_NAME%_htmldir=$(ERLANG_INSTALL_LIB_DIR_%LC_APP_NAME%)/doc/html

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
HTMLS = $(addprefix $(builddir)/doc/html/, $(addsuffix .html, $(MODULES)))
HTMLS += \
  $(builddir)/doc/html/%LC_APP_NAME%.html \
  $(builddir)/doc/html/overview.html

HTML_EXTRAS = \
  $(builddir)/doc/html/erlang-logo.png \
  $(builddir)/doc/html/otp_doc.css

%LC_APP_NAME%_html_DATA = \
  $(builddir)/doc/html/index.html \
  $(HTMLS) \
  $(HTML_EXTRAS)

if FOP
%LC_APP_NAME%_pdfdir=$(ERLANG_INSTALL_LIB_DIR_%LC_APP_NAME%)/doc/pdf
%LC_APP_NAME%_pdf_DATA = $(builddir)/doc/pdf/%LC_APP_NAME%.pdf
endif #FOP


erl_docgen_privdir = $(ERLANG_LIB_DIR_erl_docgen)/priv

$(builddir)/doc/html/erlang-logo.png: $(erl_docgen_privdir)/images/erlang-logo.png
	$(MKDIR_P) $(@D)
	$(AM_V_GEN)cp $< $@

$(builddir)/doc/html/otp_doc.css: $(erl_docgen_privdir)/css/otp_doc.css
	$(MKDIR_P) $(@D)
	$(AM_V_GEN)cp $< $@

$(HTMLS): $(builddir)/doc/html/index.html

$(builddir)/doc/html/index.html: $(builddir)/doc/book.xml $(XMLS)
	$(AM_V_GEN)$(XSLTPROC) --output $@ \
	  --stringparam outdir $(builddir) \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam topdocdir ../../../../doc \
	  --stringparam pdfdir ../pdf \
	  --stringparam pdfname %LC_APP_NAME% \
	  --xinclude \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --stringparam stylesheet ./otp_doc.css \
	  --stringparam winprefix Erlang \
	  --stringparam logo ./erlang-logo.png \
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
$(builddir)/doc/pdf/%LC_APP_NAME%.pdf: $(builddir)/doc/pdf/%LC_APP_NAME%.fo
	$(MKDIR_P) $(@D)
	$(AM_V_GEN)$(FOP) -fo $< -pdf $@ 2> /dev/null

endif #FOP
if XSLTPROC

$(builddir)/doc/pdf/%LC_APP_NAME%.fo: $(HTMLS) $(XMLS)
	$(AM_V_GEN)$(XSLTPROC) \
	  --output $@ \
	  --stringparam docgen $(ERLANG_LIB_DIR_erl_docgen) \
	  --stringparam gendate "`date +"%B %e, %Y"`" \
	  --stringparam appname $(PACKAGE_NAME) \
	  --stringparam appver $(PACKAGE_VERSION) \
	  --stringparam logo $(top_srcdir)/doc/erlang-logo.png \
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
	  -e "s#@TOP_BUILDDIR@#$(top_builddir)#g" \
	  -e "s#@TOP_SRCDIR@#$(top_srcdir)#g" $< > $@

XMLS += $(builddir)/doc/ref_man.xml

$(builddir)/doc/book.xml: $(srcdir)/doc/book.xml.in
	$(AM_V_GEN)$(SED) \
	  -e "s#@TOP_BUILDDIR@#$(top_builddir)#g" \
	  -e "s#@TOP_SRCDIR@#$(top_srcdir)#g" $< > $@

$(builddir)/doc/%.xml: $(srcdir)/src/%.erl
	$(AM_V_GEN)$(ESCRIPT) \
	  $(ERLANG_LIB_DIR_erl_docgen)/priv/bin/xml_from_edoc.escript $<
	@mv $(builddir)/$(@F) $@
endif #ERL_DOCGEN

# --------------------------------------
# Clean
# --------------------------------------
CLEANFILES = \
  $(builddir)/tests/%LC_APP_NAME%.plt

MOSTLYCLEANFILES = \
  $(BEAMS) \
  $(builddir)/bin/%LC_APP_NAME% \
  $(builddir)/ebin/%LC_APP_NAME%.app

if ERL_DOCGEN
MOSTLYCLEANFILES += \
  $(MAN1S) \
  $(MAN3S) \
  $(XMLS) \
  $(HTMLS) \
  $(builddir)/doc/html/index.html \
  $(builddir)/doc/pdf/%LC_APP_NAME%.fo \
  $(builddir)/doc/pdf/%LC_APP_NAME%.pdf \
  $(builddir)/doc/ref_man.xml \
  $(builddir)/doc/book.xml \
  $(builddir)/doc/html/erlang-logo.png \
  $(builddir)/doc/html/otp_doc.css
endif #ERL_DOCGEN

if APPLE
MOSTLYCLEANFILES += .libs/lib%LC_APP_NAME%_nif.so
endif #APPLE

# --------------------------------------
# Distribution
# --------------------------------------
EXTRA_DIST = \
  $(srcdir)/bootstrap.sh \
  $(builddir)/bin/%LC_APP_NAME%.in \
  $(builddir)/tests/atconfig \
  $(builddir)/tests/package.m4 \
  $(TESTSOURCES)

if ERL_DOCGEN
EXTRA_DIST += \
  $(srcdir)/doc/%LC_APP_NAME%_cmd.xml \
  $(srcdir)/doc/overview.xml \
  $(srcdir)/doc/part.xml \
  $(srcdir)/doc/ref_man.xml.in.in \
  $(srcdir)/doc/book.xml.in
endif #ERL_DOCGEN
