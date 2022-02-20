ACLOCAL_AMFLAGS = -I m4

APPS = \
  $(ERLANG_LIB_DIR_kernel) \
  $(ERLANG_LIB_DIR_stdlib) \
  $(ERLANG_LIB_DIR_sasl) \
  $(ERLANG_LIB_DIR_uranus)

ALT_ERLANG_DOC_ROOT = /usr/share/doc/packages/erlang-doc

ATFILES = @ATFILES@

TESTSUITE = $(builddir)/tests/testsuite

# --------------------------------------
# Release
# --------------------------------------
releases:
lib: $(PACKAGE_NAME).tar.gz
	tar xf $<

$(PACKAGE_NAME).tar.gz: $(PACKAGE_NAME).boot
	$(ERL) -noshell -run systools make_tar $(PACKAGE_NAME) -s erlang halt

$(PACKAGE_NAME).script $(PACKAGE_NAME).boot: $(PACKAGE_NAME).rel
	$(ERL) -noshell -s systools make_script $(PACKAGE_NAME) -s erlang halt

bin_SCRIPTS = \
  bin/$(PACKAGE_NAME)-system.sh

pkgbindir=$(pkgdatadir)/erts-$(ERLANG_ERTS_VER)/bin

ERTS_MODS = \
  run_erl \
  erl \
  beam.smp \
  epmd \
  erlexec \
  erl_child_setup \
  inet_gethost

ERTS = $(addprefix $(builddir)/erts-$(ERLANG_ERTS_VER)/bin/, $(ERTS_MODS))

dist_pkgbin_DATA = \
  $(ERTS) \
  $(builddir)/releases/$(PACKAGE_VERSION)/$(PACKAGE_NAME).config

$(builddir)/releases/$(PACKAGE_VERSION)/$(PACKAGE_NAME).config: $(PACKAGE_NAME).config
	$(MKDIR_P) $(@D) && cp -p $< $@

$(builddir)/erts-$(ERLANG_ERTS_VER)/bin/%: $(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/bin/%
	$(MKDIR_P) $(@D) && cp -p $< $@


if ERL_DOCGEN
erl_docgen_privdir = $(ERLANG_LIB_DIR_erl_docgen)/priv

$(builddir)/doc/otp_doc.css: $(erl_docgen_privdir)/css/otp_doc.css
	$(AM_V_GEN)cp $< $@

.PHONY:
docs: lib $(builddir)/doc/otp_doc.css
	cp -r $(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/doc \
	  erts-$(ERLANG_ERTS_VER) 2>/dev/null || true; \
	cp -r $(ALT_ERLANG_DOC_ROOT)/erts-$(ERLANG_ERTS_VER)/doc \
	  erts-$(ERLANG_ERTS_VER) 2>/dev/null || true; \
	for app in $(APPS); do \
	  cp -r $(ERLANG_ROOT_DIR)/lib/`basename $$app`/doc \
	    lib/`basename $$app` 2>/dev/null || true; \
	  cp -r $(ALT_ERLANG_DOC_ROOT)/lib/`basename $$app`/doc \
	    lib/`basename $$app` 2>/dev/null || true; \
	done;
endif #ERL_DOCGEN

if ERL_DOCGEN
all-local: docs
else
all-local: lib
endif #ERL_DOCGEN

install-data-local:
	$(MKDIR_P) $(DESTDIR)$(pkgdatadir)
	cp -vr lib $(DESTDIR)$(pkgdatadir)
	cp -vr releases $(DESTDIR)$(pkgdatadir)
	cp -v $(PACKAGE_NAME).config \
	  $(DESTDIR)$(pkgdatadir)/releases/$(PACKAGE_VERSION)

install-data-hook:
	$(SED) -i.tmp \
	  -e "s#ERL_ROOTDIR=$$ROOTDIR#ERL_ROOTDIR=$(DESTDIR)$(pkgdatadir)#g" \
	  $(DESTDIR)$(bindir)/$(PACKAGE_NAME)-system.sh
	chmod +x $(DESTDIR)$(pkgbindir)/*

# --------------------------------------
# Dialyzer PLT
# --------------------------------------
DIALYZER_APPS = \
  erts \
  kernel \
  stdlib \
  sasl \
  compiler \
  crypto

$(builddir)/tests/%LC_REL_NAME%.plt:
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
	rm -f  $(DESTDIR)$(bindir)/%LC_REL_NAME%_test

clean-local:
	test ! -f '$(TESTSUITE)' || \
	  (cd $(builddir)/tests && $(SHELL) testsuite --clean)
	rm -rf $(builddir)/tests/atconfig $(TESTSUITE)
	rm -rf erts-$(ERLANG_ERTS_VER)
	rm -rf releases
	rm -rf lib

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

if XSLTPROC

if FOP

endif #FOP

endif #XSLTPROC

endif #ERL_DOCGEN

# --------------------------------------
# Clean
# --------------------------------------
CLEANFILES = \
  %LC_REL_NAME%-release.boot \
  %LC_REL_NAME%-release.script \
  %LC_REL_NAME%-release.tar.gz

# --------------------------------------
# Distribution
# --------------------------------------
EXTRA_DIST = \
  $(srcdir)/bootstrap.sh \
  $(builddir)/bin/%LC_REL_NAME%-system.in \
  $(builddir)/tests/atconfig \
  $(builddir)/tests/package.m4 \
  $(TESTSOURCES)

if ERL_DOCGEN
EXTRA_DIST += \
  $(srcdir)/doc/index.html.in
endif #ERL_DOCGEN
