ACLOCAL_AMFLAGS = -I m4

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
  bin/$(PACKAGE_NAME).sh

pkgbindir=$(pkgdatadir)/erts-$(ERLANG_ERTS_VER)/bin

ERTS_MODS = \
  run_erl \
  erl \
  beam.smp \
  epmd \
  erlexec \
  erl_child_setup \
  inet_gethost

ERTS = $(addprefix $(top_builddir)/erts-$(ERLANG_ERTS_VER)/bin/, $(ERTS_MODS))

dist_pkgbin_DATA = \
  $(ERTS) \
  $(top_builddir)/releases/$(PACKAGE_VERSION)/$(PACKAGE_NAME).config

$(top_builddir)/releases/$(PACKAGE_VERSION)/$(PACKAGE_NAME).config: $(PACKAGE_NAME).config
	$(MKDIR_P) $(@D) && cp -p $< $@

$(top_builddir)/erts-$(ERLANG_ERTS_VER)/bin/%: $(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/bin/%
	$(MKDIR_P) $(@D) && cp -p $< $@

all-local: lib

install-data-local:
	$(MKDIR_P) $(DESTDIR)$(pkgdatadir)
	cp -vr lib $(DESTDIR)$(pkgdatadir)
	cp -vr releases $(DESTDIR)$(pkgdatadir)
	cp -v $(PACKAGE_NAME).config \
	  $(DESTDIR)$(pkgdatadir)/releases/$(PACKAGE_VERSION)

install-data-hook:
	chmod +x $(DESTDIR)$(pkgbindir)/*

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

clean-local:
	rm -rf erts-$(ERLANG_ERTS_VER)
	rm -rf releases
	rm -rf lib
