neptune_ebindir=$(ERLANG_INSTALL_LIB_DIR_neptune)/ebin

EXTRA_DIST = \
  testsuite.at \
  local.at \
  neptune-dialyzer.at \
  neptune-eunit.at \
  neptune-default.at \
  neptune-argument.at \
  $(TESTSUITE) \
  atconfig \
  package.m4

TESTSUITE = $(srcdir)/testsuite

TESTSOURCES = \
  $(srcdir)/local.at \
  $(srcdir)/testsuite.at \
  $(srcdir)/neptune-dialyzer.at \
  $(srcdir)/neptune-eunit.at \
  $(srcdir)/neptune-default.at \
  $(srcdir)/neptune-argument.at

AUTOM4TE = $(SHELL) $(top_srcdir)/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest

check-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' 'DIALYZER=$(DIALYZER)' $(TESTSUITEFLAGS)

installcheck-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' 'AUTOTEST_PATH=$(DESTDIR)$(bindir)' 'DIALYZER=$(DIALYZER)' 'prefix=$(prefix)' 'neptune_ebindir=$(neptune_ebindir)' $(TESTSUITEFLAGS)

clean-local:
	test ! -f '$(TESTSUITE)' || $(SHELL) '$(TESTSUITE)' --clean
	rm -rf atconfig

atconfig: $(top_builddir)/config.status
	cd $(top_builddir) && $(SHELL) ./config.status tests/$@

$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	$(AM_V_GEN) :;{ \
		echo '# Signature pf the current package.' && \
		echo 'm4_define([AT_PACKAGE_NAME], [$(PACKAGE_NAME)])' && \
		echo 'm4_define([AT_PACKAGE_TARNAME], [$(PACKAGE_TARNAME)])' && \
		echo 'm4_define([AT_PACKAGE_VERSION], [$(PACKAGE_VERSION)])' && \
		echo 'm4_define([AT_PACKAGE_STRING], [$(PACKAGE_STRING)])' && \
		echo 'm4_define([AT_PACKAGE_BUGREPORT], [$(PACKAGE_BUGREPORT)])'; \
		echo 'm4_define([AT_PACKAGE_URL], [$(PACKAGE_URL)])'; \
		} >'$(srcdir)/package.m4'

$(TESTSUITE): $(TESTSOURCES) $(srcdir)/package.m4
	$(AM_V_GEN) $(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at; mv $@.tmp $@
