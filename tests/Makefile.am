ACLOCAL_AMFLAGS = -I m4

TESTCASES = \
  neptune-dialyzer.at \
  neptune-eunit.at \
  neptune-default.at \
  neptune-argument.at

EXTRA_DIST = \
  testsuite.at \
  local.at \
  atconfig \
  package.m4 \
  $(TESTCASES) \
  $(TESTSUITE)

TESTSUITE = $(srcdir)/testsuite

TESTSOURCES = \
  $(srcdir)/local.at \
  $(srcdir)/testsuite.at \
  $(addprefix $(srcdir)/, $(TESTCASES))

AUTOM4TE = $(SHELL) $(top_srcdir)/build-aux/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest

neptune.plt:
	$(DIALYZER) --build_plt --output_plt $@ --apps $(DIALYZER_APPS)

MOSTLYCLEANFILES = \
  $(addsuffix .beam, $(test_modules)) \
  $(neptune_ebin_DATA)

CLEANFILES = \
  neptune.plt

DIALYZER_APPS = \
  erts \
  kernel \
  stdlib \
  compiler \
  crypto

check-local: atconfig atlocal $(TESTSUITE) neptune.plt
	$(SHELL) '$(TESTSUITE)' \
           'DIALYZER=$(DIALYZER)' $(TESTSUITEFLAGS)

installcheck-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' \
           'AUTOTEST_PATH=$(DESTDIR)$(bindir)' \
           'DIALYZER=$(DIALYZER)' \
           'prefix=$(prefix)' \
           'neptune_ebindir=$(ERLANG_INSTALL_LIB_DIR_neptune)/ebin' $(TESTSUITEFLAGS)

uninstall-local:
	rm -f  $(DESTDIR)$(bindir)/neptune_test

clean-local:
	test ! -f '$(TESTSUITE)' || $(SHELL) '$(TESTSUITE)' --clean
	rm -rf atconfig $(top_builddir)/src/*.beam

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
