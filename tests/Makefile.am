ACLOCAL_AMFLAGS = -I m4

######################################################################
# Autotest testcases
######################################################################

TESTSOURCES = @TESTSOURCES@

######################################################################

TESTSUITE = $(srcdir)/testsuite

AUTOM4TE = $(SHELL) $(top_srcdir)/build-aux/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest

######################################################################
# Static Analysis (Dialyzer)
######################################################################
DIALYZER_APPS = \
  erts \
  kernel \
  stdlib \
  compiler \
  crypto

neptune.plt:
	$(AM_V_GEN)$(DIALYZER) --build_plt --output_plt $@ \
          --apps $(DIALYZER_APPS)

######################################################################

check-local: atconfig atlocal $(TESTSUITE) neptune.plt
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)

installcheck-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' \
           'AUTOTEST_PATH=$(DESTDIR)$(bindir)' \
           $(TESTSUITEFLAGS)

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
	$(AM_V_GEN) $(AUTOTEST) -I '$(srcdir)' \
          -o $@.tmp $@.at; mv $@.tmp $@

MOSTLYCLEANFILES = \
  $(addsuffix .beam, $(test_modules)) \
  $(neptune_ebin_DATA)

CLEANFILES = \
  neptune.plt

EXTRA_DIST = \
  atconfig \
  package.m4 \
  $(TESTSOURCES) \
  $(TESTSUITE)
