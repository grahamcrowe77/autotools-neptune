SUBDIRS = . tests

neptune_srcdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/src
neptune_incdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/include
neptune_ebindir=$(ERLANG_INSTALL_LIB_DIR_neptune)/ebin
neptune_privdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/priv

dist_bin_SCRIPTS = \
  bin/neptune

dist_man1_MANS = \
  man/neptune.1

dist_neptune_src_DATA = \
  $(neptune_priv_headers) \
  $(neptune_sources)

neptune_priv_headers =

neptune_sources = \
  $(srcdir)/src/neptune.erl

dist_neptune_inc_DATA = \
  $(neptune_pub_headers)

neptune_pub_headers = \
  include/neptune.hrl

neptune_ebin_DATA = \
  ebin/neptune.app \
  ebin/neptune.beam

dist_neptune_priv_DATA =

ebin/%.beam: $(neptune_sources) ebin
	$(ERLC) $(ERLCFLAGS) -I$(top_srcdir)/src -I$(top_srcdir)/include -o ebin $<

dialyzer.plt:
	$(DIALYZER) --build_plt --output_plt $@ --apps $(DIALYZER_APPS)

check-local: dialyzer.plt
	$(DIALYZER) -Wno_behaviours -Wno_fail_call --plts $< -- $(neptune_sources)

uninstall-local:
	rm -f  $(DESTDIR)$(bindir)/neptune_test

MOSTLYCLEANFILES = \
  $(addsuffix .beam, $(test_modules)) \
  $(neptune_ebin_DATA)

CLEANFILES = \
  dialyzer.plt

DIALYZER_APPS = \
  erts \
  kernel \
  stdlib \
  compiler \
  crypto
