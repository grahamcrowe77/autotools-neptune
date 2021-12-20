ACLOCAL_AMFLAGS = -I m4

AM_V_ERLC = $(am__v_ERLC_@AM_V@)
am__v_ERLC_ = $(am__v_ERLC_@AM_DEFAULT_V@)
am__v_ERLC_0 = @echo "  ERLC    " $@;
am__v_ERLC_1 = 

neptune_srcdir=$(ERLANG_INSTALL_LIB_DIR_neptune)/src
neptune_ebindir=$(ERLANG_INSTALL_LIB_DIR_neptune)/ebin

dist_neptune_src_DATA = \
  $(neptune_priv_headers) \
  $(neptune_sources)

neptune_priv_headers =

neptune_sources = \
  $(top_srcdir)/src/neptune.erl

neptune_ebin_DATA = \
  $(top_builddir)/ebin/neptune.app \
  $(neptune_beam_files)

neptune_beam_files = \
  $(top_builddir)/ebin/neptune.beam

$(top_builddir)/ebin/%.beam: $(neptune_sources)
	$(AM_V_ERLC)$(ERLC) $(ERLCFLAGS) -I$(top_srcdir)/src -I$(top_srcdir)/include -o $(@D) $<

MOSTLYCLEANFILES = \
  $(neptune_beam_files)
