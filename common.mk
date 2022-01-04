MODULES = \
  neptune_app \
  neptune_sup \
  neptune_server \
  neptune \
  neptune_nif

APP_MODULES = $(shell echo $(MODULES) | sed -e 's# \{1\}#, #g')

XI_FUNC = echo -n "\<xi:include href="; echo -n "\\\"$$module.xml\\\""; echo "/\>";
XI_MODULES  = $(shell for module in $(MODULES); do $(XI_FUNC) done)

######################################################################
# Silent builds
######################################################################

AM_V_DOC = $(am__v_DOC_@AM_V@)
am__v_DOC_ = $(am__v_DOC_@AM_DEFAULT_V@)
am__v_DOC_0 = @echo "  GENDOC  " $@;
am__v_DOC_1 = 

AM_V_ERLC = $(am__v_ERLC_@AM_V@)
am__v_ERLC_ = $(am__v_ERLC_@AM_DEFAULT_V@)
am__v_ERLC_0 = @echo "  ERLC    " $@;
am__v_ERLC_1 = 

######################################################################
