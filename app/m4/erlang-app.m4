# AC_ERLANG_MODULES
# -----------------
# Infer the list of Erlang source modules to support the
# 'all' target (build of binaries and documentation).
AC_DEFUN([AC_ERLANG_MODULES],
[AC_REQUIRE([AC_ERLANG_NEED_ERL])[]dnl
AC_ERLANG_SRC_MODS
AC_ERLANG_MODS
AC_ERLANG_COMMA_SEPARATED_MODS
AC_ERLANG_XI_MODS
])

# AC_ERLANG_SRC_MODS
# --------------
# Create a list of Erlang source modules (without .erl extension).
AC_DEFUN([AC_ERLANG_SRC_MODS],
[erlang_source_files=`ls -1 ${srcdir}/src/*.erl`
mods=`for file in ${erlang_source_files}; do \
  basename $file .erl; \
done`
])

# AC_ERLANG_MODS
# --------------
# Provide a space separated list of Erlang source modules.
# This is used to populate @MODULES@ in the makefiles.
AC_DEFUN([AC_ERLANG_MODS],
[modules=`for module in ${mods}; do \
  printf "%s "  $module; \
done | sed 's/.$//'`
AC_SUBST([MODULES], [${modules}])
])

# AC_ERLANG_COMMA_SEPARATED_MODULES
# ---------------------------------
# Provide a list of comma separated Erlang source modules.
# This used to populate modules in the .app file.
AC_DEFUN([AC_ERLANG_COMMA_SEPARATED_MODS],
[comma_separated_modules=`for module in ${mods}; do \
  printf "%s,"  $module; \
done | sed 's/.$//'`
AC_SUBST([COMMA_SEPARATED_MODULES], [${comma_separated_modules}])
])

# AC_ERLANG_XI_MODS
# -----------------
# Provide a list of XML xi elements for Erlang source modules.
# This is used to populate the reference manual documentation.
AC_DEFUN([AC_ERLANG_XI_MODS],
[xi_modules=`for module in ${mods}; do \
  printf "<xi:include href=\\"@TOP_BUILDDIR@/%s.xml\\"/>" $module; \
done`
AC_SUBST([XI_MODULES], [${xi_modules}])
])

# AC_ERLANG_ESCRIPTS
# ------------------
# Infer the list of Erlang escripts.
AC_DEFUN([AC_ERLANG_ESCRIPTS],
[AC_REQUIRE([AC_ERLANG_NEED_ERL])[]dnl
AC_ERLANG_ESCRIPT_SRCS
AC_ERLANG_ESCRIPT_LIST
])

# AC_ERLANG_ESCRIPT_SRCS
# ----------------------
# Create a list of Erlang escript sources (without .in extension).
AC_DEFUN([AC_ERLANG_ESCRIPT_SRCS],
[escript_source_files=`ls -1 ${srcdir}/bin/*.in`
escript_srcs=`for file in ${escript_source_files}; do \
  basename $file .in; \
done`
])

# AC_ERLANG_ESCRIPT_LIST
# ----------------------
# Provide a space separated list of Erlang escripts.
# This is used to populate @ESCRIPTS@ in the makefiles.
AC_DEFUN([AC_ERLANG_ESCRIPT_LIST],
[escript_list=`for escript_src in ${escript_srcs}; do \
  printf "%s "  $escript_src; \
done | sed 's/.$//'`
AC_SUBST([ESCRIPTS], [${escript_list}])
])

# AC_AUTOTEST_FILES
# -----------------
# Infer autotests to support the 'check' target which runs
# autotests.
AC_DEFUN([AC_AUTOTEST_FILES],
[AC_AT_FILELIST
AC_AT_FILES
])

# AC_AT_FILELIST
# --------------
# Create a list ot autotest files (with .at extension).
AC_DEFUN([AC_AT_FILELIST],
[autotest_source_files=`ls -1 ${srcdir}/tests/*.at`
at_filelist=`for file in ${autotest_source_files}; do \
  basename $file; \
done`
])

# AC_AT_FILES
# ------------------
# Provide a list of space separated list of autotest files.
# This is used to populate @ATFILES@ in the makefiles
AC_DEFUN([AC_AT_FILES],
[at_files=`for file in ${at_filelist}; do \
  printf "%s "  $file; \
done | sed 's/.$//'`
AC_SUBST([ATFILES], [${at_files}])
])
