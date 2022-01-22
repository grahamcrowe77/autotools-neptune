# AC_ERLANG_MODULES
# -----------------
AC_DEFUN([AC_ERLANG_MODULES],
[AC_REQUIRE([AC_ERLANG_NEED_ERL])[]dnl
AC_ERLANG_SRC_MODS
AC_ERLANG_MODS
AC_ERLANG_COMMA_SEPARATED_MODS
AC_ERLANG_XI_MODS])

# AC_ERLANG_SRC_MODS
# --------------
# Return a list of Erlang source modules
AC_DEFUN([AC_ERLANG_SRC_MODS],
[erlang_source_files=`ls -1 ${srcdir}/src/*.erl`
mods=`for file in ${erlang_source_files}; do \
  basename $file .erl; \
done`
])

# AC_ERLANG_MODS
# --------------
# Convert a list of Erlang source modules to a space separated list of modules.
# This is used to populate modules in the makefiles
AC_DEFUN([AC_ERLANG_MODS],
[modules=`for module in ${mods}; do \
  printf "%s "  $module; \
done | sed 's/.$//'`
AC_SUBST([MODULES], [${modules}])
])

# AC_ERLANG_COMMA_SEPARATED_MODULES
# ---------------------------------
# Convert a list of Erlang source modules to a comma separated list of modules.
# This used to populate modules in the .app file
AC_DEFUN([AC_ERLANG_COMMA_SEPARATED_MODS],
[comma_separated_modules=`for module in ${mods}; do \
  printf "%s,"  $module; \
done | sed 's/.$//'`
AC_SUBST([COMMA_SEPARATED_MODULES], [${comma_separated_modules}])
])

# AC_ERLANG_XI_MODS
# -----------------
# Convert a list of Erlang source modules to a list of XML xi elements.
# This is used to populate the reference manual documentation.
AC_DEFUN([AC_ERLANG_XI_MODS],
[xi_modules=`for module in ${mods}; do \
  printf "<xi:include href=\\"@TOP_BUILDDIR@/doc/%s.xml\\"/>" $module; \
done`
AC_SUBST([XI_MODULES], [${xi_modules}])
])
