AC_PREREQ([2.69])
AC_INIT([%LC_REL_NAME%], [%REL_VERSION%], [%EMAIL%])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_SRCDIR([%LC_REL_NAME%.rel.in])
AM_INIT_AUTOMAKE([foreign subdir-objects])
AM_SILENT_RULES([yes])

AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_TESTDIR([tests])
AC_CONFIG_FILES([tests/Makefile tests/atlocal])

# Checks for programs.
AC_ERLANG_NEED_ERL
AC_PROG_MKDIR_P

# Checks for libraries.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_ERTS_VER
erlang_erts_maj_ver=`echo $ERLANG_ERTS_VER | cut -d'.' -f1`
if test ${erlang_erts_maj_ver} -lt %ERLANG_ERTS_VER%; then
  AC_MSG_ERROR([Erlang/OTP version must >= %ERLANG_ERTS_VER%])
fi
## Run time and Dialyzer build time dependencies
AC_ERLANG_CHECK_LIB([kernel], [],
  [AC_MSG_ERROR([kernel was not found!])])
AC_ERLANG_CHECK_LIB([stdlib], [],
  [AC_MSG_ERROR([stdlib was not found!])])
AC_ERLANG_CHECK_LIB([sasl], [],
  [AC_MSG_ERROR([sasl was not found!])])
AC_ERLANG_CHECK_LIB([%LC_APP_NAME%], [],
  [AC_MSG_ERROR([%LC_APP_NAME% was not found!])])
## Dialyzer build time dependencies
AC_ERLANG_CHECK_LIB([compiler], [],
  [AC_MSG_ERROR([compiler was not found!])])
AC_ERLANG_CHECK_LIB([crypto], [],
  [AC_MSG_ERROR([crypto was not found!])])
## Documentation build time dependencies
AC_ERLANG_CHECK_LIB([erl_docgen], [],
  [AC_MSG_WARN([erl_docgen was not found! Documentation build disabled])])

if test "x${prefix}" == "xNONE"; then
  prefix=/usr/local
fi
exec_prefix="${prefix}"
bindir="${exec_prefix}/bin"
pkgdatadir="${bindir}/${REL_NAME}"
AC_SUBST([pkgdatadir])

AC_CONFIG_FILES([Makefile ${REL_NAME}.rel])
AC_CONFIG_FILES([bin/%LC_REL_NAME%-rel.sh], [chmod +x bin/%LC_REL_NAME%-rel.sh])
AC_OUTPUT
cat << EOF
----------------------------------------------------------------------

${REL_NAME} Version ${REL_VERSION}

Prefix: '${prefix}'

Now type 'make @<:@<target>@:>@'
  where the optional argument <target> is:
    all                       - build all binaries
    check @<:@TESTSUITEFLAGS=-v@:>@ - run all tests @<:@verbose@:>@
    install                   - install everything
    installcheck              - test installation
----------------------------------------------------------------------
EOF
