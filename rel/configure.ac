AC_PREREQ([2.69])
AC_INIT([%LC_REL_NAME%], [%REL_VERSION%], [%EMAIL%])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_SRCDIR([bin/%LC_REL_NAME%-system.sh.in])
AM_INIT_AUTOMAKE([foreign subdir-objects])
AM_SILENT_RULES([yes])

AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_TESTDIR([tests])

# Checks for programs.
AC_ERLANG_NEED_ERL
AC_PROG_MKDIR_P
AC_PATH_PROG(DIALYZER, dialyzer)
AC_PATH_PROG(XSLTPROC, xsltproc)
AC_PATH_PROG(FOP, fop)
AC_PATH_PROG(SED, sed)
AC_PROG_MKDIR_P
AC_PROG_LN_S

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

## Installation dependent data
ROOTDIR=`pwd`
AC_SUBST([ROOTDIR])

# Infer autotest modules
AC_AUTOTEST_FILES

AM_CONDITIONAL([ERL_DOCGEN],
  [test "x${ERLANG_LIB_DIR_erl_docgen}" != "xnot found"])

AM_CONDITIONAL([XSLTPROC], [test "x${XSLTPROC}" != "x"])
if test "x${XSLTPROC}" == "x"; then
  AC_MSG_WARN([xsltproc was not found! Documentation build disabled])
fi

AM_CONDITIONAL([FOP], [test "x${FOP}" != "x"])
if test "x${FOP}" == "x"; then
  AC_MSG_WARN([fop was not found! PDF Documentation build disabled])
fi

AC_CONFIG_FILES([Makefile %LC_REL_NAME%.rel])
AC_CONFIG_FILES([tests/atlocal])
AC_CONFIG_FILES([bin/%LC_REL_NAME%-system.sh], [chmod +x bin/%LC_REL_NAME%-system.sh])
AC_CONFIG_FILES([doc/index.html])
AC_OUTPUT
cat << EOF
----------------------------------------------------------------------

${PACKAGE_NAME} version ${PACKAGE_VERSION}

Prefix: '${prefix}'

Now type 'make @<:@<target>@:>@'
  where the optional argument <target> is:
    all                       - build all binaries
    check @<:@TESTSUITEFLAGS=-v@:>@ - run all tests @<:@verbose@:>@
    install                   - install everything
    installcheck              - test installation
----------------------------------------------------------------------
EOF
