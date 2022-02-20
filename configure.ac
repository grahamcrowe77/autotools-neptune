AC_PREREQ([2.69])
AC_INIT([neptune], [0.1.0], [graham.crowe@telia.com])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_SRCDIR([bin/neptune.in])
AM_INIT_AUTOMAKE([foreign subdir-objects])
AM_SILENT_RULES([yes])

AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_TESTDIR([tests])

# Checks for programs.
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC
AC_PATH_PROG(DIALYZER, dialyzer)
AC_PATH_PROG(CT_RUN, ct_run)
AC_PATH_PROG(XSLTPROC, xsltproc)
AC_PATH_PROG(FOP, fop)
AC_PATH_PROG(SED, sed)

# Checks for libraries.
## Run time and Dialyzer build time dependencies
AC_ERLANG_CHECK_LIB([erts], [],
  [AC_MSG_ERROR([erts was not found!])])
AC_ERLANG_CHECK_LIB([kernel], [],
  [AC_MSG_ERROR([kernel was not found!])])
AC_ERLANG_CHECK_LIB([stdlib], [],
  [AC_MSG_ERROR([stdlib was not found!])])
## Dialyzer build time dependencies
AC_ERLANG_CHECK_LIB([compiler], [],
  [AC_MSG_ERROR([compiler was not found!])])
AC_ERLANG_CHECK_LIB([crypto], [],
  [AC_MSG_ERROR([crypto was not found!])])
## Documentation build time dependencies
AC_ERLANG_CHECK_LIB([erl_docgen], [],
  [AC_MSG_WARN([erl_docgen was not found! Documentation build disabled])])

# Application installation directory
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([neptune], [${PACKAGE_VERSION}])
## Installation dependent data
neptune_sysconfdir="${sysconfdir}/neptune"
AC_SUBST([neptune_sysconfdir])
neptune_ebindir="${ERLANG_INSTALL_LIB_DIR_neptune}/ebin"
AC_SUBST([neptune_ebindir])
## Test dependent data
AC_SUBST([abs_top_builddir])

# Infer Erlang source code modules
AC_ERLANG_MODULES

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

erlang_kernel_maj_ver=`echo $ERLANG_LIB_VER_kernel | cut -d'.' -f1`
if test $erlang_kernel_maj_ver -lt 8; then
  AC_MSG_WARN([Erlang/OTP kernel version < 8.0! Common test suite won't work])
fi

AC_CONFIG_FILES([Makefile ebin/neptune.app])
AC_CONFIG_FILES([tests/atlocal tests/ct-config doc/ref_man.xml.in])
AC_CONFIG_FILES([bin/neptune], [chmod +x bin/neptune])
AC_OUTPUT
cat << EOF
----------------------------------------------------------------------

${PACKAGE_NAME} Version ${PACKAGE_VERSION}

Prefix: '${prefix}'

Now type 'make @<:@<target>@:>@'
  where the optional argument <target> is:
    all                       - build all binaries and documentation
    check @<:@TESTSUITEFLAGS=-v@:>@ - run all tests @<:@verbose@:>@
    install                   - install everything
    installcheck              - test installation
----------------------------------------------------------------------
EOF
