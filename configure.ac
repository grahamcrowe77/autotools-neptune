AC_PREREQ([2.69])
AC_INIT([neptune], [0.8.0], [graham.crowe@telia.com])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_SRCDIR([src/neptune_app.erl])
AM_INIT_AUTOMAKE([foreign subdir-objects])
AM_SILENT_RULES([yes])
LT_PREREQ([2.4.6])
LT_INIT

AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_TESTDIR([tests])

# Checks for programs.
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC
AC_PATH_PROG(DIALYZER, dialyzer)
AC_PATH_PROG(CT_RUN, ct_run)
AC_PATH_PROG(ESCRIPT, escript)
AC_PATH_PROG(XSLTPROC, xsltproc)
AC_PATH_PROG(FOP, fop)
AC_PATH_PROG(SED, sed)
AC_PROG_MKDIR_P
AC_PROG_CC
AC_PROG_LN_S

# Checks for libraries.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_ERTS_VER
erlang_erts_maj_ver=`echo $ERLANG_ERTS_VER | cut -d'.' -f1`
if test ${erlang_erts_maj_ver} -lt 11; then
  AC_MSG_ERROR([Erlang/OTP version must >= OTP-23.0!])
fi
AC_ERLANG_CHECK_LIB([erts], [],
  [AC_MSG_ERROR([erts was not found!])])
AC_ERLANG_CHECK_LIB([kernel], [],
  [AC_MSG_ERROR([kernel was not found!])])
AC_ERLANG_CHECK_LIB([stdlib], [],
  [AC_MSG_ERROR([stdlib was not found!])])
AC_ERLANG_CHECK_LIB([erl_docgen], [],
  [AC_MSG_WARN([erl_docgen was not found! Documentation build disabled])])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([neptune], [${PACKAGE_VERSION}])

neptune_ebindir="${ERLANG_INSTALL_LIB_DIR_neptune}/ebin"
AC_SUBST([neptune_ebindir])

# Infer Erlang source code modules
AC_ERLANG_MODULES

# Infer autotest modules
AC_ERLANG_TESTSOURCES

if test "x${build_vendor}" == "xapple"; then
  AC_MSG_NOTICE([Build target is Apple])
  APPLE=true
fi
AM_CONDITIONAL([APPLE], [test "x${APPLE}" != "x"])

AM_CONDITIONAL([ERL_DOCGEN], [test "x${ERLANG_LIB_DIR_erl_docgen}" != "xnot found"])

AM_CONDITIONAL([XSLTPROC], [test "x${XSLTPROC}" != "x"])
if test "x${XSLTPROC}" == "x"; then
  AC_MSG_WARN([xsltproc was not found! Documentation build disabled])
fi

AM_CONDITIONAL([FOP], [test "x${FOP}" != "x"])
if test "x${FOP}" == "x"; then
  AC_MSG_WARN([fop was not found! PDF Documentation build disabled])
fi

AC_CONFIG_FILES([Makefile ebin/neptune.app tests/atlocal doc/ref_man.xml.in])
AC_OUTPUT
cat << EOF
----------------------------------------------------------------------

${PACKAGE_NAME} Version ${PACKAGE_VERSION}

Prefix: '${prefix}'
Compiler: '${ERLC} ${ERLCFLAGS}'

Now type 'make @<:@<target>@:>@'
  where the optional argument <target> is:
    all                       - build all binaries and documentation
    check @<:@TESTSUITEFLAGS=-v@:>@ - run all tests @<:@verbose@:>@
    install                   - install everything
    installcheck              - test installation
----------------------------------------------------------------------
EOF
