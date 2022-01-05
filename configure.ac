AC_PREREQ([2.69])
AC_INIT([neptune], [0.5.0], [graham.crowe@telia.com])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_SRCDIR([src/neptune.erl])
AM_INIT_AUTOMAKE([foreign subdir-objects])
AM_SILENT_RULES([yes])
LT_PREREQ([2.4.6])
LT_INIT

AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_TESTDIR([tests])
AC_CONFIG_FILES([tests/Makefile tests/atlocal])

# Checks for programs.
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC
AC_PATH_PROG(DIALYZER, dialyzer)
AC_PATH_PROG(CT_RUN, ct_run)
AC_PATH_PROG(ESCRIPT, escript)
AC_PATH_PROG(XSLTPROC, xsltproc)
AC_PATH_PROG(FOP, fop)
AC_PROG_MKDIR_P
AC_PROG_CC

# Checks for libraries.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_ERTS_VER
erlang_erts_maj_ver=`echo $ERLANG_ERTS_VER | cut -d'.' -f1`
if test ${erlang_erts_maj_ver} -lt 10; then
  AC_MSG_ERROR([Erlang/OTP version must >= OTP-22.0!])
fi
AC_ERLANG_CHECK_LIB([erts], [],
  [AC_MSG_ERROR([erts was not found!])])
AC_ERLANG_CHECK_LIB([kernel], [],
  [AC_MSG_ERROR([kernel was not found!])])
AC_ERLANG_CHECK_LIB([stdlib], [],
  [AC_MSG_ERROR([stdlib was not found!])])
AC_ERLANG_CHECK_LIB([erl_docgen], [],
  [AC_MSG_WARN([erl_docgen was not found! Documentation build disabled])])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([neptune], [0.1.0])

if test "x${prefix}" == "xNONE"; then
  prefix=/usr/local
fi
exec_prefix="${prefix}"
libdir="${exec_prefix}/lib64"
ERLANG_INSTALL_LIB_DIR="${libdir}/erlang/lib"
ERLANG_INSTALL_LIB_DIR_neptune="${ERLANG_INSTALL_LIB_DIR}/neptune-0.1.0"
neptune_ebindir="${ERLANG_INSTALL_LIB_DIR_neptune}/ebin"
AC_SUBST([neptune_ebindir])

if test "x${XSLTPROC}" == "x"; then
  AC_MSG_WARN([xsltproc was not found! Documentation build disabled])
fi
if test "x${FOP}" == "x"; then
  AC_MSG_WARN([fop was not found! PDF Documentation build disabled])
fi

AC_CONFIG_FILES([Makefile ebin/neptune.app.in doc/Makefile])
AC_OUTPUT
cat << EOF
----------------------------------------------------------------------

${PACKAGE_NAME} Version ${PACKAGE_VERSION}

Prefix: '${prefix}'
Compiler: '${ERLC} ${ERLCFLAGS}'

Now type 'make @<:@<target>@:>@'
  where the optional argument <target> is:
    all                       - build all binaries
    check @<:@TESTSUITEFLAGS=-v@:>@ - run all tests @<:@verbose@:>@
    install                   - install everything
    installcheck              - test installation
----------------------------------------------------------------------
EOF
