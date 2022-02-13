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
