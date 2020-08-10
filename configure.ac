#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
# To produce files: configure, Makefiles and Scripts
#    $ automake
#    $ autoconf
#
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([rama2met], [v2.0], [agustin@atmosfera.unam.mx],[rama2met],[https://github.com/JoseAgustin/rama2met])
AC_CONFIG_AUX_DIR(autotools)
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([1.11 foreign subdir-objects])
# Initialize the test suite.
AC_CONFIG_TESTDIR([testsuite])
AM_MISSING_PROG([AUTOM4TE], [autom4te])

# Set default prefix (where directory bin is created)
# This is the default top directory of the installation
AC_PREFIX_DEFAULT(`pwd`)

# Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_CC
AC_PROG_CPP


# Set language for configuration checks
AC_LANG(Fortran)
AC_PROG_FC
AC_LANG_PUSH(Fortran)
AC_FC_FREEFORM

AC_LANG_POP(Fortran)

AC_CONFIG_FILES([Makefile testsuite/Makefile])
AC_OUTPUT

# Write configuration on the screen
AC_MSG_NOTICE([---------------------------------------------------------])
AC_MSG_NOTICE([Configuration complete - $PACKAGE_NAME-$PACKAGE_VERSION ])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([Fortran compiler:           FC=$FC])
AC_MSG_NOTICE([Fortran flags:              FCFLAGS=$FCFLAGS])
AC_MSG_NOTICE([Install prefix:             --prefix=$prefix])
AC_MSG_NOTICE([Executables install prefix: --exec_prefix=$exec_prefix])
AC_MSG_NOTICE([Binary directory:           --bindir=$bindir])
AC_MSG_NOTICE([---------------------------------------------------------])
