#
# This file is processed by automake (produces file Makefile.in)
#
ACLOCAL_AMFLAGS = -I m4

bin_PROGRAMS = rama2met.exe
rama2met_exe_SOURCES = module_rama2met.F90 rama2metv2.F90

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod

# The `:;' works around a Bash 3.2 bug when the output is not writeable.
$(srcdir)/testsuite/package.m4: $(top_srcdir)/configure.ac

TESTSUITE = $(srcdir)/testsuite

SUBDIRS = . testsuite

EXTRA_DIST = est_rama.txt namelist.met README.md

.PHONY: $(EXTRA_DIST)
$(EXTRA_DIST):
