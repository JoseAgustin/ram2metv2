#
# This file is processed by automake (produces file Makefile.in)
#
bin_PROGRAMS = rama2met.exe
rama2met_exe_SOURCES = module_rama2met.F90 rama2metv2.F90

TESTSUITE = $(srcdir)/testsuite

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod 

EXTRA_DIST = est_rama.txt namelist.met README.md

SUBDIRS = . testsuite

.PHONY: $(EXTRA_DIST)
$(EXTRA_DIST):
