#
# This file is processed by automake (produces file Makefile.in)
#
TESTS = test1 test2 test3 test4 test5 test6 test7 test8

check_PROGRAMS= test1 test2 test3 test4 test5 test6 test7 test8
FCFLAGS = -I$(top_srcdir)
test1_SOURCES = $(top_builddir)/module_rama2met.F90 test1.F90  
test2_SOURCES = $(top_builddir)/module_rama2met.F90 test2.F90
test3_SOURCES = $(top_builddir)/module_rama2met.F90 test3.F90
test4_SOURCES = $(top_builddir)/module_rama2met.F90 test4.F90
test5_SOURCES = $(top_builddir)/module_rama2met.F90 test5.F90
test6_SOURCES = $(top_builddir)/module_rama2met.F90 test6.F90
test7_SOURCES = $(top_builddir)/module_rama2met.F90 test7.F90
test8_SOURCES = $(top_builddir)/module_rama2met.F90 test8.F90

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod 

# The `:;' works around a Bash 3.2 bug when the output is not writeable.
$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	:;{ \
	echo '# Signature of the current package.' && \
	echo 'm4_define([AT_PACKAGE_NAME],' && \
	echo '  [@PACKAGE_NAME@])' && \
	echo 'm4_define([AT_PACKAGE_TARNAME],' && \
	echo '  [@PACKAGE_TARNAME@])' && \
	echo 'm4_define([AT_PACKAGE_VERSION],' && \
	echo '  [@PACKAGE_VERSION@])' && \
	echo 'm4_define([AT_PACKAGE_STRING],' && \
	echo '  [@PACKAGE_STRING@])' && \
	echo 'm4_define([AT_PACKAGE_BUGREPORT],' && \
	echo '  [@PACKAGE_BUGREPORT@])'; \
	echo 'm4_define([AT_PACKAGE_URL],' && \
	echo '  [@PACKAGE_URL@])'; \
	}>'$(srcdir)/package.m4'

TESTSUITE = $(srcdir)/testsuite
check-local:	atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)
     
installcheck-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' \
	$(TESTSUITEFLAGS)

maintainer-clean-local:
	rm -rf aclocal.m4 configure Makefile.in autotools m4

clean-local:
	test ! -f '$(TESTSUITE)' || \
	$(SHELL) '$(TESTSUITE)' --clean
     
AUTOM4TE = $(SHELL) $(top_srcdir)/autotools/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest
$(TESTSUITE): $(srcdir)/testsuite.at $(srcdir)/package.m4
	$(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at
	mv $@.tmp $@

EXTRA_DIST = testsuite.at package.m4 $(TESTSUITE) atlocal.in

.PHONY: $(EXTRA_DIST)
$(EXTRA_DIST):
