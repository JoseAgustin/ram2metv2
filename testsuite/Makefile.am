#
# This file is processed by automake (produces file Makefile.in)
#
AUTOMAKE_OPTIONS = subdir-objects
TEST = test1 test2 test3 test4 test5 test6 test7 test8

check_PROGRAMS= test1 test2 test3 test4 test5 test6 test7 test8

test1_SOURCES = $(top_builddir)/module_rama2met.F90 test1.F90
test2_SOURCES = $(top_builddir)/module_rama2met.F90 test2.F90
test3_SOURCES = $(top_builddir)/module_rama2met.F90 test3.F90
test4_SOURCES = $(top_builddir)/module_rama2met.F90 test4.F90
test5_SOURCES = $(top_builddir)/module_rama2met.F90 test5.F90
test6_SOURCES = $(top_builddir)/module_rama2met.F90 test6.F90
test7_SOURCES = $(top_builddir)/module_rama2met.F90 test7.F90
test8_SOURCES = $(top_builddir)/module_rama2met.F90 test8.F90

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
check-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)
     
installcheck-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' \
	$(TESTSUITEFLAGS)

clean-local:
	test ! -f '$(TESTSUITE)' || \
	$(SHELL) '$(TESTSUITE)' --clean
     
AUTOM4TE = $(SHELL) $(srcdir)/build-aux/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest
$(TESTSUITE): $(srcdir)/testsuite.at $(srcdir)/package.m4
	$(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at
	mv $@.tmp $@

EXTRA_DIST = README.md $(TESTSUITE)

.PHONY: $(EXTRA_DIST)
$(EXTRA_DIST):
