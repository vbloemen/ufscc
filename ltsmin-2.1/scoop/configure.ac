#
#
AC_PREREQ(2.60)
AC_INIT([scoop],[1.0],[s.c.c.blom@utwente.nl])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([autotools])
AM_INIT_AUTOMAKE([foreign])

AC_USE_SYSTEM_EXTENSIONS

MAJOR_VERSION=`echo "$VERSION" | sed -e 's/^\([[0-9]]*\)\.[[0-9]]*.*$/\1/'`
MINOR_VERSION=`echo "$VERSION" | sed -e 's/^[[0-9]]*\.\([[0-9]]*\).*$/\1/'`
AC_DEFINE_UNQUOTED([MAJOR_VERSION], [$MAJOR_VERSION], [Scoop's major version.])
AC_DEFINE_UNQUOTED([MINOR_VERSION], [$MINOR_VERSION], [Scoop's minor version.])

AC_PROG_CC

CHECK_GHC

AC_MSG_CHECKING([if dynamic packages are available])
GHCFLAGS="-dynamic"
TRY_COMPILE_GHC([
    main = putStr "Hello world!\n"
],[],
AC_MSG_FAILURE(
"GHC's dynamic packages could not be found. 
Without GHC's dynamic packages neither scoop nor libscoop can be built.
On Ubuntu GHC's dynamic packages are in aptitude's ghc-dynamic package.
Install them before continuing."
))

AC_MSG_CHECKING([if hashmap is available])
TRY_COMPILE_GHC([
    import Data.HashMap
    main = putStr "Hello world!\n"
],[have_hashmap=yes],
                AC_MSG_WARN(scoop will not be built: hashmap not found.))
AM_CONDITIONAL([HAVE_HASHMAP], [test x"$have_hashmap" != "x"])

AC_MSG_CHECKING([if happy is available])
AC_CHECK_PROGS(HAPPY, [happy])
if test x"$HAPPY" = x ; then
AC_MSG_ERROR([Please install happy before trying to build scoop.])
fi

AC_CONFIG_FILES([Makefile src/Makefile])

AC_OUTPUT

