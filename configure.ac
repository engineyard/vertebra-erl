# -*- Autoconf -*-
AC_PREREQ(2.61)
AC_INIT(vertebra-erl, 0.4.0)
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_CONFIG_SRCDIR([src/vertebractl.in])

# Standard Programs
AC_PROG_SED

# Erlang
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC
AC_ERLANG_SUBST_INSTALL_LIB_DIR

# Output
AC_CONFIG_FILES([Makefile
                 lib/Makefile
                 lib/entrepot/Makefile
                 lib/entrepot/src/Makefile
                 lib/entrepot/tests/Makefile
                 lib/eye_update/Makefile
                 lib/eye_update/src/Makefile
                 lib/eye_update/tests/Makefile
                 lib/herault/Makefile
                 lib/herault/src/Makefile
                 lib/herault/tests/Makefile
                 lib/vertebra/Makefile
                 lib/vertebra/src/Makefile
                 lib/vertebra/tests/Makefile
                ])
AC_OUTPUT
