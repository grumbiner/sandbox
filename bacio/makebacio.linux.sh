#!/bin/sh
###############################################################
#
#   AUTHOR:    Gilbert - W/NP11
#              Grumbine - W/NP21
#
#   DATE:      01/11/1999
#              5/16/2002
#
#   PURPOSE:   This script uses the make utility to update the bacio 
#              archive libraries.
#
###############################################################

#
#     Remove make file, if it exists.  May need a new make file
#
if [ -f make.bacio ] 
then
  rm -f make.bacio
fi
#
#     Generate a make file ( make.bacio) from this HERE file.
#
cat > make.bacio << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( bacio.v1.3.o baciof.o )

\$(LIB)(bacio.v1.3.o):       bacio.v1.3.c \$(INC)
	ln -sf \$(INC) clib.h
	$(CC) -c \$(CFLAGS) bacio.v1.3.c
	ar -rv \$(AFLAGS) \$(LIB) bacio.v1.3.o
	rm clib.h

\$(LIB)(baciof.o):   baciof.f
	f77 -c \$(FFLAGS) baciof.f
	ar -rv \$(AFLAGS) \$(LIB) baciof.o
	rm -f baciof.o

EOF
#
#     Make Linux version
#
export LIB="../bacio_4"
export INC="/migr/people/wd21rg/includes/clib4.h"
export FFLAGS=" -O3 "
export AFLAGS=" "
export CFLAGS=" -O3 "
make -f make.bacio

#rm -f make.bacio
