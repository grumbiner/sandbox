C Parameter definition module
#ifndef SPCONS
#   include "spcons.h"
#endif
#define MCONS 1
#
#ifndef L1MAX
#   define L1MAX 20
#endif
#
#ifndef NTRACE
#   define NTRACE 0
#endif
#
#define NPGQ (3+NTRACE)
#
#ifdef FIXTRUNC
#   define L1 L1MAX
#endif
#
C     "Enumeration literals"
#define JVOR 1
#define JDIV 2
#define JPOT 3
#define JTR1 4
#define JX 1
#define JY 2
#
C
