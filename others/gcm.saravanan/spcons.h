C Sptransforms package version 3.3
C Parameter definition module
#define SPCONS 1
#
#ifdef DPRECISION
#   define REAL8 REAL
#else
#   define REAL8 REAL*8
#endif
#
#ifndef N1MAX
#   define N1MAX 21
#endif
#ifndef M1MAX
#   define M1MAX N1MAX
#endif
#ifndef K1MAX
#   define K1MAX 64
#endif
#ifndef K2MAX
#   define K2MAX 33
#endif
#ifndef MLOW
#   define MLOW 4
#endif
#
#define N2MAX (N1MAX+1)
#
#ifdef FIXTRUNC
#   define N1 N1MAX
#   define M1 M1MAX
#   define K1 K1MAX
#   define K2 K2MAX
#   define N2 N2MAX
#endif
C
