C Basic common module of multi-level pressure coordinate model
C     Planetary parameters
      REAL RADIUS, OMEGA0, F0, FSP01, RGAS, CP, KAPPA, G0
      COMMON/PLPARM/  RADIUS, OMEGA0, F0, FSP01, RGAS, CP, KAPPA, G0
C     Horizontal discretization details (form Sptransforms package)
#include "spgrid.h"
C     Vertical discretization details
C
#ifndef FIXTRUNC
      INTEGER L1
      COMMON/VGRID1/ L1
#endif
      REAL PSURF, DP, HDP, PLV, PHLV, PKLV, PKCHLV
      REAL TSTDLV, ZSTDLV, TREFLV
      COMMON/VGRID2/ PSURF, DP(L1MAX), HDP(L1MAX), PLV(L1MAX),
     1               PHLV(0:L1MAX), PKLV(L1MAX), PKCHLV(L1MAX-1),
     2               TSTDLV(L1MAX), ZSTDLV(L1MAX), TREFLV(L1MAX)
C
      REAL TRFFAC, QHDAMP, UD8FAC, TD8FAC
      REAL DCAP2D, D2DCAP, D2W, D2TT, T2GPCP, NNT2DT, IMPCOR
      COMMON/VGRID3/ TRFFAC, QHDAMP, UD8FAC(0:N1MAX), TD8FAC(0:N1MAX),
     1  DCAP2D(L1MAX,L1MAX-1), D2DCAP(L1MAX-1,L1MAX),
     2  D2W(L1MAX-1,L1MAX), D2TT(L1MAX,L1MAX),
     3  T2GPCP(L1MAX-1,L1MAX), NNT2DT(L1MAX,L1MAX),
     4  IMPCOR(L1MAX,L1MAX,0:N1MAX)
C
