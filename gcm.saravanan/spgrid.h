C Basic common module for sptransforms
#ifndef FIXTRUNC
      INTEGER M1, N1, K1, K2, N2
      COMMON/SPGRD1/ M1, N1, K1, K2, N2
#endif
C
      REAL DEGLON, DEGLAT, LAMBDA, PHI, MU, COSPHI, COSINV, G, D
      COMPLEX CIM
      COMMON/SPGRD2/ DEGLON(K1MAX), DEGLAT(K2MAX),
     1 LAMBDA(K1MAX), PHI(K2MAX), MU(K2MAX), COSPHI(K2MAX),
     2 COSINV(K2MAX), G(K2MAX), D(0:M1MAX, 0:N2MAX), CIM(0:M1MAX)
C
      REAL A0, A0Q, A0INV, A0QINV
      COMMON/SPGRD3/ A0, A0Q, A0INV, A0QINV
C
