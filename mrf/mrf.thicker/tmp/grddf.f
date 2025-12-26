      SUBROUTINE GRDDF
      PARAMETER(NTYPE=11,NGRID=22)
      COMMON /COMGDF/ DFK(NGRID,NTYPE)
      COMMON /COMGKT/ KTK(NGRID,NTYPE)
      REAL KTK
      COMMON /COMGDFKT/ B(NTYPE),SATPSI(NTYPE),SATKT(NTYPE),TSAT(NTYPE)
      DO K = 1, NTYPE
        DYNW = TSAT(K) * .05
        F1 = B(K) * SATKT(K) * SATPSI(K) / TSAT(K) ** (B(K) + 3.)
        F2 = SATKT(K) / TSAT(K) ** (B(K) * 2. + 3.)
C
C  CONVERT FROM M/S TO KG M-2 S-1 UNIT
C
        F1 = F1 * 1000.
        F2 = F2 * 1000.
        DO I = 1, NGRID
          THETA = FLOAT(I-1) * DYNW
          THETA = MIN(TSAT(K),THETA)
          DFK(I,K) = F1 * THETA ** (B(K) + 2.)
          KTK(I,K) = F2 * THETA ** (B(K) * 2. + 3.)
        ENDDO
      ENDDO
      RETURN
      END
