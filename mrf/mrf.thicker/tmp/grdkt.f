      SUBROUTINE GRDKT
      PARAMETER(NTYPE=11,NGRID=22)
      COMMON /COMGDFT/ DFKT(NGRID,NTYPE)
      COMMON /COMGDFKT/ B(NTYPE),SATPSI(NTYPE),SATKT(NTYPE),TSAT(NTYPE)
      DO K = 1, NTYPE
        DYNW = TSAT(K) * .05
        F1 = LOG10(SATPSI(K)) + B(K) * LOG10(TSAT(K)) + 2.
        DO I = 1, NGRID
          THETA = FLOAT(I-1) * DYNW
          THETA = MIN(TSAT(K),THETA)
          IF(THETA.GT.0.) THEN
            PF = F1 - B(K) * LOG10(THETA)
          ELSE
            PF = 5.2
          ENDIF
          IF(PF.LE.5.1) THEN
            DFKT(I,K) = EXP(-(2.7+PF)) * 420.
          ELSE
            DFKT(I,K) = .1744
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END
