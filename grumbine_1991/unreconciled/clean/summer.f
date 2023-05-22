C***********************************************************----------!!
      SUBROUTINE SUMMER(A, NX, NY, SUM)
C     SUM AN ARRAY
      INTEGER NX, NY, I, J
      REAL A(NX, NY)
      DOUBLE PRECISION SUM

      SUM = 0.D0
      DO 1000 J = 1, NY
        DO 1010 I = 1, NX
             SUM = SUM+DBLE(A(I,J))
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
