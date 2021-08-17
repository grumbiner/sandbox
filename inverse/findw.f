      SUBROUTINE findw(a0, w, npts)
C     Find the weighting function for a0
C     Variant to treat the n-dimensional (usually 2 or 3) 
C       array a0 as a vector so as to avoid multiplicities of
C       functions
C     Robert Grumbine 5 August 1998

      INTEGER npts
      REAL a0(npts), w(npts)
      
      INTEGER i

      DO 1000 i = 1, npts
        IF (a0(i) .GT. 1.28) THEN
            a0(i) = 0.0
            w(i) = 0.0
        ELSE IF (a0(i) .GT. 1.00) THEN
          a0(i) = 1.0
          w(i) = 1.0
        ELSE
          w(i) = 1.0
        ENDIF
 1000 CONTINUE

      RETURN
      END
