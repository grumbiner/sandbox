      PROGRAM todoodson
C Given a frequency in cycles per day, examine it for doodson-like numbers:
      IMPLICIT none
      INTEGER i, j, k, d1, d2, d3
      INTEGER ibest, jbest, d1best, d2best
      DOUBLE PRECISION in, residual, res1
      LOGICAL good
      REAL quality 

      READ (*,*) in
      d1 = 0
      d2 = 0

      DO i = 1, 9
        d1best = 0
        d2best = 0
        ibest = 0
        jbest = 0
        residual = in
        CALL trial(residual, i, d1, res1, good)
        IF (good ) THEN
          ibest = i
          d1best = d1
          !PRINT *,i,residual, d1, res1, res1/residual
          residual = res1
          PRINT *,1/in, 'found ',d1best,' * ',ibest,"  ",
     1                     d2best," * ",jbest,
     2             quality(in, d1best, ibest, d2best, jbest) 
          DO j = i+1, 9
            CALL trial(residual, j, d2, res1, good)
            IF (good ) THEN
              !PRINT *,"  ",j,residual, d2, res1, res1/residual
              jbest = j
              d2best = d2
              PRINT *,1/in, 'found ',d1best,' * ',ibest,"  ",
     1                         d2best," * ",jbest,
     2             quality(in, d1best, ibest, d2best, jbest) 
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      

      END
