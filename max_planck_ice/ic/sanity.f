      SUBROUTINE sanity(tml, sml, H, A, tshal, sshal, tdeep, sdeep, 
     1  qtb, qsb, qhb, qdt, qds, nx, ny)
C     General sanity checking of ice and mixed layer behavior.
C     qtb, qsb, qhb are the second layer temperature, salinity, depth
C     qdt, qds are the thermocline and halocline thickness
C     Robert Grumbine 18 October 1996
C     NANtest Robert Grumbine 30 July 2002

      IMPLICIT none

      INTEGER nx, ny, i, j
      REAL tml(nx, ny), sml(nx, ny)
      REAL h(nx, ny), a(nx, ny)
      REAL tshal(nx, ny), tdeep(nx, ny) 
      REAL sshal(nx, ny), sdeep(nx, ny) 
      REAL qtb(nx, ny), qsb(nx, ny), qhb(nx, ny)
      REAL qdt(nx, ny), qds(nx, ny)
      REAL tfreez

C     Change bounds to avoid perimeter points, which are being set to 
C       zero in the model.  They shouldn't be used for anything physical
C       anyhow.  Robert Grumbine 28 February 1997.
      DO 1000 j = 2, ny-1
      DO 1000 i = 2, nx-1
        IF (a(i,j) .GT. 1.0 ) a(i,j) = 1.0
        IF (sml(i,j) .LT. 0.) sml(i,j) = 0.
        IF (tml(i,j) .LT. tfreez(sml(i,j)) ) tml(i,j) = 
     1       tfreez(sml(i,j))
        IF (qdt(i,j) .LT. 0.) qdt(i,j) = 1.E-3
        IF (qds(i,j) .LT. 0.) qds(i,j) = 1.E-3
        IF (qhb(i,j) .LT. 0.) qhb(i,j) = 1.E-3

        IF ( (ABS(tshal(i,j) - tml(i,j) ) .GT.  11.) .OR. 
     1       (ABS(sshal(i,j) - sml(i,j) ) .GT.  5.)     ) THEN
CD2          WRITE (*,9001) i, j, tml(i,j), tshal(i,j), 
CD2     1                         sml(i,j), sshal(i,j)
          tml(i,j) = tshal(i,j)
          sml(i,j) = sshal(i,j)
        ENDIF

        IF ( (ABS(tdeep(i,j) - qtb(i,j) ) .GT.  10.) .OR. 
     1       (ABS(sdeep(i,j) - qsb(i,j) ) .GT.  5.)     ) THEN
CD2          WRITE (*,9003) i, j, qtb(i,j), tdeep(i,j), 
CD2     1                         qsb(i,j), sdeep(i,j)
          qtb(i,j) = tdeep(i,j)
          qsb(i,j) = sdeep(i,j)
        ENDIF

        IF ( h(i,j) .GT. 10.0 ) THEN
          WRITE (*,9002) i, j, h(i,j), tml(i,j)
          tml(i,j) = tfreez(sml(i,j))
          h(i,j) = 9.0
        ENDIF

C  14 October 2003
        IF ( h(i,j) .GT. 0. .AND. a(i,j) .LT. 0.15) THEN
          h(i,j) = 0.0;
          a(i,j) = 0.0;
        ENDIF

 1000 CONTINUE

 9001 FORMAT ('ml shal update ',2I5, 4F9.2)
 9003 FORMAT ('ml deep update ',2I5, 4F9.2)
 9002 FORMAT ('h update ', 2I5, 4F9.2)

C     Now, for final insurance against pathologies, test against NANs
C     Robert Grumbine 30 July 2002
      CALL nantest(tml, nx, ny)
      CALL nantest(sml, nx, ny)
      CALL nantest(h, nx, ny)
      CALL nantest(a, nx, ny)
      CALL nantest(tshal, nx, ny)
      CALL nantest(tdeep, nx, ny)
      CALL nantest(sshal, nx, ny)
      CALL nantest(sdeep, nx, ny)
      CALL nantest(qtb, nx, ny)
      CALL nantest(qsb, nx, ny)
      CALL nantest(qhb, nx, ny)
      CALL nantest(qdt, nx, ny)
      CALL nantest(qds, nx, ny)

      RETURN
      END

      SUBROUTINE nantest(x, nx, ny) 
      INTEGER nx, ny
      REAL x(nx, ny)
      INTEGER i, j
      DO j = 1, ny
      DO i = 1, nx
        IF (.NOT. ( (x(i,j) .GT. 0) .OR. (x(i,j) .LE. 0) ) ) THEN 
          x(i,j) = 0.0        
CBG          PRINT *,'nan found at ',i,j
        ENDIF
      ENDDO
      ENDDO
      RETURN
      END
