      SUBROUTINE updatec(conc, a, h, tml, tshal, sshal, nx, ny, 
     1                   hset, weight, errunit)
C     Update concentrations (a), thickness (h) and mixed layer
C       temperatures as indicated by the observed concentrations.
C     conc is the observed concentration, a is model concentration
C     Write out the delta to errunit.
C     Robert Grumbine
C     Last Modified 26 April 1996
C     If final concentration less than MINCONC, set to zero.
C     Modified 3 March 1999

      IMPLICIT none
      INCLUDE "icegrid.inc"

      INTEGER i, j, nx, ny, errunit
      REAL conc(nx, ny), a(nx, ny), h(nx, ny), tml(nx, ny)
      REAL tshal(nx, ny), sshal(nx, ny), tfreez
      REAL hset, weight, tempor(LP, MP)
      REAL MINCONC, MINTHICK, MINDEL
      PARAMETER (MINCONC  = 0.15)
      PARAMETER (MINTHICK = 0.05)
      PARAMETER (MINDEL   = 0.75)
      REAL tcrit !delta temperature above which we'll ignore alleged ice
      PARAMETER (tcrit = 4. + 1.84)
      
      DO 1000 j = 1, ny
      DO 1100 i = 1, nx

        IF (conc(i,j) .LT. 128./100.) THEN
          tempor(i,j) = a(i,j) - conc(i,j)
        ELSE
          tempor(i,j) = 0.
        ENDIF

        IF ( conc(i,j) .NE. 0. .AND. conc(i,j) .LT. 128./100. .AND. 
     1     a(i,j) .NE. 0. .AND. conc(i,j) .NE. a(i,j) ) THEN
           IF (ABS(a(i,j) - conc(i,j)) .GT. MINDEL) THEN
               WRITE (*,9001) 1, i, j, conc(i,j), a(i,j), tempor(i,j), 
     1            h(i,j)
           ENDIF
          a(i,j) = (1. - weight)*a(i,j) + weight * MIN(1.0,conc(i,j))

        ELSE IF ( conc(i,j) .GE. MINCONC .AND. conc(i,j) .LT. 128./100.
     1      .AND.  a(i,j) .EQ. 0. 
     2      .AND. (tshal(i,j)-tfreez(sshal(i,j))) .LT. tcrit ) THEN
           IF ( (conc(i,j) - a(i,j)) .GT. MINDEL) THEN
             WRITE (*,9001) 2, i, j, conc(i,j), a(i,j), tempor(i,j), 
     1            h(i,j), tshal(i,j) - tfreez(sshal(i,j))
           ENDIF
          a(i,j) = (1. - weight)*a(i,j) + weight * MIN(1.0,conc(i,j))
          h(i,j) = hset
C 23 October 2001 -- Set tshal towards freezing if we're creating ice
          tshal(i,j) = (tshal(i,j) + tfreez(sshal(i,j)) )*0.5

        ELSE IF ( conc(i,j) .LT. MINCONC .AND. 
     1               a(i,j) .GT. MINCONC)        THEN
           IF ((a(i,j) - conc(i,j)) .GT. MINDEL) THEN
          WRITE (*,9001) 3, i, j, conc(i,j), a(i,j), tempor(i,j),
     1        h(i,j), tshal(i,j) - tfreez(sshal(i,j))
          ENDIF
          a(i,j) = (1. - weight)*a(i,j) + weight * MIN(1.0,conc(i,j))
C 23 October 2001 -- reset the ocean temperature to warmer than freezing
          tshal(i,j) = AMIN1(tshal(i,j), tfreez(sshal(i,j)) + 0.01)
        ELSE
CD          PRINT *,'cupdate type= 4 ',i,j
        ENDIF

        IF (a(i,j) .LT. MINCONC) THEN 
          a(i,j) = 0.0
          h(i,j) = 0.0
        ENDIF
        IF (h(i,j) .LT. MINTHICK) THEN 
          a(i,j) = 0.0
          h(i,j) = 0.0
        ENDIF
        IF (a(i,j) .GT. 1.00) a(i,j) = 1.0

 1100 CONTINUE
 1000 CONTINUE

 9001 FORMAT ('cupdate type= ',I2, 2x, 2I4, 2F5.2, F6.2, 1x, F6.2, 
     1            2x, F6.2)

      WRITE (errunit) tempor

      RETURN
      END      
