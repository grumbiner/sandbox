      SUBROUTINE tidefr( per, k, n, match, tide, toler)
C     Subroutine to find the best fit Doodson # to the input
C       frequency and return (match) whether this is a likely
C       tidal frequency.
C     Robert Grumbine
C     Well prior to 12/1989

      REAL per, tide, toler
      LOGICAL match
      INTEGER n, i, k(n)

C     Tidal frequencies, using the lunar day as the base.
C     Frequencies from Godin.
      DOUBLE PRECISION f(6)

C     Local variables.
      DOUBLE PRECISION pera, remain, x
      INTEGER trrnd

C     Statement function:
      trrnd(x) = INT( DSIGN( DBLE(INT(.5+DABS(x))) , x  ) )
      
      f(1) = 4.0255701D-2 
      f(2) = 1.5250452D-3 
      f(3) = 1.1407955D-4 
      f(4) = 1.2894721D-5 
      f(5) = 6.1290280D-6 
      f(6) = 5.432D-9     

C     Find the Doodson #.
      pera   = DBLE(per)
      k(1)   = trrnd(pera/f(1))
      remain = pera - DBLE(k(1))*f(1)
      DO 1000 i = 2, n
        k(i)   = trrnd(remain/f(i))
        remain = remain - DBLE(k(i))*f(i)
 1000 CONTINUE

C     Now decide if the # is proper.  From Munk and Cartwright 1966, 
C       have that abs(ki) <= 6 for all terms > .0001 * the largest.
      match = .TRUE.
      DO 1010 i = -6, 6
        IF (ABS(k(i)) .GT. 6) 
C       not even marginally near a tidal period.
     1      match = .FALSE.
 1010 CONTINUE

C     Test for the magnitude of the residual
      match = (ABS(remain/f(n)) .LT. toler) 
      IF ( match ) THEN 
        tide = 0.0
        DO 1020 i = 1, n
          tide = tide + SNGL( DBLE(k(i)) * f(i) )
 1020   CONTINUE
       ELSE
        tide = per
      ENDIF

      RETURN

      ENTRY tidmak (per, k, n, match, tide, toler)  
C     Entry to compute the frequency of a tide given the Doodson #.

      f(1) = 4.0255701D-2 
      f(2) = 1.5250452D-3 
      f(3) = 1.1407955D-4 
      f(4) = 1.2894721D-5 
      f(5) = 6.1290280D-6 
      f(6) = 5.432D-9     

      per = 0.0
      DO 2000 i = 1, n
        per = per + SNGL( DBLE(k(i))*f(i) )
 2000 CONTINUE
      tide  =  per
      match = .TRUE.

      RETURN
      END
