      FUNCTION sssum(freq1, freq2, time, n)
C     Set of functions to do quadratic summation for irregularly spaced
C       data.
C     Robert Grumbine before 15 December 1994
C     Minor F90-ish updating -- implicit none, do loops, no 'ENTRY' 7 April 2014
      IMPLICIT none

      REAL sssum
      REAL freq1, freq2
      INTEGER n
      REAL time(n)

      DOUBLE PRECISION sstemp
      INTEGER i

      sstemp = 0.D0
      DO i = 1, n
        sstemp = dsin(DBLE(freq1*time(i))) * dsin(DBLE( freq2*time(i) ))
     1            + sstemp
      ENDDO

      sssum = SNGL(sstemp)
      RETURN
      END
  
      REAL FUNCTION scsum(freq1, freq2, time, n)
      IMPLICIT none
      REAL freq1, freq2
      INTEGER i, n
      REAL time(n)
      DOUBLE PRECISION sctemp

      sctemp = 0.D0
      DO i = 1, n
        sctemp = dsin(DBLE(freq1*time(i))) * dcos(DBLE(freq2*time(i)))
     1             + sctemp
      ENDDO

      scsum = SNGL (sctemp)
      RETURN
      END

      REAL FUNCTION ccsum(freq1, freq2, time, n)
      IMPLICIT none
      REAL freq1, freq2
      INTEGER i, n
      REAL time(n)
      DOUBLE PRECISION cctemp

      cctemp = 0.D0
      DO i = 1, n
        cctemp = dcos(DBLE(freq1*time(i))) * dcos(DBLE(freq2*time(i)))
     1            + cctemp
      ENDDO

      ccsum = SNGL(cctemp)
      RETURN
      END
