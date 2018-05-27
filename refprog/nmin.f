      SUBROUTINE nmin(ncite, nref, n, loc, m)
C     find papers which have been cited at least the given number of times.
C     Robert Grumbine 1987

      IMPLICIT none

      INTEGER n, m, loc(n), ncite
      INTEGER nref(n)

      INTEGER i

      m = 0
      DO 1000 i = 1, n
        IF (nref(i) .GE. ncite) THEN
          m = m + 1
          loc(m) = i
        ENDIF
 1000 CONTINUE

      RETURN
      END
