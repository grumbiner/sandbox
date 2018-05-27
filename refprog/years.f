      SUBROUTINE years(ymin, ymax, year, n, loc, m)
C     find papers published between given years.
C     Robert Grumbine 2 May 1995

      IMPLICIT none

      INTEGER ymin, ymax, n, m
      INTEGER year(n), loc(n)

      INTEGER i

      m = 0
      DO 1000 i = 1, n
        IF ( (year(i) .GE. ymin) .AND. (year(i) .LE. ymax) ) THEN
          m = m + 1
          loc(m) = i
        ENDIF
 1000 CONTINUE

      RETURN
      END      
