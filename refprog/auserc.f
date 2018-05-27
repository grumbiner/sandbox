      SUBROUTINE auserc(name, au1, n, loc, m)
C     search for a given author
C     Robert Grumbine 27 Sep 1995

      INTEGER n, m, loc(n)
      CHARACTER*24 name, au1(n)
      
      INTEGER i, blank

      blank = INDEX(name, ' ') -1 
      m = 0
      DO 1000 i = 1, n
        IF ( INDEX( au1(i),name(1:blank) ) .NE. 0) THEN
          m = m + 1
          loc(m) = i
        ENDIF
 1000 CONTINUE

      RETURN
      END      
