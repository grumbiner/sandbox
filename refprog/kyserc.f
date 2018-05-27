      SUBROUTINE kyserc(wd, title, n, loc, m)
C     Search for a keyword in the title.
      
      INTEGER n, m, loc(n)
      CHARACTER*24 wd
      CHARACTER*196 title(n)
 
      INTEGER i, blank
      
      blank = INDEX(wd,' ') -1 
      m = 0
      DO 1000 i = 1, n
        IF (INDEX(title(i) , wd(1:blank) ) .NE. 0) THEN
          m = m + 1
          loc(m) = i
        ENDIF
 1000 CONTINUE

      RETURN
      END
