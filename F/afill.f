      SUBROUTINE afill(ain, aout, nx, ny)
C     Fill in no data and bad data points with first guesses
      IMPLICIT none

      INTEGER nx, ny
      REAL ain(nx, ny), aout(nx, ny)

      INTEGER i, j
      INTEGER il, ir, jb, jt
      REAL al, ar, ab, at

      DO 1000 j = 1, ny
      DO 1100 i = 1, nx 
        IF (ain(i,j) .LT. 1.28 ) THEN
C         Do nothing
          aout(i,j) = ain(i,j)
        ELSE 
          il = i
          jb = j
          ir = i
          jt = j
 2000     CONTINUE
            il = il-1
            IF (ain(il,j) .GE. 1.28 .AND. il .GT. 1) GO TO 2000
            IF (il .EQ. 1) THEN 
              al = 0.
             ELSE
              al = ain(il, j)
            ENDIF
 2100     CONTINUE
            ir = ir+1
            IF (ain(ir,j) .GE. 1.28 .AND. ir .LT. nx) GO TO 2100
            IF (ir .EQ. nx) THEN 
              ar = 0.
             ELSE
              ar = ain(ir, j)
            ENDIF
 2200     CONTINUE
            jt = jt+1
            IF (ain(i,jt) .GE. 1.28 .AND. jt .LT. ny) GO TO 2200
            IF (jt .EQ. ny) THEN 
              at = 0.
             ELSE
              at = ain(i, jt)
            ENDIF
 2300     CONTINUE
            jb = jb-1
            IF (ain(i,jb) .GE. 1.28 .AND. jb .GT. 1) GO TO 2300
            IF (jb .EQ. 1) THEN 
              at = 0.
             ELSE
              at = ain(i, jb)
            ENDIF
C         Now have values to interpolate between.
          aout(i,j) = 0.5* 
     1    (  ((ir-i)*al+(i-il)*ar)/(ir-il)
     2      +((jt-j)*ab+(j-jb)*at)/(jt-jb) )
       
        ENDIF

 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END
             
          

