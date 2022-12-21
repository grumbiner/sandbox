C stat.ana.f
      PROGRAM AB test
      IMPLICIT NONE
 
      REAL X(20),Y(20),W(20),Z(20)
      REAL A(7)
      INTEGER n,i

      CALL GETXYWZ (X,Y,W,Z,n)

      DO 1000 i = 1,7
         A(i) = ((X(i) - Y(i))*2.645751311)/
     1 (SQRT ((W(i)**2) + (Z(i)**2)))
      WRITE (15,8001) A(i)
 8001 FORMAT (F8.4)
 1000 CONTINUE

      END

