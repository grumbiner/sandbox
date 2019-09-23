C subjohng.f
      SUBROUTINE get7 (x,y,z,i) 

      INTEGER i

      REAL  xt,yt,zt
      REAL  x(24),y(24),z(24)

      OPEN (16,FILE="mse.9703.168",FORM="FORMATTED",STATUS="OLD")
      i = 0.

 1006 CONTINUE
        READ (16, * ,END = 2006) zt,xt,yt
        i = i + 1
        x(i) = xt
        y(i) = yt
        z(i) = zt
      GO TO 1006
 2006 CONTINUE

      PRINT *,"i = ",i

      RETURN
      END
