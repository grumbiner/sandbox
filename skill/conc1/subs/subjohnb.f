C subjohnb.f
      SUBROUTINE get2 (d,e,f,i) 

      INTEGER i

      REAL  dt,et,ft
      REAL  d(24),e(24),f(24)

      OPEN (11,FILE="mse.9703.48",FORM="FORMATTED",STATUS="OLD")
      i = 0.

 1001 CONTINUE
        READ (11, * ,END = 2001) ft,dt,et
        i = i + 1
        d(i) = dt
        e(i) = et
        f(i) = ft
      GO TO 1001
 2001 CONTINUE


      PRINT *,"i = ",i

      RETURN
      END
