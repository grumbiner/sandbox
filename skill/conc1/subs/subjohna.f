C subjohna.f
      SUBROUTINE get1 (a,b,c,i) 

      INTEGER i

      REAL  at,bt,ct
      REAL  a(24),b(24),c(24)

      OPEN (10,FILE="mse.9703.24",FORM="FORMATTED",STATUS="OLD")
      i = 0.

 1000 CONTINUE
        READ (10, * ,END = 2000) ct,at,bt
        i = i + 1
        a(i) = at
        b(i) = bt
        c(i) = ct
      GO TO 1000
 2000 CONTINUE


      PRINT *,"i = ",i

      RETURN
      END
