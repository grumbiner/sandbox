C subjohnd.f
      SUBROUTINE get4 (k,l,m,i) 

      INTEGER i

      REAL  kt,lt,mt
      REAL  k(24),l(24),m(24)

      OPEN (13,FILE="mse.9703.96",FORM="FORMATTED",STATUS="OLD")
      i = 0.

 1004 CONTINUE
        READ (13, * ,END = 2004) mt,kt,lt
        i = i + 1
        k(i) = kt
        l(i) = lt
        m(i) = mt
      GO TO 1004
 2004 CONTINUE


      PRINT *,"i = ",i

      RETURN
      END
