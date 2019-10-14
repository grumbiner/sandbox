C subjohne.f
      SUBROUTINE get5 (o,p,q,i) 

      INTEGER i

      REAL  ot,pt,qt
      REAL  o(24),p(24),q(24)

      OPEN (14,FILE="mse.9703.120",FORM="FORMATTED",STATUS="OLD")
      i = 0.

 1004 CONTINUE
        READ (14, * ,END = 2004) qt,ot,pt
        i = i + 1
        o(i) = ot
        p(i) = pt
        q(i) = qt
      GO TO 1004
 2004 CONTINUE

      PRINT *,"i = ",i

      RETURN
      END
