C subjohnf.f
      SUBROUTINE get6 (r,s,u,i) 

      INTEGER i

      REAL  rt,st,ut
      REAL  r(24),s(24),u(24)

      OPEN (15,FILE="mse.9703.144",FORM="FORMATTED",STATUS="OLD")
      i = 0.

 1005 CONTINUE
        READ (15, * ,END = 2005) ut,rt,st
        i = i + 1
        r(i) = rt
        s(i) = st
        u(i) = ut
      GO TO 1005
 2005 CONTINUE

      PRINT *,"i = ",i

      RETURN
      END
