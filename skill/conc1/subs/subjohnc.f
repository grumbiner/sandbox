C subjohnc.f
      SUBROUTINE get3 (g,h,j,i) 

      INTEGER i

      REAL  gt,ht,jt
      REAL  g(24),h(24),j(24)

      OPEN (12,FILE="mse.9703.72",FORM="FORMATTED",STATUS="OLD")
      i = 0.

 1002 CONTINUE
        READ (12, * ,END = 2002) jt,gt,ht
        i = i + 1
        g(i) = gt
        h(i) = ht
        j(i) = jt
      GO TO 1002
 2002 CONTINUE


      PRINT *,"i = ",i

      RETURN
      END
