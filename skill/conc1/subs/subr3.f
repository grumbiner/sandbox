      SUBROUTINE get3(f,g, i)

      REAL f(7),g(7)
      REAL ft,gt
      INTEGER i

      OPEN (99, FILE="ss.south.03", FORM="FORMATTED", STATUS="OLD")

      i = 0
 1000 CONTINUE
        READ (99, *, END=2000) ft,gt
          i = i + 1
          f(i) = ft
          g(i) = gt
        GO TO 1000

 2000 CONTINUE

      PRINT *,"i = ",i

      RETURN
      END
