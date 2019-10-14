      SUBROUTINE get1(a,b,c, i)

      REAL a(7),b(7),c(7)
      REAL at,bt,ct
      INTEGER i

      OPEN (99, FILE="corr.south.03", FORM="FORMATTED", STATUS="OLD")

      i = 0
 1000 CONTINUE
        READ (99, *, END=2000) at,bt,ct
          i = i + 1
          a(i) = at
          b(i) = bt
          c(i) = ct
        GO TO 1000

 2000 CONTINUE

      PRINT *,"i = ",i

      RETURN
      END
