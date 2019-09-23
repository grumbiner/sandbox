      SUBROUTINE get2(d,e, i)

      REAL d(7),e(7)
      REAL dt,et
      INTEGER i

      OPEN (99, FILE="mse.south.03", FORM="FORMATTED", STATUS="OLD")

      i = 0
 1000 CONTINUE
        READ (99, *, END=2000) dt,et
          i = i + 1
          d(i) = dt
          e(i) = et
        GO TO 1000

 2000 CONTINUE

      PRINT *,"i = ",i

      RETURN
      END
