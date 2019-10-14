      PROGRAM hello
      IMPLICIT none
      REAL fred

      PRINT *,' sum is ', fred(1000)

      END

      FUNCTION fred(n)
      IMPLICIT none
      REAL sum
      REAL fred
      INTEGER i, n      

      DO i = 1,n
        sum = sum + i
      ENDDO

      fred = sum

      RETURN
      END
