      PROGRAM bench1

      IMPLICIT REAL*8 (a-h, k-z)
      INTEGER ticks, n
         
      WRITE (*, 1000)
 1000 FORMAT (' How many times do you want to iterate?')
      READ  (*, 300) n
      WRITE (*, 300) n
CD    WRITE (*, 300) TOOLBX(TICKCOUNT)
      PRINT *,'Start timing'
      s = 0.D0
      DO 180 i = 1, n 

        b = 0.5D0*i
        a = DABS(DSIN(b)-DCOS(b))
        a = DEXP(a+s)
        IF (a+s .LT. 10) GOTO 170
        a = a/2.D0
 170    s = s+1.D0/(DSQRT(a)+2.3**a)
 180  CONTINUE
      WRITE (*, 200) SNGL(s)
 200  FORMAT (1X,1E15.9)
 300  FORMAT (I11)
CD    WRITE (*, 300) TOOLBX(TICKCOUNT)
      PRINT *,'Stop timing'
      
      STOP
      END

