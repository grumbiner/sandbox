      PROGRAM wght
C     Given a thermogynamic predictability curve, create an ice thickness
C       distribution weighting for a net predictability.
      IMPLICIT none

      REAL hmin, hmax, delh
      PARAMETER (hmin = 0.10)
      PARAMETER (hmax = 10.00)
      PARAMETER (delh = 0.10)
      INTEGER nx, ny
      PARAMETER (nx = ((hmax - hmin)/delh) )
      PARAMETER (ny = ((hmax - hmin)/delh) )

      REAL pred(0:nx)
      REAL weight(0:nx)
      CHARACTER*60 fname
      REAL hs, hi, temp, grow
      REAL h, h0
 
      INTEGER i, j
      INTEGER order
      REAL pdf
      pdf(h) = exp(-h/h0)*h**order/h0**(order+1)

C     Read in the predictability in days:
      READ (*,9004) order
 9004 FORMAT (I2)

      DO 1000 i = 0, nx
        READ (*,9001) hs, hi, temp, grow, pred(i) 
        IF (i .EQ. 1) PRINT *,'pred = ',i, pred(i)
 1000 CONTINUE

C     Compute the weights and net predictability for each thickness 
      DO 2000 h0 = 0.10, 5.00, 0.05
        DO 2100 i = 1, nx
          weight(i) = pdf(i*delh) / pred(i)
 2100   CONTINUE
        temp = 0.
        DO 2110 i = 1, nx
          temp = temp + weight(i)
 2110   CONTINUE
        WRITE (*,9002) h0, 1./temp 
 2000 CONTINUE
        
 9001 FORMAT (F6.3, F5.2, F6.1, F6.2,2x, F12.2, F8.3)
 9002 FORMAT (F6.2,1x, F8.3)

      STOP
      END
