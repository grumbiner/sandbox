      SUBROUTINE sanaly(dir1, dir2, ff, dist, scorefile)
C     Conduct simple analyses of displacement fields
C     1- correlation between 1 and 2 displacements
C     2  index of agreement
C     3  vector correlation.
C     Bob Grumbine 21 April 1994.
C     Revised for generality in working with buoys as well as forecast models.
C     Bob Grumbine 10 April 1995.

      IMPLICIT none

      INTEGER nskile, ndays, scorefile
      PARAMETER (nskile = 207)
      PARAMETER (ndays  =   6)

C     Parameters for reading data in
      REAL dir1(nskile, ndays), ff(nskile, ndays)
      REAL dir2(nskile, ndays), dist(nskile, ndays)
      REAL x1(nskile,ndays), y1(nskile,ndays)
      REAL x2(nskile,ndays), y2(nskile,ndays)

      INTEGER i, j
      REAL ia(ndays), r2(ndays), vcor(ndays)
      REAL sk1ia(ndays), sk1r2(ndays), sk1vcor(ndays)
      REAL sk2ia(ndays), sk2r2(ndays), sk2vcor(ndays)
      REAL iagree, rbar
    
CD      PRINT *,' ' 
CD      PRINT *,'Simple correlation analyses - correlation vs. f.day' 
      DO 1000 j = 1, ndays
CD        PRINT *,'Fday = ',j
        ia(j) = iagree(ff(1,j), dist(1,j), nskile)
CD        PRINT *,'back from iagree'
   
        CALL correl(ff(1,j), dist(1,j), nskile, r2(j), 
     1                      rbar, rbar, rbar, rbar)
CD        PRINT *,'back from correl'

        CALL vectorize(ff(1,j), dir1(1,j), x1(1,j), y1(1,j), nskile)
        CALL vectorize(dist(1,j), dir2(1,j), x2(1,j), y2(1,j), nskile)
CD        PRINT *,'back from vectorize'

        CALL vcc(x1(1,j), y1(1,j), x2(1,j), y2(1,j), nskile, vcor(j))
CD        PRINT *,'back from vcc'

        WRITE (scorefile,9002) j, ia(j), r2(j), vcor(j)
 1000 CONTINUE
 9002 FORMAT (I3, 3F6.3)
   
CD      PRINT *,' ' 
CD      PRINT *,'Forecast self-correlation, skiles1      skiles2' 
      DO 2000 i = 1, ndays-1
        DO 2100 j = i+1, ndays
        sk1ia(j) = iagree(ff(1,i), ff(1,j), nskile)
   
        CALL correl(ff(1,i), ff(1,j), nskile, sk1r2(j), 
     1                      rbar, rbar, rbar, rbar)

        CALL vectorize(ff(1,i), dir1(1,i), x1(1,i), y1(1,i), nskile)
        CALL vectorize(ff(1,j), dir1(1,j), x2(1,j), y2(1,j), nskile)
        CALL vcc(x1(1,i), y1(1,i), x2(1,j), y2(1,j), nskile, sk1vcor(j))
C       Skiles2 computation
        sk2ia(j) = iagree(dist(1,i), dist(1,j), nskile)
   
        CALL correl(dist(1,i), dist(1,j), nskile, sk2r2(j), 
     1                      rbar, rbar, rbar, rbar)

        CALL vectorize(dist(1,i), dir2(1,i), x1(1,i), y1(1,i), nskile)
        CALL vectorize(dist(1,j), dir2(1,j), x2(1,j), y2(1,j), nskile)
        CALL vcc(x1(1,i), y1(1,i), x2(1,j), y2(1,j), nskile, sk2vcor(j))

        WRITE (scorefile,9003) i, j, sk1ia(j), sk1r2(j), sk1vcor(j),
     1   sk2ia(j), sk2r2(j), sk2vcor(j)

 2100 CONTINUE
 2000 CONTINUE
 9003 FORMAT (2I3, 3F6.3, 9x, 3F6.3)

      
      RETURN
      END
