      PROGRAM get6
C     Intercompare forecasts from the skiles1 and skiles2 programs.
C     Analyze all the forecasts.
      IMPLICIT none

      INTEGER nskile, ndays, ndates
      PARAMETER (nskile = 207)
      PARAMETER (ndays  =   6)
      PARAMETER (ndates = 200)
      INTEGER R, THETA
      PARAMETER (R = 1)
      PARAMETER (THETA = 2)

C     Parameters for reading data in
      REAL dir1(nskile, ndays), ff(nskile, ndays)
      REAL dir2(nskile, ndays), dist(nskile, ndays)
      REAL x1(nskile,ndays), y1(nskile,ndays)
      REAL x2(nskile,ndays), y2(nskile,ndays)

C     The global forecast data fields
      REAL meth1(nskile, ndates, ndays, 2)
      REAL meth2(nskile, ndates, ndays, 2)

      CHARACTER*60 fname1, fname2
      INTEGER code, ndate
      CHARACTER*3 model1, model2
      CHARACTER*2 yy, mm, dd
      REAL ia, rbar
      
      INTEGER d, m, i, j, daysin(12)

      yy = '93'
      daysin(1) = 31
      daysin(2) = 28
      daysin(3) = 31
      daysin(4) = 30
      daysin(5) = 31
      daysin(6) = 30
      daysin(7) = 31
      daysin(8) = 31
      daysin(9) = 15

 9009 FORMAT (I2)
 9008 FORMAT ('0',I1)

      DO 9998 m = 5, 9
        IF (m .GE. 10) THEN
          WRITE (mm, 9009) m
         ELSE
          WRITE (mm, 9008) m
        ENDIF
      DO 9999 d = 1, daysin(m)
        IF (d .GE. 10) THEN
          WRITE (dd, 9009) d
         ELSE
          WRITE (dd, 9008) d
        ENDIF
        model1 = 'sk1'
        model2 = 'sk2'
        fname1 = model1//'.'//yy//mm//dd
        fname2 = model2//'.'//yy//mm//dd

        PRINT *,' '
        PRINT *,' '
        PRINT *,'Date is ',yy//mm//dd
        CALL getfcst(dir1, ff, dir2, dist, 
     1        fname1, fname2, code)

        IF (code .EQ. 0) THEN
          CALL transfer(dir1, ff, dir2, dist, meth1, meth2, ndate)
         ELSE
          PRINT *,'Error opening file'
        ENDIF

        CALL sanaly(dir1, dir2, ff, dist)
 9999 CONTINUE
 9998 CONTINUE

      STOP
      END
