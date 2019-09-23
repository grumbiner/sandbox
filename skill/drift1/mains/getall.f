      PROGRAM getall
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
      REAL dir1(nskile+1, ndays), ff(nskile+1, ndays)
      REAL dir(4*nskile, ndays), dist(4*nskile, ndays)

C     The global forecast data fields
      REAL meth1(nskile, ndates, ndays, 2)
      REAL meth2(nskile, ndates, ndays, 2)

      CHARACTER*60 fname1, fname2
      INTEGER code, ndate
      CHARACTER*3 model1, model2
      CHARACTER*2 yy, mm, dd

CD      fname1 = 'sk1.930503'
CD      fname2 = 'sk2.930503'
      yy = '93'
      mm = '05'
      dd = '03'
      model1 = 'sk1'
      model2 = 'sk2'
      fname1 = model1//'.'//yy//mm//dd
      fname2 = model2//'.'//yy//mm//dd

      CALL getfcst(dir1, ff, dir, dist, 
     1      fname1, fname2, code)

      IF (code .EQ. 0) THEN
        CALL transfer(dir1, ff, dir, dist, meth1, meth2, ndate)
       ELSE
        PRINT *,'Error opening file'
      ENDIF
 
      STOP
      END
