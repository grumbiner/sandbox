      SUBROUTINE sk1vs2(year, month, sk1tmp, sk2tmp, scorefile)
C     Intercompare forecasts from the skiles1 and skiles2 programs.
C     Analyze all the forecasts for a given year and month.
C     Bob Grumbine 4 April 1994.

      IMPLICIT none

      INTEGER sk1tmp, sk2tmp, scorefile
      INTEGER nskile, ndays
      PARAMETER (nskile = 207)
      PARAMETER (ndays  =   6)


C     Parameters for reading data in
      REAL dir1(nskile, ndays), dist1(nskile, ndays)
      REAL dir2(nskile, ndays), dist2(nskile, ndays)
      REAL x1(nskile,ndays), y1(nskile,ndays)
      REAL x2(nskile,ndays), y2(nskile,ndays)

      CHARACTER*60 fname1, fname2
      INTEGER date
      INTEGER code, ndate
      CHARACTER*3 model1, model2
      CHARACTER*2 yy, mm, dd
      INTEGER year, month
      REAL ia, rbar
      
      INTEGER d, m, i, j, daysin(12)

      daysin(1) = 31
      daysin(2) = 28
      IF (month .EQ. 2 .AND. MOD(year, 4) .EQ. 0) THEN
        daysin(2) = 29
      ENDIF
      daysin(3) = 31
      daysin(4) = 30
      daysin(5) = 31
      daysin(6) = 30
      daysin(7) = 31
      daysin(8) = 31
      daysin(9) = 30
      daysin(10) = 31
      daysin(11) = 30
      daysin(12) = 31

 9009 FORMAT (I2)
 9008 FORMAT ('0',I1)
 9007 FORMAT (3A2)
      
      IF (year .GE. 10) THEN
        WRITE (yy, 9009) year
       ELSE
        WRITE (yy, 9008) year
      ENDIF

      IF (month .GE. 10) THEN
        WRITE (mm, 9009) month
       ELSE
        WRITE (mm, 9008) month
      ENDIF

      DO 9999 d = 1, daysin(month)
        IF (d .GE. 10) THEN
          WRITE (dd, 9009) d
         ELSE
          WRITE (dd, 9008) d
        ENDIF
        model1 = 'sk1'
        model2 = 'sk2'
        fname1 = model1//'.'//yy//mm//dd
        fname2 = model2//'.'//yy//mm//dd

        WRITE (*, 9007) yy, mm, dd
        date = d + 100*(month + 100*year)
        CALL getfcst(date, sk1tmp, sk2tmp, dir1, dist1, dir2, dist2, 
     1      code)

        IF (code .EQ. 0) THEN
          WRITE (scorefile, 9007) yy, mm, dd
          CALL sanaly(dir1, dir2, dist1, dist2, scorefile)
         ELSE
          PRINT *,'Error opening file', date
        ENDIF

 9999 CONTINUE

      RETURN
      END
