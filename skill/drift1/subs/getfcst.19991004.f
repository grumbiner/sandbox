      SUBROUTINE getfcst(date, funit1, funit2, dd, ff, dir, dist, code)
C     Read in both the skiles1 and skiles2 forecasts.
C     Bob Grumbine 21 April 1994.
C     Modified to work more easily as a subroutine 4 April 1995.

      IMPLICIT none

C     Parameters for reading data in
      INTEGER nskile, ndays
      PARAMETER (nskile=207)
      PARAMETER (ndays =  6)

      INTEGER i, k, l, m, nday
      INTEGER skpt(nskile), skpt2(4*nskile)
      REAL dd(nskile, ndays), ff(nskile, ndays)
      REAL dir(nskile, ndays), dist(nskile, ndays)
      REAL lat(4*nskile), long(4*nskile)

      CHARACTER*60 fname1, fname2
      CHARACTER*80 header
      CHARACTER*1 trailer
      INTEGER funit1, funit2, code, date

      code = 0
      WRITE (fname1, 9009) date
      WRITE (fname2, 9010) date
 9009 FORMAT ('sk1.',I6)
 9010 FORMAT ('sk2.',I6)

      OPEN (funit1, FILE=fname1, STATUS='OLD', FORM='FORMATTED', 
     1                    ERR=9100)
      OPEN (funit2, FILE=fname2, STATUS='OLD', FORM='FORMATTED', 
     1                    ERR=9200)

 9020 FORMAT (A80)

      DO 9000 nday = 1, ndays

        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        DO 1000 i = 1, nskile
          READ (funit2, *, END=9200, ERR=9200) skpt2(i), 
     1              dir(i,nday), dist(i,nday)
 1000   CONTINUE
        i = nskile
        READ (funit2, 9020, END=9200, ERR=9200) header
 1001   CONTINUE
        i = i + 1
        READ (funit2,9020, END=9200, ERR=9200) header
        READ ( header, 9007, ERR=1002)
     1    skpt2(i), long(i), lat(i)
        IF (skpt2(i) .NE. 0) GO TO 1001
 1002   CONTINUE
 
        READ (funit2,9020, END=9200, ERR=9200) trailer

        READ (funit1,9020) header
        READ (funit1,9020) header
        READ (funit1,9020) header
        READ (funit1,9020) header
        READ (funit1,9020) header
        READ (funit1,9020) header
        DO 1100 i = 1, 51
          k = i + 52
          l = i + 104
          m = i + 156
          READ (funit1, 9002) skpt(i), dd(i,nday), ff(i,nday),
     1                    skpt(k), dd(k,nday), ff(k,nday),
     2                    skpt(l), dd(l,nday), ff(l,nday),
     3                    skpt(m), dd(m,nday), ff(m,nday)
 1100   CONTINUE
        i = 52
        k = i + 52
        l = i + 104
        READ (funit1, 9002) skpt(i), dd(i,nday), ff(i,nday),
     1                  skpt(k), dd(k,nday), ff(k,nday),
     2                  skpt(l), dd(l,nday), ff(l,nday)
 
 9000 CONTINUE

C==========================================
 9007   FORMAT (I4, 3x, 2F8.3, 2x, 2F6.1)

 9002   FORMAT (2x, I3, 3x, F5.1, 4x, F5.1, 8x,
     1              I3, 3x, F5.1, 4x, F5.1, 8x,
     2              I3, 3x, F5.1, 4x, F5.1, 8x,
     3              I3, 3x, F5.1, 4x, F5.1        )

      code = 0
      RETURN

 9100 CONTINUE
      code = -1
      RETURN

 9200 CONTINUE
      code = -2

      RETURN

      END
