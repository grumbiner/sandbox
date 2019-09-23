C      getfcst(int *date, int *funit2, float *dir, float *dist, int *code)
      SUBROUTINE getfcst(date, funit2, dir, dist, code)
C     Read in both the skiles1 and skiles2 forecasts.
C     Bob Grumbine 21 April 1994.
C     Modified to work more easily as a subroutine 4 April 1995.

      IMPLICIT none

C     Parameters for reading data in
      INTEGER nskile, ndays
      PARAMETER (nskile=207)
      PARAMETER (ndays = 16)

      INTEGER i, k, l, m, nday
      INTEGER skpt2
      REAL dir(nskile, ndays), dist(nskile, ndays)
      REAL lat, long

      CHARACTER*60 fname2
      CHARACTER*80 header
      CHARACTER*1 trailer
      INTEGER funit2, code, date

      code = 0
      nday = 0
      WRITE (fname2, 9010) date
 9010 FORMAT ('fcsts/sk2.',I6)

CD      PRINT *,"about to try opening ",fname2

      OPEN (funit2, FILE=fname2, STATUS='OLD', FORM='FORMATTED', 
     1                    ERR=9300)

 9020 FORMAT (A80)

      DO 9000 nday = 1, ndays
CD        PRINT *,' day, nday = '
        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        DO 1000 i = 1, nskile
          READ (funit2, *, END=9200, ERR=9200) skpt2, 
     1              dir(i,nday), dist(i,nday)
 1000   CONTINUE
        i = nskile
        READ (funit2, 9020, END=9200, ERR=9200) header
 1001   CONTINUE
        i = i + 1
        READ (funit2,9020, END=9200, ERR=9200) header
        READ ( header, 9007, ERR=1002) skpt2, long, lat
        IF (skpt2 .NE. 0) GO TO 1001
 1002   CONTINUE
 
        READ (funit2,9020, END=9200, ERR=9200) trailer

 
 9000 CONTINUE

C==========================================
 9007   FORMAT (I4, 3x, 2F8.3, 2x, 2F6.1)

 9002   FORMAT (2x, I3, 3x, F5.1, 4x, F5.1, 8x,
     1              I3, 3x, F5.1, 4x, F5.1, 8x,
     2              I3, 3x, F5.1, 4x, F5.1, 8x,
     3              I3, 3x, F5.1, 4x, F5.1        )

      CLOSE (funit2)

C     The 1 shift is because 1 is added to nday before the attempt to read
      code = nday-1
      RETURN

 9200 CONTINUE
      CLOSE (funit2)
      code = -nday+1

      RETURN

 9300 CONTINUE
      code = 0
      RETURN 

      END
