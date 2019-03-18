      SUBROUTINE rin(uct, day, month, year, speed, dir, uvel, vvel,
     1               temp, press, cond, length)

C     Subroutine to fill the data matrices.

C     Declare arguments here:
      INTEGER uct(9200), day(9200), month(9200), year(9200)
      REAL speed(9200), uvel(9200), vvel(9200)
      REAL temp(9200), press(9200), cond(9200)
      INTEGER dir(9200)
      INTEGER length

C     Declare arguments to getdat here:
      INTEGER tuct, tday, tmonth, tyear, tdir
      REAL tspeed, tuvel, tvvel, ttemp, tpress, tcond
      INTEGER tline

C***********************************************************----------!!
C     Declare local variables:
      CHARACTER*60 fname
      CHARACTER*80 header
      INTEGER i

C     Start execution.
      PRINT *,'What is the name of the file which contains the data?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')

      READ (10,9002) header
      WRITE (*,9002) header

      i = 0
 1000 CONTINUE

        i = i + 1
C***********************************************************----------!!
        READ (10,9003, END=2000, ERR=3000) tuct, tday, tmonth, tyear,
     1                           tspeed, tdir, tuvel, tvvel,
     2                           ttemp, tpress, tcond, tline
C       WRITE (*,9003) tuct, tday, tmonth, tyear,
C    1                 tspeed, tdir, tuvel, tvvel,
C    2                 ttemp, tpress, tcond, tline

        uct  (i) = tuct
        day  (i) = tday
        month(i) = tmonth
        year (i) = tyear
        speed(i) = tspeed
        dir  (i) = tdir
        uvel (i) = tuvel
        vvel (i) = tvvel
        temp (i) = ttemp
        press(i) = tpress
        cond (i) = tcond
        
        IF (tline .NE. i) PRINT *,'Mismatch in line numbers',i
        
        GOTO 1000
 3000   PRINT *,'Data read error on line',i
        GOTO 1000

 2000 CONTINUE
 
      length = i
      CLOSE (10, STATUS='KEEP')

 9001 FORMAT (A60)

 9002 FORMAT (A80)

 9003 FORMAT (I4,1X,3(I2,1X),F4.1,1X,I3,1X,F5.1,1X,F5.1,2X,F5.2,2X,
     1        F5.1,2X,F6.3,2X,I4)

      RETURN
      END
