      PROGRAM tally

      IMPLICIT none

      REAL r1(31, 6), r2(31, 6)
      REAL ia1(31, 6), ia2(31, 6)
      REAL vc1(31, 6), vc2(31, 6)
      CHARACTER*60 fname
      REAL sum1(6), sum2(6), diff(6)
      INTEGER i, j, wins(6), nfor(6)
      REAL isum1(6), isum2(6), idiff(6)
      REAL vsum1(6), vsum2(6), vdiff(6)
      INTEGER iwins(6)
      INTEGER vwins(6)
      REAL correl, ia, vc
      INTEGER day, fday


      PRINT *,'What is the name of the score file for skile1?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      PRINT *,'What is the name of the skile2 score file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      PRINT *,'What is the name of the output score file?'
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')
 9001 FORMAT (A60)

      r1 = -2.
      r2 = -2.
      ia1 = -2.
      ia2 = -2.
      vc1 = -2.
      vc2 = -2.
 9002 FORMAT (9x,I2,4x,I3,8X,F6.3,4X,F6.3,4X,F6.3)
 9003 FORMAT (10x,I2,4x,I3,8X,F6.3,4X,F6.3,4X,F6.3)

 1000 CONTINUE
        READ (10, 9002, END=1100) day, fday, correl, ia, vc
        r1(day, fday) = correl
        ia1(day, fday) = ia
        vc1(day, fday) = vc
        GO TO 1000

 1100 CONTINUE
        READ (11, 9003, END=1200) day, fday, correl, ia, vc
CD        WRITE (*, 9003) day, fday, correl, ia, vc
        r2(day, fday) = correl
        ia2(day, fday) = ia
        vc2(day, fday) = vc
        GO TO 1100

 1200 CONTINUE

      sum1 = 0.
      sum2 = 0.
      diff = 0.
      wins = 0
      isum1 = 0.
      isum2 = 0.
      idiff = 0.
      iwins = 0
      vsum1 = 0.
      vsum2 = 0.
      vdiff = 0.
      vwins = 0
      nfor = 0
      DO 2000 i = 1, 31
        DO 2100 j = 1, 6
        IF (r1(i,j) .NE. -2. .AND. r2(i,j) .NE. -2) THEN
          WRITE (12, 9004) i, j, r1(i,j), r2(i,j), r1(i,j)-r2(i,j),
     1        ia1(i,j), ia2(i,j), ia1(i,j)-ia2(i,j),
     1        vc1(i,j), vc2(i,j), vc1(i,j)-vc2(i,j)
          sum1(j) = sum1(j) + r1(i,j)
          sum2(j) = sum2(j) + r2(i,j)
          diff(j) = diff(j) + r1(i,j)-r2(i,j)
          IF (r1(i,j) .GT. r2(i,j)) wins(j) = wins(j)+1
          isum1(j) = isum1(j) + ia1(i,j)
          isum2(j) = isum2(j) + ia2(i,j)
          idiff(j) = idiff(j) + ia1(i,j) - ia2(i,j)
          IF (ia1(i,j) .GT. ia2(i,j)) iwins(j) = iwins(j)+1
          vsum1(j) = vsum1(j) + vc1(i,j)
          vsum2(j) = vsum2(j) + vc2(i,j)
          vdiff(j) = vdiff(j) + vc1(i,j) - vc2(i,j)
          IF (vc1(i,j) .GT. vc2(i,j)) vwins(j) = vwins(j)+1
          nfor(j) = nfor(j) + 1
        ENDIF
 2100   CONTINUE
 2000 CONTINUE
 9004 FORMAT (2I3, 3F7.3, 2x, 3F7.3, 2x, 3F7.3)

      DO 3000 j = 1, 6
        IF (nfor(j) .NE. 0) THEN
          WRITE (12,9005) j, sum1(j)/nfor(j), sum2(j)/nfor(j), 
     1        diff(j)/nfor(j), nfor(j), wins(j),
     2        isum1(j)/nfor(j), isum2(j)/nfor(j), idiff(j)/nfor(j), 
     3        iwins(j),
     4        vsum1(j)/nfor(j), vsum2(j)/nfor(j), vdiff(j)/nfor(j), 
     5        vwins(j)
          WRITE (*,9005) j, sum1(j)/nfor(j), sum2(j)/nfor(j), 
     1        diff(j)/nfor(j), nfor(j), wins(j),
     2        isum1(j)/nfor(j), isum2(j)/nfor(j), idiff(j)/nfor(j), 
     3        iwins(j),
     4        vsum1(j)/nfor(j), vsum2(j)/nfor(j), vdiff(j)/nfor(j), 
     5        vwins(j)
        ENDIF
 3000 CONTINUE
 9005 FORMAT ('Day ',I2,' ',3F7.3, 2I3, 2x, 3F7.3, I3, 2x, 3F7.3, I3)
      STOP
      END 
