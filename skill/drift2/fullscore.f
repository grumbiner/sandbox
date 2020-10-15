      PROGRAM fullscore
C     Conduct scoring of entire collections of forecasts from
C       the drift models.
C     Presumes that 'survey' has already been run, and that buoy
C       observations are collected in a manner consistent with 
C       this program's intentions.

      INTEGER yy, mm
      INTEGER sk1tmp, sk2tmp
      INTEGER trimfile, scorefile, skpoints, matchfile

      sk1tmp    = 10
      sk2tmp    = 11
      trimfile  = 12
      skpoints  = 13
      matchfile = 14
      scorefile = 15
      OPEN (trimfile, FILE='trimall', FORM='FORMATTED', STATUS='OLD')
      OPEN (scorefile, FILE='scoreall', FORM='FORMATTED', STATUS='NEW')
      OPEN (skpoints, FILE='forecast.points', FORM='FORMATTED',
     1                STATUS='OLD')
CHP      OPEN (matchfile, FILE='matchups', FORM='FORMATTED', STATUS='UNK')
      OPEN (matchfile, FILE='matchups', FORM='FORMATTED', 
     1     STATUS='UNKNOWN')


      GO TO 2000

      yy = 93
      DO 1000 mm = 4, 12
        CALL sk1vs2(yy, mm, sk1tmp, sk2tmp, scorefile)
 1000 CONTINUE


      yy = 94
      DO 1100 mm = 1, 12
        CALL sk1vs2(yy, mm, sk1tmp, sk2tmp, scorefile)
 1100 CONTINUE
      yy = 95
      DO 1200 mm = 1, 2
        CALL sk1vs2(yy, mm, sk1tmp, sk2tmp, scorefile)
 1200 CONTINUE
    
      CALL matchup(sk1tmp, sk2tmp, trimfile, matchfile, skpoints)
  
 2000 CONTINUE
      REWIND (matchfile)

      CALL matscor(matchfile, scorefile)

      STOP
      END
