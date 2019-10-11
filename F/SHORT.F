      PROGRAM short
C     Program to shorten strings of output, make two blanks the
C       maximum allowed.

      CHARACTER*24  au1, au2, au3
      CHARACTER*196 title
      CHARACTER*24  jou
      INTEGER       vol, page1, page2, year
      CHARACTER*16  note
      INTEGER       nref

C     Local variables:
      CHARACTER*60 fname
      CHARACTER*350 outch, line
      INTEGER i, j, n, length 
      CHARACTER*10 ofmt

      PRINT *,'What is the name of the data file?'
      READ (*,9001) fname
 9001 FORMAT (A60)

      OPEN (10, FILE=fname, Form='FORMATTED', STATUS='OLD')
      READ(10, 9004) n
 9004 FORMAT (I5)

 1000 CONTINUE
        outch = ' '
        READ (10, 9002, END=3000) au1, au2, au3, title, jou, vol,
     1                  page1, page2, year, note, nref
CD       WRITE (*, 9002) au1, au2, au3, title, jou, vol,
CD    1                  page1, page2, year, note, nref
 9002   FORMAT (3A24,/,A196,/,A24,4I5,1X,A16,I4)

        WRITE (line, '(3A24,A196,A24,4I5,1x,A16,I4)') au1, au2, au3, 
     1         title, jou, vol, page1, page2, year, note, nref
        i = 1
        j = 1
 2000   CONTINUE
          n = 0
          IF (line(i:i) .EQ. ' ') THEN
C           count to end of blank space
 2001       CONTINUE
            IF (line(i+n:i+n) .EQ. ' ' .AND. i+n .LT. 336) THEN
              n = n + 1
              GO TO 2001
            ENDIF
            outch(j:j) = line(i:i)
C           outch(j+1:j+1) = line(i+1:i+1)
            j = j + 1
            i = i + n 
           ELSE
            outch(j:j) = line(i:i)
            j = j + 1
            i = i + 1
          ENDIF
          IF (i .LT. 336) GO TO 2000
          length = INDEX(outch, '     ')
C         WRITE (ofmt, 9003) length
C         WRITE (*,ofmt) outch
CD         PRINT *,length
          IF (length .LE. 79) THEN
            WRITE (*,9009) outch
           ELSE IF (length .LE. 150) THEN
CD           PRINT *,'using format 9005'
            WRITE (*,9005) outch(1:79),outch(80:150)
           ELSE IF (length .LE. 220) THEN
            WRITE (*,9005) outch(1:79),outch(80:150),outch(151:220)
           ELSE IF (length .LE. 290) THEN
            WRITE (*,9005) outch(1:79),outch(80:150),outch(151:220),
     1                                 outch(221:290) 
           ELSE
            WRITE (*,9005) outch(1:79),outch(80:150),outch(151:220),
     1                                 outch(221:290),outch(290:350)
          ENDIF
        GO TO 1000

 3000 CONTINUE

 9003 FORMAT ( '(A',I3,')' )

 9009 FORMAT (A79)

 9005 FORMAT (A79,/,8X,A71)

 9006 FORMAT (A79,/, 2(8X,A71) )

 9007 FORMAT (A79,/, 3(8X,A71) )

 9008 FORMAT (A79,/, 4(9X,A70) )

      END
