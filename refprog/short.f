
      SUBROUTINE short(au1, au2, au3, title, jou, vol, page1, page2,
     1                   year, note, nref, pfmt, unit)
C     Subroutine to shorten strings of output, make two blanks the
C       maximum allowed.

      CHARACTER*24  au1, au2, au3
      CHARACTER*196 title
      CHARACTER*24  jou
      INTEGER       vol, page1, page2, year
      CHARACTER*16  note
      INTEGER       nref
      CHARACTER*40  pfmt
      INTEGER       unit

C     Local variables:
      CHARACTER*350 outch, line
      INTEGER i, j, k, n, length 

C***********************************************************..........!!

      outch = ' '
      WRITE (line, pfmt) au1, au2, au3, 
     1       title, jou, vol, page1, page2, year, note, nref
      i = 1
      j = 1
 2000 CONTINUE
        n = 0
        IF (line(i:i) .EQ. ' ') THEN
C         count to end of blank space
 2001     CONTINUE
          IF (line(i+n:i+n) .EQ. ' ' .AND. i+n .LT. 336) THEN
            n = n + 1
            GO TO 2001
          ENDIF
          outch(j:j) = line(i:i)
C         outch(j+1:j+1) = line(i+1:i+1)
          j = j + 1
          i = i + n 
         ELSE
          outch(j:j) = line(i:i)
          j = j + 1
          i = i + 1
        ENDIF
        IF (i .LT. 336) GO TO 2000
        length = INDEX(outch, '     ')
CD      PRINT *,length
C       Make first line break at a space
C       IF (outch(79:79) .NE. ' ') THEN
CD        PRINT *,'padding?'
C         k = 0 
C3000     CONTINUE
C         k = k + 1
C         IF (outch(79-k:79-k) .NE. ' ') GO TO 3000
C         DO 3010 j = 1, 79-k 
C            line(j:j) = outch(j:j)
C3010     CONTINUE
C         DO 3020 i = j, 79
C           line(i:i) = ' '
C3020     CONTINUE
C         DO 3030 j = 80, 350
C           line(j:j) = outch(j-k:j-k)
C3030     CONTINUE
C         DO 3040 j = 1, 350-k
C           outch(j:j) = line(j:j)
C3040     CONTINUE
C         length = length+k
C       ENDIF
        IF (length .GT. 79)  CALL break(outch,  79, length)
        IF (length .GT. 150) CALL break(outch, 150, length)  
        IF (length .GT. 220) CALL break(outch, 220, length)  
        IF (length .GT. 290) CALL break(outch, 290, length)  
         

        IF (length .LE. 79) THEN
          WRITE (unit,9009) outch
         ELSE IF (length .LE. 150) THEN
CD         PRINT *,'using format 9005'
          WRITE (unit,9005) outch(1:79),outch(80:150)
         ELSE IF (length .LE. 220) THEN
          WRITE (unit,9005) outch(1:79),outch(80:150),outch(151:220)
         ELSE IF (length .LE. 290) THEN
          WRITE (unit,9005) outch(1:79),outch(80:150),outch(151:220),
     1                               outch(221:290) 
         ELSE
          WRITE (unit,9005) outch(1:79),outch(80:150),outch(151:220),
     1                               outch(221:290),outch(290:350)
        ENDIF

 9009 FORMAT (A79)

 9005 FORMAT (A79,/,8X,A71)

 9006 FORMAT (A79,/, 2(8X,A71) )

 9007 FORMAT (A79,/, 3(8X,A71) )

 9008 FORMAT (A79,/, 4(8X,A71) )

      RETURN
      END
