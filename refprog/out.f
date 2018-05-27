
      SUBROUTINE out(au1, au2, au3, title, jou, vol, page1, page2,
     1                   year, note, nref, maxsz, npap)
      
C     Parameters of the program.
      INTEGER namlen, tlen, notlen, maxsz
      PARAMETER (namlen  =   24)
      PARAMETER (tlen    =  196)
      PARAMETER (notlen  =   16)
      
C     Declare the main data structures.
      CHARACTER*24  au1(maxsz), au2(maxsz), au3(maxsz)
      CHARACTER*196 title(maxsz)
      CHARACTER*24  jou(maxsz)
      INTEGER vol(maxsz), page1(maxsz), page2(maxsz), year(maxsz)
      CHARACTER*16 note(maxsz)
      INTEGER nref(maxsz)
      INTEGER npap
 
C     Local and temporary variables.
      CHARACTER*60 fname
      CHARACTER*40 pfmt
      INTEGER i, j, m, n
      LOGICAL yes

 
C***********************************************************----------!!

C     Now write out the information.
      PRINT *,'What file name would you like to save this under?'
      READ (*,9004) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='UNKNOWN')
      WRITE (12, 9001) npap
      DO 5000 i = 1, npap
        WRITE (12, 9003) au1(i), au2(i), au3(i), title(i), jou(i),
     1     vol(i), page1(i), page2(i), year(i), note(i), nref(i)
 5000 CONTINUE
      CLOSE (12, STATUS='KEEP')

      PRINT *,'Would you like to scan the complete listing?'
      IF (yes(.FALSE.)) THEN
      pfmt = '(3A24,A196,A24,4I5,1X,A16,I4)'
        DO 5100 i = 1, npap 
          CALL short(au1(i), au2(i), au3(i), title(i), jou(i),
     1                vol(i), page1(i), page2(i), year(i),
     2                note(i), nref(i), pfmt, 6)
CD        WRITE (*, 9003)  au1(i), au2(i), au3(i), title(i), jou(i),
CD   2       vol(i), page1(i), page2(i), year(i), note(i), nref(i)
 5100   CONTINUE
      ENDIF
C***********************************************************----------!!

 9001 FORMAT (I6)
 
 9003 FORMAT (3A24,/,A196,/,A24,4I5,1X,A16,I4)

 9004 FORMAT (A60)

      RETURN
      END
