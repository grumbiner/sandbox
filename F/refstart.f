      SUBROUTINE start(au1, au2, au3, title, jou, vol, 
     1                 page1, page2, year, note, nref , npap, maxsz)
      
C     Subroutine to open up files, and read in data if requested.
C     Well before 7 July 1998
C     Robert Grumbine
      INTEGER maxsz

C     Declare the arguments.
      INTEGER npap
      CHARACTER*24  au1(maxsz), au2(maxsz), au3(maxsz)
      CHARACTER*196 title(maxsz)
      CHARACTER*24  jou(maxsz)
      INTEGER vol(maxsz), page1(maxsz), page2(maxsz), year(maxsz)
      CHARACTER*16 note(maxsz)
      INTEGER nref(maxsz)
      
      CHARACTER*60 fname
      INTEGER i
      LOGICAL yes
 
C***********************************************************----------!!

C     Read in an old file:
      PRINT *,'What is the name of the file?'
      READ (*,9004) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      READ (10, 9001) npap
      DO 100 i = 1, npap
        READ (10, 9003) au1(i), au2(i), au3(i), title(i), jou(i),
     1   vol(i), page1(i), page2(i), year(i), note(i), nref(i)
C       WRITE (*,9003) au1(i), au2(i), au3(i), title(i), jou(i), 
C    1   vol(i), page1(i), page2(i), year(i), note(i), nref(i)
 100  CONTINUE

      CLOSE (10, STATUS='KEEP')

C***********************************************************----------!!

 9001 FORMAT (I6)
 
 9003 FORMAT (3A24,/,A196,/,A24,4I5,1X,A16,I4)

 9004 FORMAT (A60)

      RETURN
      END
