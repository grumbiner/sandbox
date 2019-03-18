      SUBROUTINE hread(au1, au2, au3, title, jou, vol, page1, page2, 
     1                   year, note, nref, n, maxsz)

C     Parameters of the program.
      INTEGER namlen, tlen, notlen, maxsz
      PARAMETER (namlen =   24)
      PARAMETER (tlen   =  196)
      PARAMETER (notlen =   16)
      
C     Declare the main data structures.
      CHARACTER*24  au1(maxsz), au2(maxsz), au3(maxsz)
      CHARACTER*196 title(maxsz)
      CHARACTER*24  jou(maxsz)
      INTEGER vol(maxsz), page1(maxsz), page2(maxsz), year(maxsz)
      CHARACTER*16 note(maxsz)
      INTEGER nref(maxsz)
      
C     Local and temporary variables.
      INTEGER i, n, nmin 
      CHARACTER*60 fname
      LOGICAL yes
CHP $EMA title 

C***********************************************************----------!!
   
C     Generate the have you read list.
      PRINT *,'Will search through the bilbiography alphabetically.'
      PRINT *,'  When I find a paper with >= the minimum number of refer
     1ences'
      PRINT *,'  I will ask you if you have read it.'
      PRINT *,' '
      PRINT *,' '
      PRINT *,'What is the minimum number of references?'
      READ (*,9001) nmin
      PRINT *,'What is the name of the file for papers you have read?'
      READ (*,9004) fname
      OPEN (14, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      PRINT *,'What is the name of the file for those you havent read?'
      READ (*,9004) fname
      OPEN (15, FILE=fname, FORM='FORMATTED', STATUS='NEW')
     
      DO 4000 i = 1, n
        IF (nref(i) .GE. nmin) THEN
          PRINT *,'Have you read:'
          WRITE (*,9003) au1(i), au2(i), au3(i), title(i), jou(i),
     1                   vol(i), page1(i), page2(i), year(i),
     2                   note(i), nref(i)
          IF (yes(.FALSE.)) THEN
            WRITE (14,9003) au1(i), au2(i), au3(i), title(i), 
     1              jou(i), vol(i), page1(i), page2(i), year(i),
     2              note(i), nref(i)
           ELSE
            WRITE (15,9003) au1(i), au2(i), au3(i), title(i), 
     1              jou(i), vol(i), page1(i), page2(i), year(i),
     2              note(i), nref(i)
          ENDIF  
        ENDIF
 4000 CONTINUE

      CLOSE (14, STATUS='KEEP')
      CLOSE (15, STATUS='KEEP')
     
C***********************************************************----------!!

 9001 FORMAT (I6)
 
 9003 FORMAT (3A24,/,A196,/,A24,4I5,1X,A16,I4)

 9004 FORMAT (A60)

      RETURN
      END
