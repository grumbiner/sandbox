
      SUBROUTINE merge(au1, au2, au3, title, jou, vol, page1, page2, 
     1                  year, note, nref, maxsz, npap)
      
C     Program to search a reference database.
C       BG 10-15-87
C       Merging files added prior to 3-11-88
C       Creating a reference file added 3-11-88.
C       file merging siphoned off to subroutine 11-9-88

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
      
C     Local and temporary variables (for merging).
      CHARACTER*24 tau1, tau2, tau3, tjou
      CHARACTER*196 ttitle
      INTEGER tvol, tpage1, tpage2, tyear
      INTEGER tnref
      CHARACTER*16 tnote

C     Local and temporary variables.
      CHARACTER*60 fname
      INTEGER i, j, m, n
      INTEGER gtype, stype
      INTEGER double, posit
      LOGICAL dup, yes
            
 
C***********************************************************----------!!

C     Begin the routine, open files as needed. 
      npap = 1
      CALL start (au1, au2, au3, title, jou, vol, 
     1            page1, page2, year, note, nref, npap, maxsz)

C***********************************************************----------!!

C       PROGRAM merge

 1      CONTINUE
          PRINT *,'What is the name of the file you would like to add?'
          READ (*, 9004) fname
          OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='OLD')
          READ (11, 9001) m
          IF (npap+m .GT. maxsz) THEN
            PRINT *, 'This file is too large to merge, would you like to
     1 continue?'
            IF (yes(.FALSE.)) GO TO 1
          ENDIF

          double = 0
          j = 0
 1000     CONTINUE
          j = j + 1
          READ (11,9003) tau1, tau2, tau3, ttitle, tjou, tvol, 
     1                     tpage1, tpage2, tyear, tnote, tnref

C         Check to see if this is a new author, use subr.
          CALL oldau(tau1, tau2, tau3, ttitle, tjou, tvol, 
     1                 tpage1, tpage2, tyear, tnote, tnref,
     2                 au1, au2, au3, title, jou, vol,
     3                 page1, page2, year, note, nref,
     4                 npap, dup, posit)

          IF ( .NOT. dup) THEN
C           All entries >= posit must be bumped up one, and N increased by 1.
            CALL newau( au1, au2, au3, title, jou, vol,
     1                   page1, page2, year, note, nref,
     2                  tau1, tau2, tau3, ttitle, tjou, tvol,
     3                   tpage1, tpage2, tyear, tnote, tnref, 
     4                  posit, npap )
           ELSE
            double = double+1 
          ENDIF

          IF (j .LT. m) GO TO 1000

        PRINT *,'Would you like to add another file?'
        IF (yes(.FALSE.)) GO TO 1
      
        PRINT *,'What file name would you like for the new file?'
        READ (*,9004) fname
        OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='UNKNOWN')

        PRINT *,'Total references=',npap,'  Number in added file=', m,
     1          '  Number of duplicates=',double
        WRITE (12,9001) npap
        DO 2000 i = 1, npap
          WRITE (12, 9003) au1(i), au2(i), au3(i), title(i), jou(i), 
     1       vol(i), page1(i), page2(i), year(i), note(i), nref(i)
 2000   CONTINUE
        CLOSE(12, STATUS='KEEP')
  
C***********************************************************----------!!
C     Finished merging files.
      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP')

C***********************************************************----------!!

 9001 FORMAT (I6)
 
 9003 FORMAT (3A24,/,A196,/,A24,4I5,1X,A16,I4)

 9004 FORMAT (A60)

      RETURN
      END
