
      SUBROUTINE createa(au1, au2, au3, title, jou, vol, page1, page2,
     1                   year, note, nref, maxsz, npap)
      
C     Program to search a reference database.
C       BG 10-15-87
C       Merging files added prior to 3-11-88
C       Creating a reference file added 3-11-88.

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
      INTEGER posit
      LOGICAL dup, newpap, yes
 
C***********************************************************----------!!
 10   CONTINUE

C     Begin the program, open files as needed, read in from old file
C       if requested.
      npap = 1
      PRINT *,'Are you creating a new file?'
      IF (.NOT.yes(.FALSE.)) THEN
        CALL start (au1, au2, au3, title, jou, vol, 
     1              page1, page2, year, note, nref, npap, maxsz)
      ENDIF
      PRINT *,'What do you want to call the crash file?'
      READ (*,9004) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='NEW')

C     Now read in new information.
C     Emulate a repeat-until loop
 4000 CONTINUE
 
C       Get bibliographic info for an entry:	
        CALL getbib ( tau1, tau2, tau3, ttitle, tjou, tvol,
     1                tpage1, tpage2, tyear, tnote, tnref)

C       Check to see if this is a new author, use subr.
        CALL oldau(tau1, tau2, tau3, ttitle, tjou, tvol, 
     1               tpage1, tpage2, tyear, tnote, tnref,
     2               au1, au2, au3, title, jou, vol,
     3               page1, page2, year, note, nref,
     4               npap, dup, posit)

        IF ( .NOT. dup) THEN
          newpap = .TRUE.
          GO TO 4030 
         ELSE
          GO TO 4100
        ENDIF

 4030   CONTINUE

C       All entries >= posit must be bumped up one, and N increased by 1.
        CALL newau( au1, au2, au3, title, jou, vol,
     1                 page1, page2, year, note, nref,
     2              tau1, tau2, tau3, ttitle, tjou, tvol,
     3                 tpage1, tpage2, tyear, tnote, tnref, 
     4              posit, npap )

 4100   CONTINUE
C       Done entering this paper

C       Write info to session file in case of crash.
        WRITE (11, 9003) au1(posit), au2(posit), au3(posit), 
     1      title(posit), jou(posit), vol(posit), 
     2      page1(posit), page2(posit), year(posit),
     3      note(posit), nref(posit)

C       Check to see if there are more papers.
        PRINT *,'Would you like to enter more papers?'
	IF (yes(.TRUE.)) GO TO 4000

C***********************************************************----------!!

C     Finished entering papers.
      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP')

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

      PRINT *,'Would you like to see the complete listing?'
      IF (yes(.FALSE.)) THEN
        DO 5100 i = 1, npap 
          WRITE (*, 9003) au1(i), au2(i), au3(i), title(i), jou(i),
     2       vol(i), page1(i), page2(i), year(i), note(i), nref(i)
 5100   CONTINUE
      ENDIF
C***********************************************************----------!!

 9999 CONTINUE 
      PRINT *,'Would you like to (create/add to) a different file now?'
      IF (yes(.FALSE.)) GO TO 10 

C***********************************************************----------!!

 9001 FORMAT (I6)
 
 9003 FORMAT (3A24,/,A196,/,A24,4I5,1X,A16,I4)

 9004 FORMAT (A60)

      RETURN
      END
