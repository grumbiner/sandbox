      SUBROUTINE search(au1, au2, au3, title, jou, vol, page1, page2,
     1                   year, note, nref, maxsz, npap) 
      
C     Program to search a reference database.
C       BG 10-15-87
C       Merging files added prior to 3-11-88
C       Creating a reference file added 3-11-88.
C       searching made a subroutine 11-9-88

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

C     Local and temporary variables - searching.
      INTEGER nloc
      PARAMETER (nloc = 2400)
      CHARACTER*60 fname
      INTEGER i, j, m, n
      INTEGER gtype, stype
      INTEGER double, posit
      LOGICAL dup, newpap, yes
      INTEGER loc(nloc), eyear, lyear, ncite
      CHARACTER*24 auname, jname, keywd

CHP $EMA title
 
C***********************************************************----------!!
 10   CONTINUE

C     Begin the program, open files as needed, read in from old file
C       if requested.
      npap = 1
      CALL start (au1, au2, au3, title, jou, vol, 
     1            page1, page2, year, note, nref, npap, maxsz)

C***********************************************************----------!!

C       start making searches:
C          First author 
C          Keywords in title
C          journal
C          year
C          nref

 9100   CONTINUE
         
        PRINT *,'Which type of search would you like: '
        PRINT *,'      Author   = 1'
        PRINT *,'      Key word = 2'
        PRINT *,'      Journal  = 3'
        PRINT *,'      Year     = 4'
        PRINT *,'      N refs.  = 5'
        READ (*,9001) stype

C       for the searches, pass the thing searched for only, and return
C         a vector with the locations of the things which contain them,
C         and the number of items identified.
        IF (stype .EQ. 1) THEN
          PRINT *,'What is the author"s last name?'
          READ (*,9005) auname
          CALL auserc(auname, au1, npap, loc, m)          
         ELSE IF (stype .EQ. 2) THEN
          PRINT *,'What is the key word?'
          READ (*,9005) keywd
          CALL kyserc(keywd, title, npap, loc, m)
         ELSE IF (stype .EQ. 3) THEN
          PRINT *,'What is the Journal name?'
          READ (*,9005) jname
          PRINT *,'not implemented'
         ELSE IF (stype .EQ. 4) THEN
          PRINT *,'What is the earliest year to search?'
          READ (*,9001) eyear
          PRINT *,'What is the latest year to search?'
          READ (*,9001) lyear
C         PRINT *,'not implemented'
          CALL years(eyear, lyear, year, npap, loc, m)
         ELSE IF (stype .EQ. 5) THEN
          PRINT *,'What is the minimum number of citations?'
          READ (*,9001) ncite
          CALL nmin(ncite, nref, npap, loc, m)
         ELSE
          PRINT *,'not a valid search'
        ENDIF 

C       Now write out the information.
        PRINT *,'What file name would you like to save this under?'
        READ (*,9004) fname
        OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='UNKNOWN')
        WRITE (12, 9001) m
        DO 3000 i = 1, m
          WRITE (12, 9003) au1(loc(i)), au2(loc(i)), au3(loc(i)),
     1                     title(loc(i)), jou(loc(i)),
     1                     vol(loc(i)), page1(loc(i)), page2(loc(i)),
     1                     year(loc(i)), note(loc(i)), nref(loc(i))
 3000   CONTINUE
        CLOSE (12, STATUS='KEEP')

        PRINT *,'Would you like to re-search the file?'
        IF (yes(.TRUE.)) GO TO 9100

C***********************************************************----------!!

      PRINT *,'Would you like to work on a different file now?'
      IF (yes(.FALSE.)) GO TO 10 

C***********************************************************----------!!

 9001 FORMAT (I6)
 
 9003 FORMAT (3A24,/,A196,/,A24,4I5,1X,A16,I4)

 9004 FORMAT (A60)

 9005 FORMAT (A24)

      RETURN
      END
