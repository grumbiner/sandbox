      PROGRAM refer
      
C     Program to search a reference database.
C       Robert Grumbine 10-15-87
C       Merging files added prior to 3-11-88
C       Creating a reference file added 3-11-88.
C       Main program modified to use search, create, merge as subs. 11-9-88
C       Page checking added 11-10-88

      IMPLICIT none

C     Parameters of the program.
      INTEGER namlen, tlen, notlen, maxsz
      PARAMETER (namlen  =   24)
      PARAMETER (tlen    =  196)
      PARAMETER (notlen  =   16)
      PARAMETER (maxsz   = 2400)
      
C     Declare the main data structures.
      CHARACTER*24  au1(maxsz), au2(maxsz), au3(maxsz)
      CHARACTER*196 title(maxsz)
      CHARACTER*24  jou(maxsz)
      INTEGER vol(maxsz), page1(maxsz), page2(maxsz), year(maxsz)
      CHARACTER*16 note(maxsz)
      INTEGER nref(maxsz)
      
C     Local and temporary variables.
      CHARACTER*60 fname
      INTEGER npap
      INTEGER i, j, m, n, unit
      INTEGER gtype, stype
      INTEGER double, posit
      LOGICAL dup, newpap, yes, change
      INTEGER loc(maxsz), eyear, lyear, ncite
      CHARACTER*24 auname, jname, keywd
      CHARACTER*40 pfmt
CHP $EMA title
 
C***********************************************************----------!!
 10   CONTINUE
        PRINT *,'Which type of operation would you like?'
        PRINT *,'  1 = Create or add directly to a reference file'
        PRINT *,'  2 = Merge reference files '
        PRINT *,'  3 = Search reference files '
        PRINT *,'  4 = Make list of have (not) read papers, with min num
     1ber of'
        PRINT *,'       citations'
        PRINT *,'  5 = Create a list of citations per author'
        PRINT *,'  6 = Create a list of citations per journal'
        PRINT *,'  7 = Print out a file in short form'
        PRINT *,'  8 = Check page numbers'
        READ (*,9001) gtype
        IF (gtype .LT. 1 .OR. gtype .GT. 8) GO TO 10

        IF (gtype .EQ. 1) THEN
          CALL create(au1, au2, au3, title, jou, vol, page1, page2,
     1                 year, note, nref, maxsz, npap)
         ELSE IF (gtype .EQ. 2) THEN
          CALL merge(au1, au2, au3, title, jou, vol, page1, page2,
     1                 year, note, nref, maxsz, npap)
         ELSE IF (gtype .EQ. 3) THEN
          CALL search(au1, au2, au3, title, jou, vol, page1, page2,
     1                 year, note, nref, maxsz, npap)
         ELSE IF (gtype .EQ. 4) THEN
          CALL start(au1, au2, au3, title, jou, vol, page1, page2, 
     1                 year, note, nref, npap, maxsz)
          CALL hread(au1, au2, au3, title, jou, vol, page1, page2, 
     1                 year, note, nref, maxsz, npap)
         ELSE IF (gtype .EQ. 5) THEN
          CALL start(au1, au2, au3, title, jou, vol, page1, page2, 
     1                 year, note, nref, npap, maxsz)
          CALL aulist(au1, au2, au3, nref, npap, loc) 
         ELSE IF (gtype .EQ. 6) THEN
          CALL start(au1, au2, au3, title, jou, vol, page1, page2, 
     1                 year, note, nref, npap, maxsz)
          CALL jlist(jou, nref, npap, loc) 
         ELSE IF (gtype .EQ. 7) THEN
          CALL start(au1, au2, au3, title, jou, vol, page1, page2, 
     1                 year, note, nref, npap, maxsz)
          PRINT *,'What file name would you like for the short output?'
          READ (*,9004) fname
          OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')
          pfmt = '(3A24,A196,A24,4I5,1X,A16,I4)'
          DO 1000 i = 1, npap
            CALL short(au1(i), au2(i), au3(i), title(i), jou(i), 
     1                 vol(i), page1(i), page2(i), year(i), 
     2                 note(i), nref(i), pfmt, 12)
 1000     CONTINUE
          CLOSE (12)
         ELSE
          CALL start(au1, au2, au3, title, jou, vol, page1, page2, 
     1                 year, note, nref, npap, maxsz)
          CALL pageck(vol, page1, page2, year, note, npap, change) 
          IF (change) 
     1      CALL out(au1, au2, au3, title, jou, vol, page1, page2,
     2                 year, note, nref, maxsz, npap)
        ENDIF
 
C***********************************************************----------!!

      PRINT *,'Would you like to use a different function now?'
      IF (yes(.FALSE.)) GO TO 10 

C***********************************************************----------!!

 9001 FORMAT (I6)
 
 9003 FORMAT (3A24,/,A196,/,A24,4I5,1X,A16,I4)

 9004 FORMAT (A60)

      END
