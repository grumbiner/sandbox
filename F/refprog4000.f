      PROGRAM refer
      
C     Program to search a reference database.
C       BG 10-15-87
C       Merging files added prior to 3-11-88
C       Creating a reference file added 3-11-88.
C       Main program modified to use search, create, merge as subs. 11-9-88
C       Page checking added 11-10-88

C     Parameters of the program.
      INTEGER namlen, tlen, notlen, maxsz
      PARAMETER (namlen  =   24)
      PARAMETER (tlen    =  196)
      PARAMETER (notlen  =   16)
      PARAMETER (maxsz   = 4000)
      
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
          CALL createa(au1, au2, au3, title, jou, vol, page1, page2,
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
      SUBROUTINE aulist(au1, au2, au3, nref, maxsz, npter)

C     Declare the main data structures.
      INTEGER maxsz
      CHARACTER*24  au1(maxsz), au2(maxsz), au3(maxsz)
      INTEGER npter(maxsz), nref(maxsz)
 
C     Local and temporary variables.
      INTEGER maxtmp
      PARAMETER (maxtmp = 4000)
      CHARACTER*24 tau(maxtmp)
      INTEGER ntau(maxtmp)
      INTEGER sumref, numau, j, m 
      CHARACTER*60 fname
      INTEGER i, n
      LOGICAL yes, match

C***********************************************************
   
C     Make list of cited authors 
      j = 1
      tau(1)  = au1(1)
      ntau(1) = nref(1)
      DO 1000 i = 2, maxsz
        IF ( au1(i) .EQ. tau(j) ) THEN
          ntau(j) = ntau(j) + nref(i)
         ELSE
          j = j + 1
          ntau(j) = nref(i)
          tau(j)  = au1(i)
        ENDIF
 1000 CONTINUE
      numau = j

C     Now write out the information.
      CALL tsort(tau, ntau, numau)
      PRINT *,'File name for authors?'
      READ (*,9004) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      WRITE (12, 9001) numau
      sumref = 0
      DO 3000 i = 1, numau
        WRITE(12, 9005) tau(i), ntau(i)
CD      WRITE(* , 9005) tau(i), ntau(i)
        sumref = sumref + ntau(i)
 3000 CONTINUE
      WRITE (12,9001) numau, sumref
      CLOSE (12, STATUS='KEEP')

C************************************************************

 9001 FORMAT (I6)
 
 9004 FORMAT (A60)

 9005 FORMAT (2x,A24,I6) 

      RETURN
      END

      SUBROUTINE auserc(name, au1, n, loc, m)
C     search for a given author

      INTEGER n, m, loc(n)
      CHARACTER*24 name, au1(n)
      
      INTEGER i, blank

      blank = INDEX(name, ' ') -1 
      m = 0
      DO 1000 i = 1, n
        IF ( INDEX( au1(i),name(1:blank) ) .NE. 0) THEN
          m = m + 1
          loc(m) = i
        ENDIF
 1000 CONTINUE

      RETURN
      END
      SUBROUTINE break(outch, brakpt, length)
C     Subroutine to break a long string of output into groups 
C       which don't wrap across line breaks.
C     BG 11-9-88

C     Arguments:
      CHARACTER*350 outch
      INTEGER length, brakpt

C     Local variables:
      INTEGER i, j, k, n
      CHARACTER*350 line
C***********************************************************

C       Make line break at a space
        IF (outch(brakpt:brakpt) .NE. ' ') THEN
CD        PRINT *,'padding?'
          k = 0 
 3000     CONTINUE
          k = k + 1
          IF (outch(brakpt-k:brakpt-k) .NE. ' ') GO TO 3000
          DO 3010 j = 1, brakpt-k 
             line(j:j) = outch(j:j)
 3010     CONTINUE
          DO 3020 i = j, brakpt
            line(i:i) = ' '
 3020     CONTINUE
          DO 3030 j = brakpt+1, 350
            line(j:j) = outch(j-k:j-k)
 3030     CONTINUE
          DO 3040 j = 1, 350-k
            outch(j:j) = line(j:j)
 3040     CONTINUE
          length = length+k
        ENDIF

      RETURN
      END

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
      SUBROUTINE getbib(tau1, tau2, tau3, ttitle, tjou, tvol, 
     1                  tpage1, tpage2, tyear, tnote, tnref)
C     Subroutine to get bibliographic data.

C     Arguments.
      CHARACTER*24 tau1, tau2, tau3, tjou
      CHARACTER*196 ttitle
      INTEGER tvol, tpage1, tpage2, tyear, tnref
      CHARACTER*16 tnote
 
C     Read in the author name(s).
 1000 PRINT *,'first authors name?'
      READ (*,9002, ERR=1000) tau1

 1001 PRINT *,'second authors name?'
      READ (*,9002, ERR=1001) tau2

 1002 PRINT *,'third authors name?'
      READ (*,9002, ERR=1002) tau3

C     Get the title of the paper.
 1003 PRINT *,'title?'
      READ (*,9005, ERR=1003) ttitle

C     Get the journal name.
 1004 PRINT *,'journal name?'
      READ (*,9002, ERR=1004) tjou
	
C     Get the vol #, pages, and year of appearence.
 1010 PRINT *,'volume #?' 
      READ (*,9001, ERR=1010) tvol
 1005 PRINT *,'first page?'
      READ (*,9001, ERR=1005) tpage1
 1006 PRINT *,'last page?'
      READ (*,9001, ERR=1006) tpage2
 1007 PRINT *,'year?'
      READ (*,9001, ERR=1007) tyear 

C     Get any notes.
 1008 PRINT *,'Add an up to 16 character note.'
      READ (*,9004, ERR=1008) tnote

 1009 PRINT *,'How many times has this been referenced?'
      READ (*,9001, ERR=1009) tnref
      tnref = MAX0(1, tnref)

C***********************************************************

CD    Now echo out the information.
CD    PRINT *,'The info you entered was:'
CD    WRITE (*, 9003) tau1, tau2, tau3, ttitle, tjou,
CD   1  tvol, tpage1, tpage2, tyear, tnote, tnref
CD9003 FORMAT (3A24/,A196/,A24,4I5,1X,A16,I4)
 
 9001 FORMAT (I5)

 9002 FORMAT (A24)
 
 9004 FORMAT (A16)
 
 9005 FORMAT (A196)

      RETURN 
      END
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

      SUBROUTINE jlist(jou, nref, maxsz, npter)

C     Parameters of the program.
      INTEGER namlen, tlen, notlen, maxsz
      PARAMETER (namlen =   24)
      PARAMETER (tlen   =  196)
      PARAMETER (notlen =   16)
      
C     Declare the main data structures.
      CHARACTER*24  jou(maxsz)
      INTEGER npter(maxsz), nref(maxsz)
      
C     Local and temporary variables.
      INTEGER nloc
      PARAMETER (nloc = 4000)
      CHARACTER*24 tjou(nloc)
      INTEGER njou(nloc)
      INTEGER sumref, numau, numjou, j, m 
      CHARACTER*60 fname
      INTEGER i, n, nmin
      LOGICAL yes, match

C***********************************************************   
C     Make list of cited journals:
      m = 1
      tjou(1) = jou(1)
      njou(1) = nref(1)
      DO 2000 i = 2, maxsz
        match = .FALSE.
        DO 2100 j = 1, m
          IF (jou(i) .EQ. tjou(j)) THEN
            njou(j) = njou(j) + nref(i)
            match = .TRUE.
          ENDIF
 2100   CONTINUE
        IF (.NOT. match) THEN
          m = m + 1
          tjou(m) = jou(i)
          njou(m) = nref(i)
        ENDIF
 2000 CONTINUE
      numjou = m 

      CALL tsort(tjou, njou, numjou)
      PRINT *,'File name for journals?'
      READ (*,9004) fname
      OPEN (13, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      WRITE (13, 9001) numjou
      DO 3100 i = 1, numjou
        WRITE (13, 9005) tjou(i), njou(i)
 3100 CONTINUE
      CLOSE (13, STATUS='KEEP')
C***********************************************************

 9001 FORMAT (I6)
 
 9004 FORMAT (A60)

 9005 FORMAT (2x,A24,I6) 

      RETURN
      END
      SUBROUTINE kyserc(wd, title, n, loc, m)
C     Search for a keyword in the title.
      
      INTEGER n, m, loc(n)
      CHARACTER*24 wd
      CHARACTER*196 title(n)
 
      INTEGER i, blank
      
      blank = INDEX(wd,' ') -1 
      m = 0
      DO 1000 i = 1, n
        IF (INDEX(title(i) , wd(1:blank) ) .NE. 0) THEN
          m = m + 1
          loc(m) = i
        ENDIF
 1000 CONTINUE

      RETURN
      END

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
      SUBROUTINE newau(au1, au2, au3, title, jou, vol, 
     1                    page1, page2, year, note, nref,
     2                 tau1, tau2, tau3, ttitle, tjou, tvol,
     3                    tpage1, tpage2, tyear, tnote, tnref,
     4                 posit, n)
      
C     Subroutine to insert a new author into the current list.

C     Parameters of the program.
      INTEGER namlen, tlen, notlen, maxsz
      PARAMETER (namlen =  24)
      PARAMETER (tlen   = 196)
      PARAMETER (notlen =  16)
      PARAMETER (maxsz  = 4000)
      
C     Declare the main data structures.
      CHARACTER*24  au1(maxsz), au2(maxsz), au3(maxsz)
      CHARACTER*196 title(maxsz)
      CHARACTER*24  jou(maxsz)
      INTEGER vol(maxsz), page1(maxsz), page2(maxsz), year(maxsz)
      CHARACTER*16 note(maxsz)
      INTEGER nref(maxsz)
      
C     Local and temporary variables.
      CHARACTER*24 tau1, tau2, tau3, tjou
      CHARACTER*196 ttitle
      INTEGER tvol, tpage1, tpage2, tyear, tnref
      CHARACTER*16 tnote
 
      INTEGER i, n, posit
      LOGICAL yes
C***********************************************************----------!!

C       All entries >= posit must be bumped up one, and N increased by 1.
        DO 1000 i = n, posit, -1
          au1(i+1)   = au1(i)
          au2(i+1)   = au2(i)
          au3(i+1)   = au3(i)
          title(i+1) = title(i)
          jou(i+1)   = jou(i)
          vol(i+1)   = vol(i)
          page1(i+1) = page1(i)
          page2(i+1) = page2(i)
          year(i+1)  = year(i)
          note(i+1)  = note(i)
          nref(i+1)  = nref(i)
 1000   CONTINUE
        au1(posit)   = tau1
        au2(posit)   = tau2
        au3(posit)   = tau3
        title(posit) = ttitle
        jou(posit)   = tjou
        vol(posit)   = tvol
        page1(posit) = tpage1
        page2(posit) = tpage2
        year(posit)  = tyear
        note(posit)  = tnote
        nref(posit)  = tnref

        n = n + 1

C***********************************************************----------!!

      RETURN 
      END

      SUBROUTINE oldau(tau1, tau2, tau3, ttitle, tjou, tvol, 
     1                 tpage1, tpage2, tyear, tnote, tnref,
     2                  au1, au2, au3, title, jou, vol, 
     3                  page1, page2, year, note, nref,
     4                  n, dup, posit ) 
      
C     Subroutine to test whether the author is already on the list.

      INTEGER n 
C     Declare the main data structure.
      CHARACTER*24  au1(n), au2(n), au3(n)
      CHARACTER*196 title(n)
      CHARACTER*24  jou(n)
      INTEGER vol(n), page1(n), page2(n), year(n)
      CHARACTER*16 note(n)
      INTEGER nref(n)
      
C     Arguments
      CHARACTER*24 tau1, tau2, tau3, tjou
      CHARACTER*196 ttitle
      INTEGER tvol, tpage1, tpage2, tyear, tnref
      CHARACTER*16 tnote
      INTEGER posit

C     Local, temporary variables. 
      INTEGER i
      LOGICAL newpap, yes, dup
      INTEGER upper, lower, m
 

C***********************************************************----------!!
        dup    = .FALSE.
        newpap = .FALSE.
        posit  = 1
 1000   CONTINUE
          IF (tau1 .GT. au1(posit) .AND. posit .LT. n) THEN
            posit     = posit + 1
            GO TO 1000
           ELSE
            IF (posit .EQ. n) THEN
C             Must be new author at the end of the alphabet.
              newpap = .TRUE.
              dup    = .FALSE.
             ELSE IF ( tau1 .EQ. au1(posit)) THEN
C             duplicate author name
              dup = .TRUE.
             ELSE
C             Must be new author in middle of the alphabet.
              newpap = .TRUE.
              dup    = .FALSE.
            ENDIF
          ENDIF

C***********************************************************----------!!

C       If this may be a duplication, find the limits on the author's
C         entries, and check further interactively.
        IF (.NOT. dup) GO TO 2000
        lower = posit
        upper = posit
 1200   IF (tau1 .EQ. au1(upper+1)) THEN
          upper = upper + 1
          GO TO 1200
        ENDIF

C       If the years match between the new entry and any of the old
C         entries, ask if the new is a duplicate.
        dup = .FALSE.
        DO 1300 i = lower, upper
          IF (tyear .EQ. year(i)) THEN
            PRINT *,'Possible match.'
            PRINT *,'The current entry is:'
            WRITE (*,9003) au1(i), au2(i), au3(i), title(i),  
     1                   jou(i), vol(i), page1(i), page2(i), 
     2                   year(i), note(i), nref(i)
            PRINT *,'The candidate is:'
            WRITE (*,9003) tau1, tau2, tau3, ttitle, tjou, tvol,
     1         tpage1, tpage2, tyear, tnote, tnref
            PRINT *,'Are these the same?'
            IF (yes(.FALSE.)) THEN
              dup = .TRUE.
              PRINT *,'Is the old entry completely correct?'
              IF (yes(.TRUE.)) THEN
                nref(i) = nref(i) + MAX0(1, tnref)
               ELSE
                PRINT *,'Is the new entry completely correct?'
                IF (yes(.TRUE.)) THEN
                  au1(i)   = tau1
                  au2(i)   = tau2
                  au3(i)   = tau3
                  title(i) = ttitle
                  jou(i)   = tjou
                  vol(i)   = tvol
                  page1(i) = tpage1
                  page2(i) = tpage2  
                  note(i)  = tnote
                  nref(i)  = nref(i) + tnref
                 ELSE
                  PRINT *,'Re-enter the info.'
                  CALL getbib(  au1(i), au2(i), au3(i), title(i),
     1              vol(i), page1(i), page2(i), year(i), note(i),
     2              nref(i) ) 
                ENDIF
              ENDIF
              GO TO 2000 
            ENDIF
          ENDIF
 1300   CONTINUE
C***********************************************************----------!!

 2000   CONTINUE

C***********************************************************----------!!

 
 9003 FORMAT (3A24,/,A196,/,A24,4I5,1X,A16,I4)

      RETURN 
      END

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
      SUBROUTINE pageck(vol, page1, page2, year, note, npap, change)
C     Check the page numbers for reasonability, modify if requested.
C       BG 11-10-88

      INTEGER npap
      INTEGER vol(npap), page1(npap), page2(npap), year(npap)
      CHARACTER*16 note(npap)

      INTEGER i
      LOGICAL yes, change

      change = .FALSE.
      DO 1000 i = 1, npap
        IF (page1(i) .GT. page2(i) .OR. page1(i) .EQ. 0) THEN
C         negative paper length or no page numbers
          PRINT *,'Change anything? [n]'
          IF (yes(.FALSE.)) THEN
            PRINT *,'Set page2 = page1? [y]'
            IF (yes(.TRUE.)) THEN
              page2(i) = page1(i)
             ELSE
              WRITE (*,9001) vol(i), page1(i), page2(i),
     1                                  year(i), note(i)
                  PRINT *,'Change volume number?'
              IF (yes(.FALSE.)) THEN
                PRINT *,'New volume number:'
                READ (*,9002) vol(i)
                change = .TRUE.    
              ENDIF
              PRINT *,'Change page1?'
              IF (yes(.FALSE.)) THEN
                PRINT *,'New first page:'
                READ (*,9002) page1(i)
                change = .TRUE.    
              ENDIF
              PRINT *,'Change page2?'
              IF (yes(.FALSE.)) THEN
                PRINT *,'New final page:'
                READ (*,9002) page2(i)
                change = .TRUE.    
              ENDIF
              PRINT *,'Change year?'
              IF (yes(.FALSE.)) THEN
                PRINT *,'New year:'
                READ (*,9002) year(i)
                change = .TRUE.    
              ENDIF
              PRINT *,'Change note?'
              IF (yes(.FALSE.)) THEN
                PRINT *,'New note:'
                READ (*,9003) note
                change = .TRUE.    
              ENDIF
  
            ENDIF
          ENDIF
        ENDIF

 1000 CONTINUE
     
 9001 FORMAT  ('  Vol.  Page1 Page2 Year     Note',/,4I6,2X,A16)

 9002 FORMAT (I5)

 9003 FORMAT (A16)

      RETURN
      END

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
      PARAMETER (nloc = 4000)
      CHARACTER*60 fname
      INTEGER i, j, m, n
      INTEGER gtype, stype
      INTEGER double, posit
      LOGICAL dup, newpap, yes
      INTEGER loc(nloc), eyear, lyear, ncite
      CHARACTER*24 auname, jname, keywd

 
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
      SUBROUTINE start(au1, au2, au3, title, jou, vol, 
     1                 page1, page2, year, note, nref , npap, maxsz)
      
C     Subroutine to open up files, and read in data if requested.
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
      SUBROUTINE tsort(name, nref, n)
C     Sort author or journal names by the number of citations
C       to them.
      INTEGER n
      INTEGER nref(n)
      CHARACTER*24 name(n)

      INTEGER i, step
      INTEGER tn
      CHARACTER*24 tname
      LOGICAL change
 
      step = n/2
 100  CONTINUE
      change = .FALSE.
      DO 1000 i = 1, n-step
        IF (nref(i+step) .GT. nref(i)) THEN
          tn           = nref(i)
          nref(i)      = nref(i+step)
          nref(i+step) = tn
          tname        = name(i)
          name(i)      = name(i+step)
          name(i+step) = tname
          change = .TRUE.
        ENDIF
 1000 CONTINUE
      IF (change) THEN
        GO TO 100
       ELSE IF (step .GT. 1) THEN
        step = step/2
        GO TO 100
       ELSE
C       finished
      ENDIF

      RETURN
      END
      SUBROUTINE years(ymin, ymax, year, n, loc, m)
C     find papers published between given years.

      INTEGER ymin, ymax, n, m
      INTEGER year(n), loc(n)

      INTEGER i

      m = 0
      DO 1000 i = 1, n
        IF ( (year(i) .GE. ymin) .AND. (year(i) .LE. ymax) ) THEN
          m = m + 1
          loc(m) = i
        ENDIF
 1000 CONTINUE

      RETURN
      END
      SUBROUTINE nmin(ncite, nref, n, loc, m)
C     find papers which have been cited at least the given number of times.

      INTEGER n, m, loc(n), ncite
      INTEGER nref(n)

      INTEGER i

      m = 0
      DO 1000 i = 1, n
        IF (nref(i) .GE. ncite) THEN
          m = m + 1
          loc(m) = i
        ENDIF
 1000 CONTINUE

      RETURN
      END
      FUNCTION yes(defalt)
C     Function to return .TRUE. if the user responds y, .FALSE. if he 
C       says n, and the default value otherwise.

      LOGICAL yes, defalt
      CHARACTER resp

      READ (*,9001) resp
 9001 FORMAT(A1)

      yes = (resp.EQ.'y') .OR. (defalt .AND. resp.NE.'y'
     1                                 .AND. resp.NE.'n')

      RETURN
      END