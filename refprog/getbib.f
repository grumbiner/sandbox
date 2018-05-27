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
