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
