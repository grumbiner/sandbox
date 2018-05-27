      SUBROUTINE pageck(vol, page1, page2, year, note, npap, change)
C     Check the page numbers for reasonability, modify if requested.
C       Robert Grumbine 11-10-88

      IMPLICIT none

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
