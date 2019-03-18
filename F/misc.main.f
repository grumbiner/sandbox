      PROGRAM misc 
C     Program to do miscellaneous things with the data
C     Robert Grumbine 27 September 1994

      IMPLICIT none

      REAL x(10000), y(10000), arga(-2500:2500), argb(2500)
      INTEGER size, opt, m
      REAL mult
      INTEGER first, last
      INTEGER len, del
      LOGICAL yes

 2000 CONTINUE
      PRINT *,'How many data points are there?'
      READ (*,9001) size
      
      PRINT *,'Do you want 1 frequency of occurrence '
      PRINT *,'            2 correlation'
      PRINT *,'            3 average subsections of the record'
      PRINT *,'            4 simple filter (A24**2 A25 filter)'
      PRINT *,'            5 compute velocity components given amplitude 
     1 and direction'
      PRINT *,'            6 compute magnitude given two components'
      READ (*,9001) opt

      IF (opt .EQ. 1) THEN
        CALL readin(x, size)
        PRINT *,'What multiplier do you want?'
        READ (*,9002) mult
        PRINT *,'How many different values do you think you have?'
        READ (*,9001) m

        CALL frecmp(x, size, mult, arga, m)

       ELSE IF (opt .EQ. 2) THEN
        PRINT *,'Not implemented yet'

       ELSE IF (opt .EQ. 3) THEN 
        CALL readin(x, size)

        PRINT *,'How many points do you want to analyze at a time?'
        READ (*,9001) len

        PRINT *,'How often do you want to recompute?'
        READ (*,9001) del

        CALL subsec(x, size, len, del)
       ELSE IF (opt .EQ. 4) THEN
         
        CALL filtr

       ELSE IF (opt .EQ. 5) THEN

        PRINT *,'Amplitude file:'
        CALL readin(x, size)

        PRINT *,'Direction file:' 
        CALL readin(y, size)

        CALL velcmp(x, y, size)
       
       ELSE
        PRINT *,'First component:'
        CALL readin (x, size)

        PRINT *,'Second component:'
        CALL readin (y, size)

        CALL pythag(x, y, size)

      ENDIF

      PRINT *,'Would you like to try again?'
      IF (yes(.FALSE.)) GO TO 2000

 9001 FORMAT (I6)

 9002 FORMAT (E13.6)

      END
