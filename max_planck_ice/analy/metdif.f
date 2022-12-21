      PROGRAM metdif
C     From Rachel Teboulle's  mldplot.f
      IMPLICIT none

      INCLUDE "icegrid.inc"
      REAL z(L+1, M+1, 34), x(L+1, M+1, 34)
      REAL y(L+1, M+1)

      REAL clevel(34), xlo(34), xhi(34), a, b, c
      INTEGER ihgh, linsol, i, jframe
      CHARACTER*60 fname

      CALL OPNGKS

      PRINT *,'What is the name of the base file?'
      READ (*,9002) fname
      OPEN (UNIT=10, FILE=fname, FORM='UNFORMATTED', 
     1  STATUS='OLD')
      PRINT *,'What is the name of the other file?'
      READ (*,9002) fname
      OPEN (UNIT=11, FILE=fname, FORM='UNFORMATTED', 
     1  STATUS='OLD')
      PRINT *,'How many frames?'
      READ (*,9003) jframe

CD      PRINT *,'What contour interval would you like?'
CD      READ (*,9001) clevel
CD      PRINT *,'What lower bound would you like?'
CD      READ (*,9001) xlo
CD      PRINT *,'What upper bound would you like?'
CD      READ (*,9001) xhi
CD      clevel = 0.
CD      xlo  = 0.
CD      xhi  = 0.
 1111 CONTINUE
        READ (*,*,END=1112) i, a, b, c
        clevel(i-2) = a
        xlo(i-2) = b
        xhi(i-2) = c
        GO TO 1111
 1112 CONTINUE


      ihgh = 0
      linsol = 0
 9003 FORMAT (I3)
      READ (10) x
      READ (11) z
      x = x - z

      DO 9 i = 1, jframe
        IF (i .GE. 14 .AND. i .LE. 27) GO TO 9

        CALL trans(y, x, i)
   
        CALL gselnt (0)
        CALL SET(0.10,0.90,0.10,0.90,0.,FLOAT(L+1),0.,FLOAT(M+1),1)

        CALL CONREC (y,L+1,L+1,M+1,xlo(i),xhi(i),clevel(i),0,
     1                 ihgh,linsol)
 
        CALL FRAME
   9  CONTINUE

      CALL CLSGKS
  
 9001 FORMAT (E13.6)
 9002 FORMAT (A60)
 
      STOP 
      END                                                        
