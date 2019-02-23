C***********************************************************----------!!
      PROGRAM dcheck
C     Program to check the self-consistency of the data, and write
C       out selected portions for graphical, and other analysis.

C     Declare data matrices:
      INTEGER uct(9200), day(9200), month(9200), year(9200)
      REAL speed(9200), uvel(9200), vvel(9200)
      REAL temp(9200), press(9200), cond(9200)
      INTEGER dir(9200)

C     Declare local variables:
      INTEGER unit, n
      CHARACTER*40 fname
      LOGICAL yes

C     Put some of the arrays into extended storage so that they won't
C       cause a stack overflow.
CHP$EMA  uct, day, month, year

C***********************************************************----------!!

C     Read in the data:
      CALL rin(uct, day, month, year, speed, dir, uvel, vvel, 
     1          temp, press, cond, n)

C     Write out the data in unformatted form.
      PRINT *,'What do you want to call the data file?'
      READ (*,9001) fname
      OPEN (20, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')

      WRITE (20) uct
      WRITE (20) day  
      WRITE (20) month  
      WRITE (20) year  
      WRITE (20) speed  
      WRITE (20) dir  
      WRITE (20) uvel  
      WRITE (20) vvel  
      WRITE (20) temp 
      WRITE (20) press  
      WRITE (20) cond  

      CLOSE (20, STATUS='KEEP')

 9001 FORMAT (A40)

      END
C***********************************************************----------!!
