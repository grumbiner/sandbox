 
C*************************************************----------++++++++++!!
      SUBROUTINE setup

      COMMON /contrl/ hem, maxx, d, corr, xpole, ypole 

      INTEGER hem
      REAL maxx, d, corr, xpole, ypole

 
      CHARACTER*40 datfil

   10 PRINT *,' Is this the northern hemisphere (1), or the southern(2),
     1?'
      READ (*,9001) hem
      IF (hem .LT. 1 .OR. hem .GT. 2) GOTO 10

      PRINT *,' What is the name of the data file you want to use?'
      PRINT *,' Please use the full pathname.'
      PRINT *,' The name must be 40 chars or less.    *'
      READ (*,9002) datfil

      OPEN (10,FILE=datfil,STATUS='NEW')

C     Now prepare the graphics
      PRINT *, 'Now preparing the work stations.  This will take about
     1a minute.'
 
      CALL JBEGN

      PRINT *,'Initializing the screen.'
C     Must use hollerith file names here.
      CALL JDINT(2, 40, 40H/usr/local/graphics/work-stations/digwsp,
     1         14, 14H/dev/plt98770b, 0 )
      CALL JWON (2)

      PRINT *,'Turning on the digitizer.'
C     Prepare the digitizer.
      CALL JDINT(1, 40, 40H/usr/local/graphics/work-stations/digwsp,
     1            11, 11H/dev/tablet, 0)
      CALL JWON(1)

      PRINT *,'Enabling the digitizer.'
      CALL JEDEV(1, 4, 11, 11H/dev/tablet)
      CALL JEDEV(1, 2, 11, 11H/dev/tablet)

C     End of setting up.
 
 9001 FORMAT (I1)
     
 9002 FORMAT (A40)

      RETURN

      ENTRY endit
C     Close down everything, save files.

      CLOSE(10,STATUS='KEEP')

      CALL JWOFF(2)
      CALL JWEND(2)

      CALL JDDEV(1, 2)
      CALL JDDEV(1, 4)

      CALL JWOFF(1)
      CALL JWEND(1)

      CALL JEND

      RETURN
      END
