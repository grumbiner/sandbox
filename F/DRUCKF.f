      SUBROUTINE DRUCKF(FELD, FLG, FAK, ADD, HEADER, ICOUNT, NX, NY)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     ?                    MPI, HAMBURG
C     Robert Grumbine      NCEP, Camp Springs, MD                1993
C  LAST MODIFIED: 14 January 1993
C  PURPOSE:
C     -PRINTING OF AN ARRAY BY IGNORING "LAND" POINTS
C  METHOD:
C     -ARRAY TAKEN AS AN INTERNAL FILE
C  INTERFACE:
C     -FELD:   FIELD TO BE PRINTED
C     -FLG:    FLAG FIELD OF MASK
C     -FAK:    MULTIPLICATION FACTOR FOR VARIABLE TO BE SHOWN
C     -ADD:    SUMMATION FACTOR FOR VARIABLE TO BE SHOWN
C     -HEADER: HEADER FOR OUTPUT
C     -ICOUNT: RUNNING TIME STEP
C     -NX:     NUMBER OF GRID POINTS IN X-DIRECTION
C     -NY:     NUMBER OF GRID POINTS IN Y-DIRECTION
C=======================================================================
      INTEGER line_length
      PARAMETER (line_length = 120)
      INTEGER ICOUNT, NX, NY
      REAL FELD(NX,NY), FLG(NX,NY), FAK, ADD
      CHARACTER*3 TEXT(line_length/3)
      CHARACTER HEADER*(*)
      INTEGER NXSTEP, NYSTEP, I, J, JJ, INDEX
      REAL XP
C=======================================================================

      NXSTEP=1+(3*(NX-2)-1)/line_length
      NYSTEP=1+NY/70

      WRITE(16,101)
      WRITE(16,100) ICOUNT,HEADER
      DO 1 J=1,NY-1,NYSTEP
       JJ=NY-J
       INDEX=0
       DO 2 I=3,NX-1,NXSTEP
        INDEX=INDEX+1
        XP=FELD(I,JJ)*FAK+ADD
        IF (XP .LT. 1000. .AND. XP .GT. -99.) THEN 
          WRITE (TEXT(INDEX),200) INT(XP)
        ELSE
CD          WRITE (TEXT(INDEX),205) 
          WRITE (TEXT(INDEX),200) 999
        ENDIF
        IF (FLG(I,JJ).EQ.0.0) THEN
         TEXT(INDEX)='   '
        ELSE IF (XP.LT.0..AND.XP.GT.-1.) THEN
         TEXT(INDEX)=' -0'
        END IF
    2  CONTINUE
       WRITE(16,300) (TEXT(I),I=1,INDEX)
    1 CONTINUE
      RETURN
  100 FORMAT (1H ,5X,' DAY ',I5,'  ARRAY : ',A50)
  101 FORMAT (1H )
  200 FORMAT (I3)
  205 FORMAT ('---')
  300 FORMAT (1X,44A3)
      END
