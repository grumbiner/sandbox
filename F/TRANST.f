      SUBROUTINE TRANST (KOUT,KBUF,WMOHDR,QUEUE,ITOT,LTOT, 
     1                   lwork, linelen, nlines)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    TRANST      WRITE GRIB MESSAGE TO TRAN FILE.
C   PRGMMR: FARLEY           ORG: W/NMC42   DATE: 86-12-10
C
C ABSTRACT: FORMS WAVE MESSAGES INTO 1280 BYTE PHYSICAL RECORDS AND
C   ADDS THEM TO THE OUTPUT TRANSMISSION.
C
C PROGRAM HISTORY LOG:
C   86-12-10  FARLEY      ORIGINAL AUTHOR
C   93-07-01  GERALD      MODIFIED TO TRANSMIT REGIONAL GULF OF
C                         ALASKA REGIONAL WAVE MODEL OUTPUT.
C   98-01-28  Grumbine    Generalized for use with ice products
C 2000-01-27  Grumbine    New end of message section required for SP
C
C USAGE:    CALL TRANST(KOUT,KBUF,WMOHDR,QUEUE,ITOT,LTOT, lwork,
C            linelen, nlines)
C   INPUT ARGUMENT LIST:
C     KOUT     -  UNIT NUMBER OF TRANSMISSION FILE
C     KBUF     -  TRANSMISSION WORK ARRAY
C     WMOHDR   -  WMO HEADER
C     QUEUE    -  QUEUE DESCRIPTOR
C     ITOT     -  LENGTH OF GRIB MESSAGE  (GRIB - 7777)
C     lwork    -  Integer working space array
C     linelen  -  Number of integers equivalent to the length of a 
C                  line in a WMO message
C     nlines   -  Number of lines in the message
C
C   OUTPUT ARGUMENT LIST:  NONE
C     LTOT     -  TOTAL LENGTH OF BULLETIN QUEDES + WMOHDR +
C                 (GRIB - 7777)
C
C   OUTPUT FILES:
C     FTXXF001 - WMO-encoded grib message.  XX is kout
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90, Standard
C   MACHINE:  Cray
C
C$$$
C

      IMPLICIT none

      INTEGER       KOUT, ITOT, LTOT
      INTEGER linelen, nlines
      INTEGER       lwork(linelen,nlines)
      INTEGER byteint
      PARAMETER (byteint = 4)
C
      CHARACTER*1 KBUF(ITOT)
      CHARACTER*1 QUEUE (*)
      CHARACTER*1 WMOHDR(*)
C 27 January 2000 -- added for SP, note the equivalence is nonstandard
      INTEGER(8)  ENDXTZ(160)
      CHARACTER*1 ENDXTR(1280)
      EQUIVALENCE (ENDXTR(1),ENDXTZ(1))
      DATA  ENDXTZ/Z'E7E3D9D540C5D5C4',159 * Z'4040404040404040'/

C
      INTEGER i, j, LEFT, NEXT, IPUT
C

      NEXT = 0
      CALL W3AI19 (QUEUE ,     80, LWORK, byteint*linelen*nlines, NEXT)
      IF (NEXT .LE. 0) THEN
        PRINT *,'Error in W3AI19, next = ',NEXT
        STOP
      ENDIF

      CALL W3AI19 (WMOHDR,     21, LWORK, byteint*linelen*nlines, NEXT)
      IF (NEXT .LE. 0) THEN
        PRINT *,'Error in W3AI19, next = ',NEXT
        STOP
      ENDIF

      CALL W3AI19 (KBUF, ITOT, LWORK, byteint*linelen*nlines, NEXT)
      IF (NEXT .LE. 0) THEN
        PRINT *,'Error in W3AI19, next = ',NEXT
        STOP
      ENDIF

      LTOT = 80 + 21 + ITOT

      IPUT = LTOT / (linelen*byteint)
      LEFT = MOD(LTOT,(linelen*byteint))
      IF (LEFT .GT. 0) IPUT = IPUT + 1

      DO 100 j = 1,IPUT
        WRITE(kout,REC=j)(LWORK(i,j),i=1,linelen)
  100 CONTINUE

C NEW 27 January 2000 -- need to put out a new end of message record:
      WRITE (kout,REC=j+1) (ENDXTR(i),i=1,1280)

      CLOSE (UNIT=kout,STATUS='KEEP')

      RETURN
      END
