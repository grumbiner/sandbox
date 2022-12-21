      PROGRAM metplt
C     Simultaneous contour plot of all the meteorological fields
C       used by the ice model.
C     Uses NCAR Graphics
C     Robert Grumbine 16 June 1994.

      IMPLICIT none
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL UWIN(L,M),VWIN(L,M)
      REAL TAIR(0:L,0:M), TD(0:L,0:M), PA(0:L,0:M)
     1  ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      REAL LWUP(0:L,0:M), LWDN(0:L,0:M), SWDN(0:L,0:M)
C=======================================================================
      REAL rcont, xlo, xhi
      INTEGER ihgh, linsol, i, j, jframe, fstep
      CHARACTER*60 fname

      PRINT *,'What is the name of the meteorological data file?'
      READ (*,9002) fname
      OPEN (UNIT=20, FILE=fname, FORM='UNFORMATTED', 
     1  STATUS='OLD')

      rcont = 0.
      xlo = 0.
      xhi = 0.
      ihgh = 0
      linsol = 0
      PRINT *,'How many steps?'
      READ (*,9003) jframe
      PRINT *,'Start with which step?'
      READ (*,9003) fstep
 9003 FORMAT (I3)
      CALL OPNGKS

      IF (fstep .NE. 1) THEN
        DO 1000 j = 1, fstep-1
          READ (20) TAIR
          READ (20) PA
          READ (20) TD
          READ (20) SWDN
          READ (20) LWDN
          READ (20) LWUP
          READ (20) RPREC
          READ (20) UWIN
          READ (20) VWIN
 1000   CONTINUE
      ENDIF


      DO 2000 i = 1, jframe
          READ (20) TAIR
          READ (20) PA
          READ (20) TD
          READ (20) SWDN
          READ (20) LWDN
          READ (20) LWUP
          READ (20) RPREC
          READ (20) UWIN
          READ (20) VWIN

        CALL gselnt (0)
        CALL SET(0.10,0.90,0.10,0.90,0.,FLOAT(L+1),0.,FLOAT(M+1),1)

        rcont = 2.5
        xlo = -50.
        xhi = 40.
        CALL CONREC (TAIR,L+1,L+1,M+1,xlo,xhi,rcont,0,ihgh,linsol)
        CALL FRAME
        rcont = 0.
        xlo = 90000.
        xhi = 105000.
        CALL CONREC (PA,L+1,L+1,M+1,xlo,xhi,rcont,0,ihgh,linsol)
        CALL FRAME
  
        rcont = 5.
        xlo = 0.
        xhi = 105.
        CALL CONREC (TD,L+1,L+1,M+1,xlo,xhi,rcont,0,ihgh,linsol)
        CALL FRAME
        rcont = 25.
        xlo = 0.
        xhi = 675.
        CALL CONREC (SWDN,L+1,L+1,M+1,xlo,xhi,rcont,0,ihgh,linsol)
        CALL FRAME
        rcont = 25.
        xlo =    0.
        xhi =  500.
        CALL CONREC (LWDN,L+1,L+1,M+1,xlo,xhi,rcont,0,ihgh,linsol)
        CALL FRAME
        rcont = 25.
        xlo =    0.
        xhi =  500.
        CALL CONREC (LWUP,L+1,L+1,M+1,xlo,xhi,rcont,0,ihgh,linsol)
        CALL FRAME
 
 2000 CONTINUE


      CALL CLSGKS
 9001 FORMAT (E13.6)
 9002 FORMAT (A60)
 
      STOP
      END
