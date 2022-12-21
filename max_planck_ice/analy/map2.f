      PROGRAM map2
C     Plot a map for a given domain
C     Bob Grumbine
C     LAST MODIFIED 16 August 1994

      IMPLICIT none
                                                                  
      REAL cmin, cmax, bmin, bmax
      INTEGER idot, ier, jproj, latint
      REAL rlat, rlon, rot

      CALL OPNGKS
 
      PRINT *,'Western longitude'
      READ (*,9010) cmin
      PRINT *,'Eastern longitude'
      READ (*,9010) cmax
      PRINT *,'southern latitude'
      READ (*,9010) bmin
      PRINT *,'northern latitude'
      READ (*,9010) bmax
 9010  FORMAT (E13.6)

C -  PLOT CONTOURED DATA --  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
c     prepare data
c     ============
      CALL gselnt (0)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
c
      PRINT *,'lat-long interval'
      READ (*,9001) latint
      PRINT *,'What is the projection type?'
      READ (*,9001) jproj
 9001 FORMAT (I2)
      
      rot  = 0.
      PRINT *,'what is the reference latitude?'
      READ (*,9002) rlat
      PRINT *,'what is the reference longitude?'
      READ (*,9002) rlon
 9002 FORMAT (E13.6)

c     idot=0 for cont. line, 1 for dotted line
      idot = 0
c
      CALL conop1('PER=OFF')

      CALL SUPMAP (jproj,rlat,rlon,rot,
     1   bmin,cmin,bmax,cmax,+2,latint,4,idot,ier)
      if (ier.ne.0) PRINT *,'supmap error=',ier
      PRINT *,'Returned from supmap'

      CALL FRAME
 3000 CONTINUE
 
      CALL CLSGKS
C
C                                                                       
      END                                                         
