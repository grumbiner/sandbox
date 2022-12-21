      PROGRAM tsmap
C     Read in the Great Lakes grid and write out fields for
C       ice forecast model.  Need to flip the vertical.
C     Bob Grumbine 9 July 1995.

      IMPLICIT none

      INTEGER NX_GLK, NY_GLK, ILAND, IWATER, OLAND, OWATER
      PARAMETER (NX_GLK = 85)
      PARAMETER (NY_GLK = 85)
      PARAMETER (ILAND  =  1)
      PARAMETER (IWATER =  0)
      PARAMETER (OLAND  =  0)
      PARAMETER (OWATER =  1)

      CHARACTER imask(NY_GLK,NX_GLK)
      INTEGER  omask(NY_GLK,NX_GLK)
      REAL tshal(NY_GLK,NX_GLK)
      REAL sshal(NY_GLK,NX_GLK)
      REAL tdeep(NY_GLK,NX_GLK)
      REAL sdeep(NY_GLK,NX_GLK)
      REAL bathy(NY_GLK,NX_GLK)

      INTEGER i, j
  
c     External assign
      READ (10) imask

      DO 1000 j = 1, NY_GLK
        DO 1100 i = 1, NX_GLK
          IF (ICHAR(imask(i, NY_GLK - j + 1) )  .EQ. ILAND ) THEN
            omask(i,j)  = (OLAND)
            tshal(i,j)  = 0.0 
            tdeep(i,j)  = 0.0 
            sshal(i,j)  = 0.0 
            sdeep(i,j)  = 0.0 
            bathy(i,j)  = 0.0 
          ELSE
            omask(i,j)  = (OWATER)
            tshal(i,j)  =  10.0 
            tdeep(i,j)  =   4.0 
            sshal(i,j)  =   0.0 
            sdeep(i,j)  =   0.0 
            bathy(i,j)  = 200.0 
          ENDIF
 1100   CONTINUE
 1000 CONTINUE
    
      WRITE (11) tshal
      WRITE (11) sshal
      WRITE (12) tdeep
      WRITE (12) sdeep

      WRITE (13) bathy

      DO 2000 j = 1, 84
        WRITE (14, 9001) (omask(i,j),i=1,84)
 2000 CONTINUE
      DO 2100 j = 1, 85
        WRITE (14, 9002) (omask(i,j),i=1,85)
 2100 CONTINUE
      DO 2200 j = 1, 85
        WRITE (14, 9002) (omask(i,j),i=1,85)
 2200 CONTINUE
 9001 FORMAT (84I1)
 9002 FORMAT (85I1)

      STOP
      END
