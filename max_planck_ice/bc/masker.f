      PROGRAM masker
C     Create land-sea mask for ice models
C     Take as input (user selected)
C       1) MRF flux file slimsk
C       2) NESDIS Terrain file
C       3) ZMASK 1x1 file
C     Interpolate to the ice model grid (0:L)
C     Then clean up the masks (remove isolated lakes, ...
C     Create the 1:L grid
C     Put in the required buffer zones.
C     Use bathymetry in constructing file BG 9 November 1995.

      IMPLICIT none

      INCLUDE "icegrid.inc"
      REAL rmask(0:L,0:M), bathy(0:L, 0:M)
      INTEGER imask(0:L,0:M), inmask(L,M)
      REAL global(360, 181)
      INTEGER cglob(LP, MP)

      INTEGER pole, tlat, tlong, terr
      INTEGER i, j
      CHARACTER*60 fname
      CHARACTER*12 form, formp

C     Select the data type
CD      PRINT *,'Which type of terrain file are you using?'
CD      PRINT *,'   1) MRF flux file slimsk'
CD      PRINT *,'   2) NESDIS Terrain file'
CD      PRINT *,'   3) ZMASK 1x1 file'
 9000 READ (*,9001) terr
 9001 FORMAT (77I1)
      PRINT *,'Preparing to decide land type terr = ',terr
 9004 FORMAT (A60)

      OPEN(11, FILE='maskout', FORM='FORMATTED', STATUS='NEW')
      OPEN(12, FILE='maskfull', FORM='FORMATTED', STATUS='NEW')
      OPEN(13, FILE='bathy', FORM='UNFORMATTED', STATUS='OLD')
      OPEN(14, FILE='unit14', FORM='UNFORMATTED', STATUS='NEW')

      READ (13) bathy

      PRINT *,'Preparing to test land type terr = ',terr
      IF (terr .EQ. 1) THEN
        PRINT *,'Not currently prepared to read slimsk'
        STOP  
       ELSE IF (terr .EQ. 2) THEN
        PRINT *,'Not currently prepared to read NESDIS terrain file'
        STOP  
       ELSE IF (terr .EQ. 3) THEN
        PRINT *,'In terr = 3'
        tlong = 360
        tlat  = 181
        READ (*,9004) fname
        WRITE (*,9004) fname
        OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
        DO 2000 j = 1, tlat
          READ (10, 9003) (global(i,j),i=1,360)
 2000   CONTINUE
 9003   FORMAT (72F1.0)
CD        READ (10) global
        DO 1000 j = 1, tlat
          DO 1100 i = 1, tlong
            IF (global(i,j) .EQ. 1.) THEN
              global(i,j) = 0.
             ELSE IF (global(i,j) .EQ. 0.) THEN
              global(i,j) = 1.
             ELSE 
              global(i,j) = 0.
            ENDIF
 1100     CONTINUE
 1000   CONTINUE
        IF (LATMIN .GT. 0) THEN
          pole = 1
         ELSE 
          pole = 2
        ENDIF 
C       Interpolate
        CALL terph(global, rmask, pole, .FALSE.)


       ELSE IF (terr .EQ. 4) THEN
C       Use a reduced version of the ssmi land mask file
        READ (*,9004) fname
        WRITE (*,9004) fname
        OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
        DO 1199 j = 1, MP
          READ (10,9001) (cglob(i,j),i=1,LP)
          WRITE (*,9001) (cglob(i,j),i=1,LP)
 1199   CONTINUE
        DO 1200 j = 1, MP
          DO 1300 i = 1, LP
            IF (cglob(i,j) .EQ. 1) THEN
              rmask(i,j) = 1.0
            ELSE
              rmask(i,j) = 0.0
            ENDIF
 1300     CONTINUE
 1200   CONTINUE

       ELSE
        PRINT *,'Not a legal terrain type'
        GO TO 9000
      ENDIF
     
C     Clean the mask and return the integral masks
C     12 specifies the unit number to write out the land mask file
C       prior to making the boundary modifications.
C       For use by other people in reading my fields.
      CALL mclean(rmask, bathy, L, M, imask, inmask, 12)


C     Write out the masks
      WRITE (form, 9002) L
      WRITE (formp, 9002) LP

      DO 6000 j = 1, M
CD        WRITE (11,9002) (inmask(i,j),i=1,L)
        WRITE (11,form) (inmask(i,j),i=1,L)
 6000 CONTINUE
      DO 6100 j = 0, M
CD        WRITE (11,9002) (imask(i,j),i=0,L)
        WRITE (11,formp) (imask(i,j),i=0,L)
 6100 CONTINUE     
      DO 6110 j = 0, M
CD        WRITE (11,9002) (imask(i,j),i=0,L)
        WRITE (11,formp) (imask(i,j),i=0,L)
 6110 CONTINUE     

CD 9002 FORMAT (95I1)
 9002 FORMAT ( '(', I3, 'I1)' )

      DO 7000 j = 0, M
      DO 7000 i = 0, L
        rmask(i,j) = imask(i,j)
 7000 CONTINUE
      WRITE (14) rmask

      CLOSE (11, STATUS='KEEP')
      CLOSE (12, STATUS='KEEP')

      STOP
      END
