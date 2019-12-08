C***********************************************************----------!!
      SUBROUTINE sread(taux, tauy, nlong, nlat)
C     Routine to read in wind stress data from the Han+Lee data set.
      INTEGER nlong, nlat
      REAL taux(nlong, nlat), tauy(nlong, nlat)
      
      INTEGER i, j
      CHARACTER*60 fname
      REAL maxmag
CD      SAVE taux, tauy
      
      PRINT *,'What is the name of the han+lee tau-x file?'
      READ (*,9001) fname
 9001 FORMAT (A60)
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      PRINT *,'What is the name of the han+lee tau-y file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      
      READ (10,9001) fname
      DO 1000 j = 1, nlat
        READ (10,9002) (taux(i,j),i=1,nlong)
 1000 CONTINUE
 
      READ (11,9001) fname
      DO 1100 j = 1, nlat
        READ (11,9002) (tauy(i,j),i=1,nlong)
 1100 CONTINUE
 
 9002 FORMAT (8x,72F6.3)
 
C     Rescale to N/m2
      DO 2000 j = 1, nlat
        DO 2010 i = 1, nlong
          taux(i,j) = taux(i,j)*0.1
          tauy(i,j) = tauy(i,j)*0.1
 2010   CONTINUE
 2000 CONTINUE
 
      maxmag = 0.0
      DO 3200 j = 1, nlat
        DO 3210 i = 1, nlong
          maxmag = max(maxmag,ABS(tauy(i,j)))
 3210   CONTINUE
 3200 CONTINUE
      PRINT *,'The tauy magnitude extremum is',maxmag
 
C     Replace Han+Lee 9.99 values with zero 
      DO 3000 j = 1, nlat
        DO 3010 i = 1, nlong
          IF (taux(i,j) .GE. 9.9) taux(i,j) = 0.0
          IF (tauy(i,j) .GE. 9.9) tauy(i,j) = 0.0
 3010   CONTINUE
 3000 CONTINUE
 
      RETURN
      END	
