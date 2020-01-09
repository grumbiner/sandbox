C***********************************************************----------!!
      SUBROUTINE sread(taux, tauy, nlong, nlat)
C     Routine to read in wind stress data from the Hellerman 2 degree
C      data set
C     Robert Grumbine 27 Sep 1995

      IMPLICIT none

      INTEGER nlong, nlat
      REAL taux(nlong, nlat), tauy(nlong, nlat)
      
      INTEGER i, j
      CHARACTER*60 fname
      REAL maxmag
      
      PRINT *,'What is the name of the hellerman stress file?'
      READ (*,9001) fname
 9001 FORMAT (A60)
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      
      READ (10,9001) fname
      DO 1000 j = 1, nlat
        READ (10,9002) (taux(i,j),i=1,nlong)
 1000 CONTINUE
 
      READ (10,9001) fname
      DO 1100 j = 1, nlat
        READ (10,9002) (tauy(i,j),i=1,nlong)
 1100 CONTINUE
 
 9002 FORMAT (15F8.4)
 
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
  
      RETURN
      END      
      SUBROUTINE start(au1, au2, au3, title, jou, vol, 
     1                 page1, page2, year, note, nref , npap, maxsz)
      
C     Subroutine to open up files, and read in data if requested.
      INTEGER maxsz

C     Declare the arguments.
      INTEGER npap
      CHARACTER*24  au1(maxsz), au2(maxsz), au3(maxsz)
      CHARACTER*196 title(maxsz)
      CHARACTER*24  jou(maxsz)
      INTEGER vol(maxsz), page1(maxsz), page2(maxsz), year(maxsz)
      CHARACTER*16 note(maxsz)
      INTEGER nref(maxsz)
      
      CHARACTER*60 fname
      INTEGER i
      LOGICAL yes
 
C***********************************************************----------!!

C     Read in an old file:
      PRINT *,'What is the name of the file?'
      READ (*,9004) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      READ (10, 9001) npap
      DO 100 i = 1, npap
        READ (10, 9003) au1(i), au2(i), au3(i), title(i), jou(i),
     1   vol(i), page1(i), page2(i), year(i), note(i), nref(i)
C       WRITE (*,9003) au1(i), au2(i), au3(i), title(i), jou(i), 
C    1   vol(i), page1(i), page2(i), year(i), note(i), nref(i)
 100  CONTINUE

      CLOSE (10, STATUS='KEEP')

C***********************************************************----------!!

 9001 FORMAT (I6)
 
 9003 FORMAT (3A24,/,A196,/,A24,4I5,1X,A16,I4)

 9004 FORMAT (A60)

      RETURN
      END
