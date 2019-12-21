      PROGRAM diffuse
C     given the salinity field and the forcing, compute the equivalent
C       effective diffusivity which would give the field in the absence
C       of advection.
C     Robert Grumbine 2-23-90.

      IMPLICIT none

      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)
      
      REAL s(nx, ny), qss(nx, ny), qsd(nx, ny)
      REAL h(nx, ny)
      INTEGER tstep, loy, xcen, xlen, ycen, ylen
      REAL delx, dely
      INTEGER strspr, strsum, strfll, strwin
      
      INTEGER i, j, k
      REAL qssum(nx, ny), ahm(nx, ny)
      REAL qsfmax, qsfref, qsm, ahmean
      CHARACTER*60 fname
      REAL secpyr
      PARAMETER (secpyr = 3.1556908E7)

      REAL ahmn, ahmx 
      
      DO 10 j = 1, ny
        DO 20 k = 1, nx
          qssum(k,j) = 0.0
          ahm(k,j)   = 0.0
  20    CONTINUE
  10  CONTINUE
  
      PRINT *,'qsfmax?'
      READ (*,9001) qsfmax
      PRINT *,'qsfref?'
      READ (*,9001) qsfref
      PRINT *,'qsm?'
      READ (*,9001) qsm
 9001 FORMAT (E13.6)
        
      DO 100 i = 1, 600
        IF (MOD(i,15) .EQ. 0) PRINT *,'time step',i
        CALL qsext(qss, qsd, h, nx, ny, i, 600,
     1             540000, 400000, 20000, 40000,
     2             qsfmax, qsfref, qsm,
     3             2.E4, 2.E4, 250, 400, 550, 0)
        DO 110 j = 2, ny-1
          DO 120 k = 2, nx-1
            qssum(k,j) = qssum(k,j) + qss(k,j)
 120      CONTINUE
 110    CONTINUE
 100  CONTINUE
 
      DO 150 j = 1, ny
        DO 160 i = 1, nx
          qssum(k,j) = qssum(k,j)/600./500.
  160   CONTINUE
  150 CONTINUE
  
      PRINT *,'What is the name of the ss file?'
      READ (*,9002) fname
      OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      PRINT *,'What is the name of the diffusivity file?'
      READ (*,9002) fname
      OPEN (12, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
 9002 FORMAT (A60)
      DO 2000 k = 1, 8
        ahmx = 0.0
        ahmn = 0.0
        READ (11) s
        ahmean = 0.0
        DO 1000 j = 2, ny-1
          DO 1100 i = 2, nx-1
            ahm(i,j) = 4.E8*(s(i,j)/secpyr-qssum(i,j))/
     1                    ( (s(i+1,j)-2.*s(i,j)+s(i-1,j)) +
     2                      (s(i,j+1)-s(i,j)*2.+s(i,j-1))     )
            ahmean = ahmean + ahm(i,j)
            ahmx = MAX(ahmx, ahm(i,j))
            ahmn = MIN(ahmn, ahm(i,j))
 1100     CONTINUE
 1000   CONTINUE
        PRINT *,'the mean, max, min of ah was',ahmean/(nx-2)/(ny-2)
     1                                        ,ahmx, ahmn
        WRITE (12) ahm
 2000 CONTINUE
      
      PAUSE
      END
