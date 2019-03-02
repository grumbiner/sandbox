      PROGRAM winterp
C     Read in temperature data from fixed moorings and interpolate to even grid.
      INTEGER npts, ndepths, ninterp
      PARAMETER (npts = 90)
      PARAMETER (ndepths = 9)
      PARAMETER (ninterp = 60)
      
      INTEGER i, j, k, jmax
      CHARACTER*60 fname
      CHARACTER*1 ncsa(npts*ninterp)
      REAL temper(npts,ndepths)
      INTEGER d(ndepths+1), dupper
      REAL interp(npts,ninterp)
      REAL LINEAR, tlower, tupper
      
      LINEAR(j,k) = temper(i,k) + ( temper(i,k+1)-temper(i,k) )
     1             * FLOAT( 5*(j-1)-d(k) ) / FLOAT(d(k+1)-d(k))
      PRINT *,'What is the name of the file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='OLD')
 9001 FORMAT (A60)
 
      READ (11,9002) (d(i), i = 1, ndepths)
 9002 FORMAT (24x,9(3x,I3))
      WRITE (*,9002) (d(i), i = 1, ndepths)
      d(ndepths+1) = 2*d(ndepths)

      DO 1000 i = 1, npts
        READ (11,9003) (temper(i, j),j=1,ndepths)
CD      WRITE (*,9003) (temper(i, j),j=1,ndepths)
 1000 CONTINUE
 9003 FORMAT (24x,9(F6.2))
 
      jmax = d(ndepths)/5
      PRINT *,'jmax is : ', jmax, d(ndepths)
      DO 2000 i = 1, npts
        k = 1
        interp(i,1) = temper(i,k)
        DO 2100 j = 2, jmax+1
          dupper = d(k+1)
          IF (5*(j-1) .LE. dupper) THEN
            interp(i,j) = LINEAR(j,k)
           ELSE
            k = k+1
            dupper = d(k+1)
            interp(i,j) = LINEAR(j,k)
          ENDIF
CD        WRITE (*,9004) interp(i,j)
 2100   CONTINUE
 2000 CONTINUE
 
      PRINT *,'What is the lower temperature?'
      READ (*,9004) tlower
      PRINT *,'What is the upper temperature?'
      READ (*,9004) tupper
      k = 0
      DO 3000 i = 1, npts
        DO 3100 j = 1, jmax+1
          k = k+1
CD        WRITE (*,9004) interp(i,j)
          ncsa(k) = CHAR(MOD(INT( 256.*(interp(i,j)-tlower)
     1                           / (tupper-tlower)          ), 256)  )
 3100   CONTINUE
 3000 CONTINUE
 9004 FORMAT (F5.2)
   
      PRINT *,'What would you like to call the output?'
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      WRITE (12) ncsa
      CLOSE (12, STATUS='KEEP')
      
      END