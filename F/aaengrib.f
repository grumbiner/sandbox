      PROGRAM aagrib
C     Engrib maps already on lat-long grids.
C     Variant for Antarctica to put out latitude belts for high 
C        resolution grids. 
      IMPLICIT none

      INTEGER nx, ny
      REAL dxlat, dylat
      PARAMETER (dxlat = 1./6.)
      PARAMETER (dylat = 1./6.)
      PARAMETER (nx = 360. / dxlat + 0.5)
      PARAMETER (ny =  40. / dylat + 0.5)
C     Maximum points in a grib message
      INTEGER gribmx
      PARAMETER (gribmx = 260000)
C     Bits for encoding
      INTEGER mxbit
      PARAMETER (mxbit  = 32)

      REAL outmap(nx, ny)
      LOGICAL lbm(nx, ny)

      CHARACTER grib((100 + 28 + nx*ny*(mxbit+1))/8 )
      INTEGER lgrib, ierr, nbelt
      REAL north, south
      
      INTEGER i, j, yy, mm, dd
      CHARACTER*6 tag

      PRINT *,'nx, ny = ',nx, ny
      READ (10) outmap

      READ (*, 9001) tag
 9001 FORMAT (A6)
      READ (tag, 9002) yy, mm, dd
 9002 FORMAT (I2, I2, I2)

      
      IF (nx*ny .LT. gribmx) THEN
        CALL gribit(outmap, lbm, 0, nx, ny, mxbit, 0.0, 
     1        28, 1, 7, 0, 0, 008, 
     1       102, 0, 0, yy, mm, dd, 0, 1,
     2       0, 0, 10, 0, 0, 2, 
C     Last argument is power in multiplying (data)*10**x prior to gribbing.
     3       90.-dylat/2.,         dxlat/2., 
     4       50.-dylat/2+(ny-1)*dylat, dxlat/2.+dxlat*(nx-1), 
     5       dxlat, dylat, 0, -10., grib, lgrib, ierr) 
C    Note above that there is a non-global coverage
        PRINT *,'ierr = ',ierr
        PRINT *,'lgrib = ',lgrib
        IF (ierr .EQ. 0) THEN
          CALL WRYTE(12, lgrib, grib)
        ENDIF
      ELSE
        PRINT *,'In belting loop' 
        nbelt = 4
        IF ( MOD(ny, nbelt) .NE. 0) THEN
          PRINT *,'can"t print out even number of lats per belt'
          STOP
        ENDIF
        DO 1000 i = 1, nbelt
          south = 90. - dylat/2. + (i-1)*(ny/nbelt)*dylat
          north = south + (ny/nbelt)*dylat
          CALL gribit( outmap(1, (i-1)*(ny/nbelt) + 1), 
     1         lbm, 0, nx, ny/nbelt, 
     1         mxbit, 0.0, 28, 1, 7, 0, 0, 008, 
     1         102, 0, 0, yy, mm, dd, 0, 1,
     2         0, 0, 10, 0, 0, 0, 
C     Last argument is power in multiplying (data)*10**x prior to gribbing.
     3         south, dxlat/2., 
     4         north, dxlat/2.+dxlat*(nx-1), 
     5         dxlat, dylat, 0, -10., grib, lgrib, ierr) 
          PRINT *,'ierr = ',ierr
          PRINT *,'lgrib = ',lgrib
          IF (ierr .EQ. 0) THEN
            CALL WRYTE(12, lgrib, grib)
          ENDIF
 1000   CONTINUE

      ENDIF

        
     
      STOP
      END
