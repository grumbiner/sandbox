      PROGRAM onea
C     convert from arctic quarter degree lat/long grid to a 1 degree lat/long.
C     use w3lib routine to manage translation.

      INTEGER nx, ny
      PARAMETER (nx =  1440)
      PARAMETER (ny =   181)
      INTEGER ratio
      PARAMETER (ratio = 4)

      REAL in(nx, ny), out(nx/ratio, ny/ratio+1)
      CHARACTER*1 cin(nx, ny), cout(nx/ratio, ny/ratio+1)
      REAL scale, ai, aj, bi, bj
      PARAMETER (scale = ratio )
      PARAMETER (ai    = 1.)
      PARAMETER (aj    = 1.)
      PARAMETER (bi    = 1.)
      PARAMETER (bj    = 1.)
      INTEGER i, j
      LOGICAL nothing

      nothing = .TRUE.
      READ (10) cin
      DO 1000 j = 1, ny
        DO 1000 i = 1, nx
          in(i,j) = FLOAT(ICHAR(cin(i,j))) 
          IF (in(i,j) .NE. 0.) nothing = .FALSE.
 1000 CONTINUE
      IF (nothing) PRINT *,'No nonzero data points'

      CALL w3ft00(in, out, nx, ny, nx/ratio, ny/ratio+1, 
     1  ai, aj, bi, bj, scale, 0.0, 1)

      DO 2000 j = 1, ny/ratio+1
         DO 2000 i = 1, nx/ratio
           cout(i,j) = CHAR(INT(out(i,j)+0.5))
 2000 CONTINUE
      
      WRITE (11) cout

      STOP
      END
