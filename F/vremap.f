      PROGRAM vremap
C     Read in the values from the vary.out file and re-map onto
C       sea ice grids
      INTEGER nx, ny, nstep
      PARAMETER (nx = 385)
      PARAMETER (ny = 465)
      PARAMETER (nstep = 55)
      CHARACTER*1 humunga(nx, ny, nstep)
CD      REAL rmunga(nx, ny, nstep)

      INTEGER i, j, k
      REAL x, y
      CHARACTER*60 fname

      READ (*,9002) fname
 9002 FORMAT (A60)
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')

 9001 FORMAT (8x, F8.4, 8x, F10.6)
      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
          DO 1020 k = 1, nstep
CD            READ (10, 9001) x, y
            READ (10, 9001) x
            IF (x .LT. 0.) x = 0.
            IF (x .GT. 2.55) x = 2.55
            humunga(i, j, k) = CHAR( INT ( x*100. + 0.5) )
CD            rmunga(i, j, k) = y
 1020     CONTINUE
 1010   CONTINUE
 1000 CONTINUE


      DO 2000 k = 1, nstep
        CALL cout(humunga(1, 1, k), k)
CD        CALL rout(rmunga(1, 1, k), k)
 2000 CONTINUE


      STOP
      END
