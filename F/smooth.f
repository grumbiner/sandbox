      PROGRAM smooth
C
      INTEGER nx, ny, nk
      PARAMETER (nx = 385)
      PARAMETER (ny = 465)
      PARAMETER (nk =  31)

      CHARACTER*80 fname

      REAL atmp(nx, ny)
      REAL atot(nx, ny, nk)
      INTEGER i, j, k

C     Variables for the inversion/smoothing
      REAL epsilon(nx, ny)
      REAL y(nk), a(nk), b(nk), c(nk), r(nk)
      REAL impa(nk), w(nk)
      REAL dt
      PARAMETER (dt = 1.0)
      INTEGER bctype
      PARAMETER (bctype = 2)
      INTEGER nhi
   

      DO 1000 i = 1, 9
        WRITE (fname, 9001) i
        PRINT *,fname
 9001   FORMAT ("north.96010",I1)
        OPEN (10, FILE=fname, FORM="UNFORMATTED", STATUS="OLD")
        READ (10) atmp
        CALL enter(atmp, atot, nx, ny, nk, i)
        CLOSE (10) 
 1000 CONTINUE
      DO 1001 i = 10, 31
        WRITE (fname, 9002) i
        PRINT *,fname
 9002   FORMAT ("north.9601",I2)
        OPEN (10, FILE=fname, FORM="UNFORMATTED", STATUS="OLD")
        READ (10) atmp
        CALL enter(atmp, atot, nx, ny, nk, i)
        CLOSE (10) 
 1001 CONTINUE

C  Now begin the actual work of performing the inversion
      DO 2000 j = 1, ny
        DO 2100 i = 1, nx
          PRINT *,i,j
          DO 2101 k = 1, nk
            y(k) = atot(i,j,k)
 2101     CONTINUE
          CALL findw(y, w, nk, nhi)
CT          CALL wght1(y, a, b, c, r, dt, epsilon(i,j), nhi, w, bctype)
          CALL wght1(y, a, b, c, r, dt, 1., nhi, w, bctype)
          CALL tridig(a, b, c, r, impa, nhi)
          DO 2102 k = 1, nk
            atot(i,j,k) = impa(k)
 2102     CONTINUE
 2100   CONTINUE
 2000 CONTINUE

C     Write out results
      DO 3000 k = 1, nk
        DO 3100 j = 1, ny
          DO 3200 i = 1, nx
            atmp(i,j) = atot(i,j,k)
 3200     CONTINUE
 3100   CONTINUE
        IF (k .GT. 9) THEN
          WRITE (fname, 9003) k
        ELSE
          WRITE (fname, 9004) k
        ENDIF
 9003   FORMAT ("imp.9601",I2)
 9004   FORMAT ("imp.96010",I1)
        OPEN (10, FILE=fname, FORM="UNFORMATTED", STATUS="NEW")
        WRITE (10) atmp
        CLOSE (10)
 3000 CONTINUE



      STOP
      END
