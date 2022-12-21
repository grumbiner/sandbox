      SUBROUTINE mclean(rmask, bathy, L, M, imask, inmask, unit)
C     Convert the interpolated mask to the integral
C        masks and clean up things like isolated lakes
C        and the boundary condition conventions.
C     Use bathymetry information in the remasking procedure BG 11/08/95.
C     Robert Grumbine 
C     Last Modified 9 November 1995

      IMPLICIT none

      INTEGER L, M, unit
      REAL rmask(0:L,0:M), bathy(0:L, 0:M)
      INTEGER imask(0:L,0:M), inmask(L,M)

      INTEGER i, j

C     0 = ocean, 1 = land (NMC convention)
      DO 1000 j = 0, M
        DO 1100 i = 0, L
CD          imask(i,j) = INT(rmask(i,j) + 0.5)
          IF (rmask(i,j) .LE. 0.5 .AND. bathy(i,j) .GE. 5.0 .OR.
     1        rmask(i,j) .LE. 0.1 .AND. bathy(i,j) .GT. 1.0 .OR.
     2        rmask(i,j) .LE. 0.75 .AND. bathy(i,j) .GE. 100.0 ) THEN
            imask(i,j) = 0
          ELSE
            imask(i,j) = 1
            IF (bathy(i,j) .GT. 1.0 ) THEN
              PRINT *,i,j,bathy(i,j)
              bathy(i,j) = 1.0
            ENDIF
          ENDIF 
 1100   CONTINUE
 1000 CONTINUE
      
C     Clear isolated water points in the interior
      DO 2000 j = 1, M-1
        DO 2100 i = 1, L-1
          IF (imask(i,j) .EQ. 0 .AND. 
     1  imask(i-1,j)*imask(i+1,j)*imask(i,j-1)*imask(i,j+1) 
     1                        .EQ. 1) THEN
           imask(i,j) = 1
           PRINT *,'isolated water point at ',i,j
          ENDIF
 2100   CONTINUE
 2000 CONTINUE

C     Now work on the inner mask
C     First pass, count up how many of the nearest neighbors are
C      land.  Use rmask for temporary storage.
      DO 2999 j = 1, M
        DO 2998 i = 1, L
          inmask(i,j) = -9
 2998   CONTINUE
 2999 CONTINUE

      DO 3000 j = 1, M
        DO 3100 i = 1, L
          rmask(i,j) = 0.
          IF (imask(i-1,j) .EQ. 1) rmask(i,j) = rmask(i,j)+1
          IF (imask(i,j-1) .EQ. 1) rmask(i,j) = rmask(i,j)+1
          IF (imask(i,j)   .EQ. 1) rmask(i,j) = rmask(i,j)+1
          IF (imask(i-1,j-1) .EQ. 1) rmask(i,j) = rmask(i,j)+1
C         Decision: if 3 or 4 are land, make land.  If 3 or 4 are
C           ocean, make ocean.
          IF (rmask(i,j) .GE. 3) inmask(i,j) = 1
          IF (rmask(i,j) .LE. 1) inmask(i,j) = 0
C         Decision: round towards ocean.
          IF (rmask(i,j) .EQ. 2) inmask(i,j) = 0
 3100   CONTINUE
 3000 CONTINUE
      DO 3200 j = 1, M
        DO 3300 i = 1, L
          IF (inmask(i,j) .EQ. -9) inmask(i,j) = 1
 3300   CONTINUE
 3200 CONTINUE

C     Flip masks for MPI model.  1 = ocean, 0 = land
      DO 4000 j = 1, M
        DO 4100 i = 1, L
          IF (inmask(i,j) .EQ. 1) THEN
            inmask(i,j) = 0
           ELSE
            inmask(i,j) = 1
          ENDIF
 4100   CONTINUE
 4000 CONTINUE
      DO 4001 j = 0, M
        DO 4101 i = 0, L
          IF (imask(i,j) .EQ. 1) THEN
            imask(i,j) = 0
           ELSE
            imask(i,j) = 1
          ENDIF
 4101   CONTINUE
 4001 CONTINUE

C     Write out the scalars mask prior to setting boundary conditions
C      which are used only by my model.
      DO 4200 j = 0, M
        WRITE (12,9002) (imask(i,j),i=0,L)
 4200 CONTINUE
 9002 FORMAT (95I1)

C     Set boundary mask values
      imask(0,0) = 0
      imask(L,0) = 0
      DO 5000 j = 1, M
        imask(0,j) = 0
        imask(L,j) = 0
        inmask(1,j) = 0
        inmask(L,j) = 0
 5000 CONTINUE
     
      imask(0,0) = 0
      imask(0,M) = 0
      DO 5100 i = 1, L
        imask(i,0) = 0
        imask(i,M) = 0
        inmask(i,1) = 0
        inmask(i,M) = 0
 5100 CONTINUE
      
      RETURN
      END
