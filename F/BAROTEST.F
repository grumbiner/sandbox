      PROGRAM bartst
C     verification of the barotropic solution.
C     Check that: 
C       1) It statisfies div(U) = 0.0
C       2) It satisfies U(0) = U(Lx) = 0.0
C       3) It satisfies V(0) = V(Lx) = 0.0
C       4) It satisfies ? V(y=0) = V(y=Ly) = 0.0?

      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)
      REAL ut(nx, ny), vt(nx, ny), we(nx, ny)

      CHARACTER*60 fname
      INTEGER i, j
      REAL div(nx, ny)
      REAL dx, dy, eps

CD    PRINT *,'What is the name of the wind forcing file?'
CD    READ (*, 9001) fname
CD    OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      PRINT *,'What is the name of the Ut file?'
      READ (*, 9001) fname
      OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      PRINT *,'What is the name of the Vt file?'
      READ (*, 9001) fname
      OPEN (12, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
 9001 FORMAT (A60)
   
CD    READ (10) we
      READ (11) ut
      READ (12) vt

      PRINT *,'Checking no normal flow conditions'
      PRINT *,'First, U, V = 0 at Lx'
      DO 100 j = 1, ny
        i = nx
        IF (ABS(ut(i,j)) .NE. 0.0 .OR. ABS(vt(i,j)) .NE. 0.0) THEN
          PRINT *,'ut, vt fails at',i, j, ut(i,j), vt(i,j)
        ENDIF
  100 CONTINUE

      PRINT *,'Now try U, V = 0 at 0'
      DO 200 j = 1, ny
        i = 1
        IF (ABS(ut(i,j)) .NE. 0.0 .OR. ABS(vt(i,j)) .NE. 0.0) THEN
          PRINT *,'ut, vt fails at',i, j, ut(i,j), vt(i,j)
        ENDIF
  200 CONTINUE

      PRINT *,'Now look at V = 0 at 0'
      j = 1
      DO 300 i = 1, nx
        IF (ABS(vt(i,j)) .NE. 0.0) THEN
          PRINT *,'vt fails at ', i, j, vt(i,j)
        ENDIF
  300 CONTINUE
      PRINT *,'Now look at V = 0 at Ly'
      j = ny
      DO 400 i = 1, nx
        IF (ABS(vt(i,j)) .NE. 0.0) THEN
          PRINT *,'vt fails at ', i, j, vt(i,j)
        ENDIF
  400 CONTINUE

      PRINT *,' '
      PRINT *,'Now consider the divergence field'
      PRINT *,'Compute the divergence at the non-boundary pts.'
      PRINT *,'What is dx?'
      READ (*,9002) dx
      PRINT *,'What is dy?'
      READ (*,9002) dy
      DO 1000 j = 2, ny-1
        DO 1100 i = 2, nx-1
          div(i,j) = (ut(i+1,j)-ut(i-1,j))/2./dx +
     1               (vt(i,j+1)-vt(i,j-1))/2./dy
 1100   CONTINUE
 1000 CONTINUE
      DO 1110 i = 1, nx
        j = 1
        div(i,j) = 0.0
        j = ny
        div(i,j) = 0.0
 1110 CONTINUE
      DO 1120 j = 1, ny
        i = 1
        div(i,j) = 0.0
        i = nx
        div(i,j) = 0.0
 1120 CONTINUE

      PRINT *,'Now compare div and 0.0'
      PRINT *,'What is the standard of smallness?'
      READ (*,9002) eps
      DO 2000 j = 2, ny-1
        DO 2010 i = 2, nx-1
          IF (ABS(div(i,j)) .GT. eps) THEN
            PRINT *,'divergence fails at', i, j, div(i,j)
          ENDIF
 2010   CONTINUE
 2000 CONTINUE

 9002 FORMAT (E13.6)
         
      END
