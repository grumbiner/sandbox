      SUBROUTINE SPHERD
     1(IDIR,GRIDDX,GRIDDY,GRIDMN,WAVE,WORK,COSL,IMAX,JMAX,MAXWV,IROMB)
C
C  SPHERICAL TRANSFORM BETWEEN
C  A SCALAR FIELD IN SPECTRAL SPACE AND
C  ITS HORIZONTAL GRADIENT VECTOR AND GLOBAL MEAN IN GRID SPACE
C
C  IDIR.GT.0  GRID TO WAVE:  FIELD WAVE IS OUTPUT
C       IDIR= 1 ...... NORMAL TRANSFORM (GAUSSIAN GRID)
C           101 ...... AS IDIR= 1 BUT FOR EQUIDISTANT LAT/LON GRID
C  IDIR.LT.0  WAVE TO GRID:  FIELDS GRIDDX,GRIDDY,GRIDMN ARE OUTPUT
C       IDIR= -1 ...... NORMAL TRANSFORM (GAUSSIAN GRID)
C           -101 ...... AS IDIR=-1 BUT FOR EQUIDISTANT LAT/LON GRID
C
C  GRIDDX ARE THE GRIDDED IMAX*JMAX X-DERIVATIVES OF THE FIELD
C  GRIDDX = 1./(RERTH*COSL) * D(WAVE)/DLAMBDA
C
C  GRIDDY ARE THE GRIDDED IMAX*JMAX Y-DERIVATIVES OF THE FIELD
C  GRIDDY = 1./A * D(WAVE)/DPHI
C
C  GRIDMN IS THE GLOBAL MEAN OF THE FIELD
C
C  WAVE ARE THE 2*NMMAX SPECTRAL COEFFICIENTS OF THE FIELD
C
C  WORK IS WORKSPACE OF AT LEAST 2*NMMAX WORDS
C
C  COSL ARE THE JMAX COSINES OF LATITUDE
C
C  IROMB = 1 FOR RHOMBOIDAL TRUNCATION:  NMMAX=(MAXWV+1)*(MAXWV+1)
C        = 0 FOR TRIANGULAR TRUNCATION:  NMMAX=(MAXWV+1)*(MAXWV+2)/2
C
C  REQUIRES TWO-WAY VERSION OF SUBROUTINE SPHERT TO PERFORM TRANSFORMS
C
C     LAST MODIFIED 7 April 1994
      DIMENSION GRIDDX(IMAX,JMAX), GRIDDY(IMAX,JMAX)
      DIMENSION WAVE(2,(MAXWV+1)*(MAXWV+2)/2)
      DIMENSION WORK((MAXWV+1)*(MAXWV+2))
      DIMENSION COSL(JMAX)
      DATA SQRT2/1.414214/, RERTH/6.3712E6/
C***********************************************************************
C  GRID TO WAVE SECTION
      IF (IDIR.GT.0) THEN
 
C  SCALE GRID FIELDS
        DO 10 J=1,JMAX
        DO 10 I=1,IMAX
          IF (COSL(J).GT.0.) THEN
            GRIDDX(I,J) = GRIDDX(I,J)*(RERTH*COSL(J))
            GRIDDY(I,J) = GRIDDY(I,J)*(RERTH*COSL(J))
          ENDIF
   10   CONTINUE
 
C  SAVE ZONAL WAVENUMBER
        NM = 0
        DO 20 M=0,MAXWV
        DO 20 N=M,MAXWV+IROMB*M
          NM = NM + 1
          WORK(NM) = M
   20   CONTINUE
 
C  TRANSFORM TO WAVESPACE, RESPECTIVELY TAKING X AND Y DERIVATIVES
        CALL SPHERT(IDIR+2,GRIDDX,WAVE,-1,WORK,IMAX,JMAX,MAXWV,IROMB)
        CALL SPHERT(IDIR+3,GRIDDY,WORK,0,0,IMAX,JMAX,MAXWV,IROMB)
 
C  SAVE MEAN IN 0,0 COMPONENT
C  OTHERWISE ADD SECOND DERIVATIVES AND TAKE INVERSE LAPLACIAN
        NM = 0
        DO 30 M=0,MAXWV
        DO 30 N=M,MAXWV+IROMB*M
          RNN1 = -N*(N+1)
          NM = NM + 1
          IF (N.EQ.0)  THEN
            WAVE(1,NM) = GRIDMN*SQRT2
            WAVE(2,NM) = 0.
          ELSE
            WAVE(1,NM) = (WAVE(1,NM) + WORK(2*NM-1))/RNN1
            WAVE(2,NM) = (WAVE(2,NM) + WORK(2*NM))/RNN1
          ENDIF
   30   CONTINUE
 
C  UNSCALE GRID FIELDS
        DO 40 J=1,JMAX
        DO 40 I=1,IMAX
          IF (COSL(J).GT.0.) THEN
            GRIDDX(I,J) = GRIDDX(I,J)/(RERTH*COSL(J))
            GRIDDY(I,J) = GRIDDY(I,J)/(RERTH*COSL(J))
          ENDIF
   40   CONTINUE
C***********************************************************************
C  WAVE TO GRID SECTION
      ELSE IF (IDIR.LT.0) THEN
 
C  SAVE ZONAL WAVENUMBER
        NM = 0
        DO 60 M=0,MAXWV
        DO 60 N=M,MAXWV+IROMB*M
          NM = NM + 1
          WORK(NM) = M
   60   CONTINUE
 
C  TRANSFORM TO GRIDSPACE, RESPECTIVELY TAKING X AND Y DERIVATIVES
        CALL SPHERT(IDIR,GRIDDX,WAVE,-1,WORK,IMAX,JMAX,MAXWV,IROMB)
        CALL SPHERT(IDIR-1,GRIDDY,WAVE,0,0,IMAX,JMAX,MAXWV,IROMB)
 
C  SCALE GRID FIELDS NOTING THAT Y-DERIVATIVES CHANGE SIGN
        DO 70 J=1,JMAX
        DO 70 I=1,IMAX
          IF (COSL(J).GT.0.) THEN
            GRIDDX(I,J) = GRIDDX(I,J)/(RERTH*COSL(J))
            GRIDDY(I,J) = -GRIDDY(I,J)/(RERTH*COSL(J))
          ENDIF
   70   CONTINUE
 
C  SAVE MEAN FROM 0,0 COMPONENT
        GRIDMN = WAVE(1,1)/SQRT2
 
      ENDIF
C***********************************************************************
      RETURN
      END
