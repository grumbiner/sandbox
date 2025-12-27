      SUBROUTINE COF2GRD (LUN1,NC,JROMB,
     1      JCAP,XGRID,PGRID)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    COF2GRD     CONVERT ONE RECORD OF SIGMA COEFF FILE
C                            TO LAT/LON GRID
C   PRGMMR: ROGERS           ORG: W/NP22     DATE: 99-01-28
C
C ABSTRACT: CONVERT SIGMA COEFFICIENT RECORD TO GRID SPACE USING
C           SPLIB ROUTINES. THESE ROUTINES WILL RETURN A GLOBAL
C           LAT/LON GRID WHOSE RESOLUTION IS DETERMINED BY THE 
C           NUMBER OF GRID POINTS. THEN, THE RELEVENT SUBSET FOR
C           WHICH WE HAVE HIGH-RES OROGRAPHY IS EXTRACTED (DIMENSION
C           OF BOTH THE EXTRACTED GRID AND GLOBAL GRID SET IN 
C           parmanl FILE)
C
C PROGRAM HISTORY LOG:
C   99-01-28  ROGERS
C
C USAGE:    CALL COF2GRD(LUN1,NC,KMAX,JROMB, 
C               JCAP,XGRID,PGRID)
C
C   INPUT ARGUMENT LIST:
C     LUN1     - FORTRAN UNIT FOR SIGMA FILE
C     NC       - LENGTH OF SIGMA RECORD = RES+1*RES+2
C     KMAX     - NUMBER OF SIGMA LEVELS IN GLOBAL MODEL
C     JROMB    - SPECTRAL DOMAIN SHAPE (0 FOR TRIANGULAR, 
C                1 FOR RHOMBOIDAL)
C     JCAP     - SPECTRAL TRUNCATION
C
C   OUTPUT FILES:
C     XGRID    - ARRAY holding the IMAX x JMAX grids at KMAX levels
C                FOR FOUR VARIABLES:  1-Tv  2-U  3-V  4-q
C     PGRID    - ARRAY holding the IMAX x JMAX grids of Z* and p*
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE: CRAY C-90
C
C$$$
      INCLUDE "parmlbc"
      COMMON /GRID/ POLA,NORTH,ALONVT,POLEI,POLEJ,XMESHL
C
C     XMESHL = EAST-WEST GRID INCREMENT
C     POLEI  = SOUTH-NORTH GRID INCREMENT
C     POLEJ  = WESTERN BOUNDARY OF LAT/LON GRID
C     ALONVT = SOUTHERN BOUNDARY OF LAT/LON GRID
C
      REAL XGRID(IMAX,JMAX,KMAX,4),PGRID(IMAX,JMAX,2)
      REAL DWORK(NC),ZWORK(NC),HWORK(NC),GRD(IMAX,JMAX)
C
      print *,'entering cof2grd',jromb,jcap,imax,jmax
C
C   READ TERRAIN COEFFICIENTS
C
      print *,'cof2grd ',kmax
      READ(LUN1) HWORK
      CALL SPTRAN(JROMB,JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX,
     1        0,0,0,0,1,HWORK,PGRID(1,JMAX,2),PGRID(1,1,2),1)
      print *,'ok after terrain coeffs'
C
C   READ SFC PRESSURE COEFFICIENTS
C
      print *,'cof2grd ',kmax
      READ(LUN1) HWORK
      CALL SPTRAN(JROMB,JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX,
     1        0,0,0,0,1,HWORK,GRD(1,JMAX),GRD(1,1),1)
      print *,'ok after ps coeffs'
      DO J = 1, JMAX
        DO I = 1, IMAX
         PGRID(I,J,1) = 10.*EXP(GRD(I,J)) 
        ENDDO
      ENDDO
C
C   READ TEMPERATURE COEFFICIENTS
C
      DO L = 1, KMAX
        READ(LUN1) HWORK
        CALL SPTRAN(JROMB,JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX,
     1        0,0,0,0,1,HWORK,XGRID(1,JMAX,L,1),XGRID(1,1,L,1),1)
      ENDDO
      print *,'ok after t coeffs'
C
C   READ DIVERGENCE AND VORTICITY COEFFICIENTS
C
      DO L = 1, KMAX
        READ(LUN1) DWORK
        READ(LUN1) ZWORK
        CALL SPTRANV(JROMB,JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX,
     1        0,0,0,0,1,DWORK,ZWORK,XGRID(1,JMAX,L,2),XGRID(1,1,L,2),
     2        XGRID(1,JMAX,L,3),XGRID(1,1,L,3),1)
      ENDDO
      print *,'ok after div/vort coeffs'
C
C   READ SPECIFIC HUMIDITY COEFFICIENTS
C
      DO L = 1, KMAX
        READ(LUN1) HWORK
        CALL SPTRAN(JROMB,JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX,
     1        0,0,0,0,1,HWORK,XGRID(1,JMAX,L,4),XGRID(1,1,L,4),1)
      ENDDO
      print *,'ok after q coeffs'
C
C
c     DO j = 1, jmax
c      do i = 1, imax
c        if(mod(i,30).eq.0.and.mod(j,30).eq.0) then
c          write(6,1222)i,j,pgrid(i,j,1),pgrid(i,j,2)
c222       format(1x,2i4,2(1x,e12.5))
c         do k = 1, kmax
c          write(6,1223)k,xgrid(i,j,k,1),xgrid(i,j,k,2),xgrid(i,j,k,3),
c    1       xgrid(i,j,k,4)
c223       format(1x,i3,4(1x,e12.5))
c         enddo
c        endif
c      enddo
c     enddo
       
      RETURN
      END
