#ifndef MCONS
#   include "mcons.h"
#endif
      PROGRAM ADIABT
C ADIABT tests energy conservation properties of Prognos, using a
C adiabatic baroclinic lifecycle experiment in a manner similar to
C that of Hoskins and Simmons(1975): QJRMS, Vol. 101, 637-655.
C
C Version 2.4 created on November 19, 1990.
C
#include "mgrid.h"
      REAL A, OMEGA, R, CPRESS, GRAVIT
      REAL PTHICK(L1MAX), TZMEAN(L1MAX), TRADEQ(K2MAX,L1MAX)
      REAL TSTEEP, DEL8DF, TDAMP(L1MAX), UDAMP(L1MAX), SFRIC, VERVIS
      REAL IMPLCT, DP1, DTHETA, DT1
      REAL DTHETY, DTHETZ, ROSST, UTOP, ROBFAC, TIM
      REAL VEL(K1MAX, K2MAX, JX:JY), FPH(K1MAX, K2MAX)
      REAL VELTEN(K1MAX, K2MAX, JX:JY)
      REAL UBAR(K2MAX, L1MAX), VBAR(K2MAX, L1MAX), WBAR(K2MAX, L1MAX)
      REAL TBAR(K2MAX, L1MAX), APEINI, TKEINI, TEINI, ENORM
      REAL AM, AMTEN, TKE, TKETEN, EKE, PIE, PIETEN, TE, TETEN, APE
      REAL THET1, THET2, THET1L, THET2L, TEMP
      REAL GAVGE, WTOTAL
      COMPLEX  PGQSP0(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      COMPLEX  PGQSP1(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      COMPLEX  PGQSP2(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      COMPLEX DPGQSP(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      COMPLEX FSP1(0:M1MAX,0:N1MAX), FSP2(0:M1MAX,0:N1MAX)
      COMPLEX WSP(0:M1MAX,0:N1MAX)
      INTEGER MMAX, NMAX, NLEV, J, K, L, M, N, IPGQ, IT
      INTEGER NDT, NSTEP, NSKIP
#ifdef GRAPHICS
      INTEGER IDISP
      REAL VCROSS(K2MAX, L1MAX), FDISP
#endif
C     Initialize planetary parameters (non-dimensional)
C     (Temperature scale = 300K, Length scale = planetary radius,
C      Time scale = planetary rotation period,
C      Pressure scale = surface pressure)
C
      A= 1.0
      OMEGA= 2.0 * 3.1416
      R= 16.0
      CPRESS= 56.0
      GRAVIT= 1.E4
      CALL PLINI( OMEGA, R, CPRESS, GRAVIT )
C
      WRITE(*,*) 'MMAX=?, NMAX=?, NLEV=?, NDT=?, NSTEP=?, NSKIP=?'
      READ(*,*) MMAX, NMAX, NLEV, NDT, NSTEP, NSKIP
C     Initialize horizontal truncation
      CALL SPINI( MMAX, NMAX, A )
C     Initialize vertical truncation and damping parameters
      DTHETY= 0.15
      DTHETZ= 0.1
      DTHETA= DTHETZ / (NLEV-1)
      DP1= 1.0 / NLEV
      DO 20 L=1,NLEV
        PTHICK(L)= DP1
        TDAMP(L)= 0.0
        UDAMP(L)= 0.0
        TZMEAN(L)= 1.0 + DTHETZ/2.0 - (L-1)*DTHETA
C       Radiative equilibrium THETA (see Held and Hou, JAS 1980)
        DO 10 K=1,K2
          TRADEQ(K,L)= TZMEAN(L) + 0.5*DTHETY*( COS(2.*PHI(K)) - 1./3. )
   10   CONTINUE
   20   CONTINUE
      TSTEEP= 1.5
      DEL8DF= 0.0
      SFRIC= 0.0
      IF (NLEV.GT.2) THEN
        VERVIS= 0.0
      ELSE
        VERVIS= 0.0
      ENDIF
      CALL VERINI( NLEV, PTHICK, TZMEAN, TSTEEP, DEL8DF,
     1             TRADEQ, TDAMP, UDAMP, SFRIC, VERVIS )
#ifdef GRAPHICS
C     Initialize graphics
      CALL GSETUP
C     Read display parameters
      WRITE(*,*) 'IDISP=?, FDISP=?'
      READ(*,*) IDISP, FDISP
#endif
C     Compute thermal Rossby no.
      ROSST= CPRESS * DTHETY / (OMEGA0 * A0)**2
      UTOP= OMEGA0*A0*(SQRT(1.0+2.*ROSST*(PKLV(NLEV)-PKLV(1)))-1.0)
      WRITE(*,*) 'ZTOP= ', ZSTDLV(1), ', UTOP= ', UTOP
      DO 60 L=1,NLEV
        DO 30 IPGQ=1,NPGQ
   30     CALL ZEROSP( PGQSP0(0,0,IPGQ,L), N1 )
C       Initial wind distribution (uniform superrotation)
        TEMP= OMEGA0*A0*(SQRT(1.0+2.*ROSST*(PKLV(NLEV)-PKLV(L)))-1.0)
        DO 40 K=1,K2
   40     UBAR(K,L)= TEMP * COSPHI(K)
        CALL VBRINI( PGQSP0(0,0,JVOR,L), UBAR(1,L) )
C       Initial potential temperature distribution (rad. eq.)
        DO 50 K=1,K2
          DO 50 J=1,K1
   50       FPH(J,K)= TRADEQ(K,L)
        CALL SPECTR( PGQSP0(0,0,JPOT,L), FPH )
        IF (M1.GE.9) THEN
C         Add a wavy barotropic vorticity perturbation
          PGQSP0(8,9,JVOR,L)= (0.05, 0.0)
        ENDIF
   60   CONTINUE
C     Initialize time-stepping scheme
      IMPLCT= 0.5
      DT1= 1.0 / NDT
      ROBFAC= 0.0001
      CALL DDTINI( 0.5 * DT1, IMPLCT )
C     Compute backward time-tendency
      CALL DDTPGQ( DPGQSP, PGQSP0, PGQSP0 )
      CALL DDTFRC( DPGQSP, PGQSP0, PGQSP0 )
      CALL DDTIMP( DPGQSP )
      DO 70 L=1,NLEV
        DO 70 IPGQ=JVOR,JPOT
          DO 70 N=0,N1
            DO 70 M=0,MIN0(N,M1)
              PGQSP1(M,N,IPGQ,L)= PGQSP0(M,N,IPGQ,L)
     1                            + DT1 * DPGQSP(M,N,IPGQ,L)
  70          CONTINUE
C
      CALL DDTINI( DT1, IMPLCT )
C     Time-march
      DO 600 IT=1,NSTEP
        TIM= IT*DT1
C       Compute leap-frog "adiabatic" time tendency
        CALL DDTPGQ( DPGQSP, PGQSP0, PGQSP1 )
C       Include effects of forcing/damping
        CALL DDTFRC( DPGQSP, PGQSP0, PGQSP1 )
C       Make implicit corrections to tendency
        CALL DDTIMP( DPGQSP )
C       Leap-frog to next time-step
        DO 100 L=1,NLEV
          DO 100 IPGQ=JVOR,JPOT
            DO 100 N=0,N1
              DO 100 M=0,MIN0(N,M1)
                PGQSP2(M,N,IPGQ,L)= PGQSP0(M,N,IPGQ,L)
     1                + 2.0 * DT1 * DPGQSP(M,N,IPGQ,L)
  100           CONTINUE
        IF (MOD(IT,NSKIP).EQ.0) THEN
C         Calculate and display diagnostics
C         Compute forward "adiabatic" time tendency
          CALL DDTPGQ( DPGQSP, PGQSP1, PGQSP1 )
C         Initialize omega at the upper boundary
          CALL ZEROSP( WSP, N1 )
          AM= 0.0
          AMTEN= 0.0
          EKE= 0.0
          TKE= 0.0
          PIE= 0.0
          APE= 0.0
          TKETEN= 0.0
          PIETEN= 0.0
          THET1= 0.0
          THET2= 0.0
          DO 200 L=1,L1
C           Compute omega at level L
C           (Actually at level L+1/2)
            DO 110 N=0,N1
              DO 110 M=0,MIN0(N,M1)
  110           WSP(M,N)= WSP(M,N) - DP(L) * PGQSP1(M,N,JDIV,L)
            CALL PHYSIC( FPH, WSP )
            CALL ZAVGE( WBAR(1,L), FPH )
C           Compute horizontal velocity at level L
            CALL SPCOPY( FSP1, PGQSP1(0,0,JVOR,L), N1)
            CALL SPCOPY( FSP2, PGQSP1(0,0,JDIV,L), N1)
            CALL IDELSQ( FSP1, FSP1, N1)
            CALL IDELSQ( FSP2, FSP2, N1)
            CALL HVELOC( VEL(1,1,JX), VEL(1,1,JY), FSP1, FSP2 )
C           Compute tendency of horizontal velocity at level L
            CALL SPCOPY( FSP1, DPGQSP(0,0,JVOR,L), N1)
            CALL SPCOPY( FSP2, DPGQSP(0,0,JDIV,L), N1)
            CALL IDELSQ( FSP1, FSP1, N1)
            CALL IDELSQ( FSP2, FSP2, N1)
            CALL HVELOC( VELTEN(1,1,JX), VELTEN(1,1,JY), FSP1, FSP2 )
C           Compute tendency of total kinetic energy
            DO 120 K=1,K2
              DO 120 J=1,K1
                FPH(J,K)= VEL(J,K,JX) * VELTEN(J,K,JX) +
     1                    VEL(J,K,JY) * VELTEN(J,K,JY)
  120           CONTINUE
            TKETEN= TKETEN + (DP(L)/PSURF) * GAVGE( FPH )
C           Compute total kinetic energy at level L
            DO 130 K=1,K2
              DO 130 J=1,K1
  130           FPH(J,K)= 0.5 * (VEL(J,K,JX)**2 + VEL(J,K,JY)**2)
            TKE= TKE + (DP(L)/PSURF) * GAVGE( FPH )
C           Compute angular momentum
            CALL MULCOS( FPH, VEL(1,1,JX), 1.0, 1 )
            AM= AM + (DP(L)/PSURF) * GAVGE( FPH )
C           Compute angular momentum tendency
            CALL MULCOS( FPH, VELTEN(1,1,JX), 1.0, 1 )
            AMTEN= AMTEN + (DP(L)/PSURF) * GAVGE( FPH )
C           Compute zonal-mean u
            CALL ZAVGE( UBAR(1,L), VEL(1,1,JX) )
            CALL ZAVGE( VBAR(1,L), VEL(1,1,JY) )
C           Compute eddy kinetic energy at level L
            CALL EDDY( VEL(1,1,JX), VEL(1,1,JX) )
            CALL EDDY( VEL(1,1,JY), VEL(1,1,JY) )
            DO 140 K=1,K2
              DO 140 J=1,K1
  140           FPH(J,K)= 0.5 * (VEL(J,K,JX)**2 + VEL(J,K,JY)**2)
            EKE= EKE + (DP(L)/PSURF) * GAVGE( FPH )
C           Compute THETA
            CALL PHYSIC( FPH, PGQSP1(0,0,JPOT,L) )
C           Compute zonal-mean THETA
            CALL ZAVGE( TBAR(1,L), FPH )
C           Compute moments of THETA
            THET1L= GAVGE( FPH )
            DO 150 K=1,K2
              DO 150 J=1,K1
  150           FPH(J,K)= FPH(J,K)**2
            THET2L= GAVGE( FPH )
            THET1= THET1 + (DP(L)/PSURF) * THET1L
            THET2= THET2 + (DP(L)/PSURF) * THET2L
C           Compute change in Potential+Internal energy at level L
            PIE= PIE + (DP(L)/PSURF) * CP * PKLV(L)
     1                  * (THET1L - WTOTAL(G,TRADEQ(1,L),K2))
C           Compute Potential+Internal energy tendency at level L
            CALL PHYSIC( FPH, DPGQSP(0,0,JPOT,L) )
            PIETEN= PIETEN + (DP(L)/PSURF) * CP * PKLV(L) * GAVGE( FPH )
C           Compute "Available Potential Energy" at level L
            TEMP= REAL(PGQSP1(0,0,JPOT,L))
            IF (L.EQ.1) THEN
              APE= APE - (DP(L)/PSURF)*CP*(THET2L-THET1L**2)*
     1            PKCHLV(L)   / ( REAL(PGQSP1(0,0,JPOT,L+1)) - TEMP )
            ELSE IF (L.EQ.L1) THEN
              APE= APE - (DP(L)/PSURF)*CP*(THET2L-THET1L**2)*
     1            PKCHLV(L-1) / ( TEMP - REAL(PGQSP1(0,0,JPOT,L-1)) )
            ELSE
              APE= APE - (DP(L)/PSURF)*CP*(THET2L-THET1L**2)*0.5*(
     1            PKCHLV(L)   / ( REAL(PGQSP1(0,0,JPOT,L+1)) - TEMP ) +
     2            PKCHLV(L-1) / ( TEMP - REAL(PGQSP1(0,0,JPOT,L-1)) ) )
            ENDIF
  200       CONTINUE
C         Compute total energy and its tendency
          TE= TKE + PIE
          TETEN= TKETEN + PIETEN
C
          IF (IT.EQ.NSKIP) THEN
            WRITE(*,*) 'TIM, EKE, APE, TKE, PIE, TE'
            WRITE(*,'(1X,F4.1,1P,5E15.6)') TIM, EKE, APE, TKE, PIE, TE
            APEINI= APE
            TKEINI= TKE
            TEINI= TE
            WRITE(*,'(1X,A4,5A15)') 'TIM', 'EKE', 'TKETEN', 'TETEN',
     1                              '<deltaTE>', 'PIE'
          ELSE
          ENDIF
          IF (EKE.GT.0.0) THEN
            ENORM= EKE
          ELSE
            ENORM= 1.0
          ENDIF
          WRITE(*,'(1X,F4.1,1P,5E15.6)') TIM, EKE, TKETEN, TETEN,
     1                                  (TE-TEINI)/ENORM, PIE
#ifdef GRAPHICS
C         Display diagnostics
          GOTO (410, 420, 430, 440, 450, 460), IDISP
C         Display u-bar cross section
  410     CALL DISCRS( UBAR, 0, 0.0, 0.0, 'U bar' )
          GOTO 490
C         Display THETA cross section
  420     CALL DISCRS( TBAR, 0, 0.0, 0.0, 'T bar' )
          GOTO 490
C         Display MMC cross section
  430     DO 435 L=1,L1-1
            TEMP= (ZSTDLV(L+1)-ZSTDLV(L))/(PLV(L+1)-PLV(L))*6.4E3/90.
            DO 435 K=1,K2
              VCROSS(K,L)= TEMP * WBAR(K,L)
  435         CONTINUE
          DO 437 K=1,K2
  437       VCROSS(K,L1)= 0.0
          CALL DISVCS( VBAR, VCROSS, FDISP, 'MMC' )
          GOTO 490
C         Display U bar at specified level
  440     CALL DISPRF( UBAR(1,NINT(FDISP)), -1.0, 1.0, 'U bar' )
          GOTO 490
C         Display rel-vorticity at specified level
  450     CALL PHYSIC( FPH, PGQSP1(0,0,JVOR,NINT(FDISP)) )
          CALL DISFLD( FPH, 0, 0.0, 0.0, 'rel-vor' )
          GOTO 490
C         Display divergence at specified level
  460     CALL PHYSIC( FPH, PGQSP1(0,0,JDIV,NINT(FDISP)) )
          CALL DISFLD( FPH, 0, 0.0, 0.0, 'diverg' )
          GOTO 490
C
  490     CALL ZFRAME
#endif
        ENDIF
C       Apply Robert filter
        DO 500 L=1,NLEV
          DO 500 IPGQ=JVOR,JPOT
            CALL ROBFIL( PGQSP0(0,0,IPGQ,L), PGQSP1(0,0,IPGQ,L),
     1                   PGQSP2(0,0,IPGQ,L), ROBFAC)
            CALL SPCOPY( PGQSP0(0,0,IPGQ,L), PGQSP1(0,0,IPGQ,L), N1)
            CALL SPCOPY( PGQSP1(0,0,IPGQ,L), PGQSP2(0,0,IPGQ,L), N1)
  500       CONTINUE
  600   CONTINUE
#ifdef GRAPHICS
C     De-initialize graphics
      CALL GEND
#endif
      STOP
      END
 
      SUBROUTINE VBRINI(VORSP,UBAR)
C VBRINI determines the spectral representation of zonal mean rel-
C vorticity, given the u-bar (zonal mean x-velocity) profile.
C Input:  REAL UBAR(K2MAX) -- UBAR(K) is ubar at MU = MU(K)
C         COMPLEX VORSP(0:M1MAX,0:N1MAX) -- initial rel- vorticity
C Output: COMPLEX VORSP(0:M1MAX,0:N1MAX) -- spectral rel-vorticity
C         with the field corresponding to UBAR added on. In other words,
C         the spectral representation is "accumulated into" VORSP.
C
#include "spcons.h"
#include "spgrid.h"
      COMPLEX VORSP(0:M1MAX,0:N1MAX)
      REAL UBAR(K2MAX)
      INTEGER J,K
      REAL UPHK(K1MAX), VPHK(K1MAX)
      DO 20 K=1,K2
C     Initialize horizontal velocity at this latitude
        DO 10 J=1,K1
          UPHK(J)=UBAR(K)
          VPHK(J)=0.0
   10     CONTINUE
C       Add contribution to rel. vorticity from this latitude
        CALL CURLK(VORSP,UPHK,VPHK,K)
   20   CONTINUE
      RETURN
      END
 
#ifdef GRAPHICS
      SUBROUTINE DISPRF( FPRF, FMIN, FMAX, STR )
C DISPRF displays the zonal-mean profile FPRF of a quantity described
C by string STR. FMIN and FMAX are the minimum and maximum values for
C display of FPRF.
C
#include "mgrid.h"
      REAL FPRF(K2), FMIN, FMAX
      CHARACTER*(*) STR
      CHARACTER*80 TEMSTR
      TEMSTR= 'Zonal-mean profile of '//STR
      CALL GLABEL( 'lat', 3, STR, LEN(STR), TEMSTR, 22+LEN(STR) )
      CALL GXYRNG(-90.0, 90.0, 6, -1, FMIN, FMAX, 0, -1)
      CALL GDRAW
      CALL GCURV( K2, DEGLAT, FPRF, 1)
      RETURN
      END
 
      SUBROUTINE DISFLD( FPH, NCDIV, CMIN, CMAX, STR )
C DISFLD displays a latitude-longitude scalar field FPH described by
C string STR. NCDIV is the number of contour divcisions, CMIN and CMAX
C are the minimum/maximum contour values.
C (specifying NCDIV=0 implies that CMIN is the contour interval; CMAX is
C  ignored in this case)
#include "mgrid.h"
      INTEGER NCDIV
      REAL FPH(K1MAX, K2MAX), CMIN, CMAX
      CHARACTER*(*) STR
C
      IF (NCDIV.EQ.0) THEN
        CALL CONTUR( K1, K2, DEGLON, DEGLAT, K1MAX, FPH, 0, 0,
     1               CMIN, 0, 0.0, 'lon', 'lat', ' ', STR )
      ELSE
        CALL CONTU2( K1, K2, DEGLON, DEGLAT, K1MAX, FPH, 0, 0, NCDIV,
     1               CMIN, CMAX, 0, 0.0, 'lon', 'lat', ' ', STR )
      ENDIF
      RETURN
      END
 
      SUBROUTINE DISVPR( VPR, FMIN, FMAX, STR )
C DISVPR displays a vertical (pressure-dependent) profile VPR described
C by string STR. FMIN and FMAX are the minimum and maximum values for
C display of FPRF.
#include "mgrid.h"
      REAL VPR(L1), FMIN, FMAX
      CHARACTER*(*) STR
      REAL VCOORD(L1MAX), VTEM(L1MAX), TEMP
      INTEGER L
      CHARACTER*80 TEMSTR
C
      DO 10 L=1,L1
        VCOORD(L1+1-L)= ZSTDLV(L)*6.4E3
        VTEM(L1+1-L)= VPR(L)
   10   CONTINUE
C
      TEMSTR= 'Vertical profile of '//STR
      CALL GLABEL( STR, LEN(STR), 'z(km)', 5, TEMSTR, 22+LEN(STR) )
C     (Round off range to tens of kilometres)
      TEMP= INT(0.9999 + ZSTDLV(1)/10000.)*10.0
      CALL GXYRNG( FMIN, FMAX, 0, -1, 0.0, TEMP, 0, -1 )
      CALL GDRAW
      CALL GCURV( L1, VTEM, VCOORD, 1)
      RETURN
      END
 
      SUBROUTINE DISCRS( VPH, NCDIV, CMIN, CMAX, STR )
C DISCRS displays a latitude-pressure scalar field VPH described by
C string STR. NCDIV is the number of contour divcisions, CMIN and CMAX
C are the minimum/maximum contour values.
C (specifying NCDIV=0 implies that CMIN is the contour interval; CMAX is
C  ignored in this case)
#include "mgrid.h"
      INTEGER NCDIV
      REAL VPH(K2MAX, L1MAX), CMIN, CMAX
      CHARACTER*(*) STR
      REAL VCOORD(L1MAX), VCROSS(K2MAX, L1MAX)
      INTEGER K, L
C
      DO 10 L=1,L1
   10   VCOORD(L1+1-L)= ZSTDLV(L)*6.4E3
      DO 20 L=1,L1
        DO 20 K=1,K2
          VCROSS(K,L1+1-L)= VPH(K,L)
   20     CONTINUE
C
      IF (NCDIV.EQ.0) THEN
        CALL CONTUR( K2, L1, DEGLAT, VCOORD, K2MAX, VCROSS, 0, 0,
     1               CMIN, 0, 0.0, 'lat', 'z(km)', ' ', STR )
      ELSE
        CALL CONTU2( K2, L1, DEGLAT, VCOORD, K2MAX, VCROSS, 0, 0, NCDIV,
     1               CMIN, CMAX, 0, 0.0, 'lat', 'z(km)', ' ', STR )
      ENDIF
      RETURN
      END
 
      SUBROUTINE DISVCS( VPHX, VPHY, VSCALE, STR )
C DISVCS displays a latitude-pressure vector field (VPHX, VPHY),
C described by string STR. VSCALE is the vector scaling factor.
#include "mgrid.h"
      REAL VPHX(K2MAX, L1MAX), VPHY(K2MAX, L1MAX), VSCALE
      CHARACTER*(*) STR
      REAL VCOORD(L1MAX), VCROS1(K2MAX, L1MAX), VCROS2(K2MAX, L1MAX)
      REAL TEMP
      INTEGER K, L
C
      DO 10 L=1,L1
   10   VCOORD(L1+1-L)= ZSTDLV(L)*6.4E3
      DO 20 L=1,L1
        DO 20 K=1,K2
          VCROS1(K,L1+1-L)= VPHX(K,L)
          VCROS2(K,L1+1-L)= VPHY(K,L)
   20     CONTINUE
C     (Round off range to tens of kilometres)
      TEMP= INT(0.9999 + ZSTDLV(1)*6.4E2)*10.0
      CALL GXYRN2( -90.0, 90.0, 6, -1, 0.0, TEMP, 0, -1,
     1             'lat', 'z(km)', STR )
      CALL GDRAW
C
      CALL GVEDAT( TEMP, K2, L1, DEGLAT, PLV,
     1             K2MAX, VCROS1, VCROS2)
      TEMP= VSCALE*TEMP
      CALL GVEBOX( K2, L1, DEGLAT, VCOORD, K2MAX, VCROS1, VCROS2,
     1             1000.0, STR )
      CALL GVEPLT( K2, L1, DEGLAT, VCOORD, K2MAX,
     1             VCROS1, VCROS2, TEMP, 1000.0, 1 )
      RETURN
      END
#endif
 
 
 
 
