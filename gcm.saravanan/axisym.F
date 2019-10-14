#ifndef MCONS
#   include "mcons.h"
#endif
      PROGRAM AXISYM
C AXISYM carries out axisymmetric calculations analogous to
C those of Held and Hou(1980), J. Atmos. Sci, 37, p.515-533
C
C Version 1.8 created November 19, 1990.
C
#include "mgrid.h"
      REAL A, OMEGA, R, CPRESS, GRAVIT
      REAL PTHICK(L1MAX), TZMEAN(L1MAX), TRADEQ(K2MAX,L1MAX)
      REAL TSTEEP, DEL8DF, TDAMP(L1MAX), UDAMP(L1MAX), SFRIC, VERVIS
      REAL PHALFL(0:L1MAX), PP(L1MAX+1), TBOT, DTHETZ
      REAL DAYINV, PBOT, PLID, PFAC, DTHETY, IMPLCT, DT1, ROBFAC, TIM
      REAL VEL(K1MAX, K2MAX, JX:JY), FPH(K1MAX, K2MAX)
      REAL UBAR(K2MAX, L1MAX), VBAR(K2MAX, L1MAX), WBAR(K2MAX, L1MAX)
      REAL TBAR(K2MAX, L1MAX), EKE, PIETEN, APE, UTROP
      REAL THET1L, THET2L, TEMP
      REAL GAVGE
      COMPLEX  PGQSP0(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      COMPLEX  PGQSP1(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      COMPLEX DPGQSP(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      COMPLEX FSP1(0:M1MAX,0:N1MAX), FSP2(0:M1MAX,0:N1MAX)
      COMPLEX WSP(0:M1MAX,0:N1MAX)
      INTEGER MMAX, NMAX, NLEV, I, J, K, L, M, N, IPGQ, IT
      INTEGER IEXPT, NSTEP, NSKIP, KMID
      CHARACTER*72 FMT, PRGSTR, FILNAM, RUNTTL, TEMSTR
#ifdef GRAPHICS
      INTEGER IDISP
      REAL VCROSS(K2MAX, L1MAX), FDISP
#endif
C     Format for saving model state
      FMT= '(1X,1P,5E15.6)'
C     Program description
      PRGSTR= 'AXISYM v1.7 created 2 Nov 90'
C     Initialize planetary parameters
      DAYINV= 1.0 / 86400.
      OMEGA= 2.0 * 3.1416 * DAYINV
      R= 287.0
      CPRESS= 1004.0
      GRAVIT= 9.81
      A= 6.4E6
      CALL PLINI( OMEGA, R, CPRESS, GRAVIT )
C
      WRITE(*,*) PRGSTR
      WRITE(*,*) 'Continue file name? (blank for new run)'
      READ(*,'(A)') FILNAM
      IF (FILNAM.NE.' ') THEN
        WRITE(*,*) 'Reading model state from ', FILNAM
        OPEN( 11, FILE=FILNAM )
        READ(11,'(1X,A72)') RUNTTL
        READ(11,'(1X,A72)') TEMSTR
        READ(11,'(1X,1P,4I4,2E15.6)')MMAX,NMAX,NLEV,IEXPT,PLID,TIM
        WRITE(*,*) RUNTTL
        WRITE(*,*) TEMSTR
        WRITE(*,*) 'MMAX=?, NMAX=?, NLEV=?, PLID=?, IEXPT=?'
        WRITE(*,*) MMAX, NMAX, NLEV, PLID, IEXPT
      ELSE
        WRITE(*,*) 'Title of run?'
        READ(*,'(A72)') RUNTTL
        WRITE(*,*) 'MMAX=?, NMAX=?, NLEV=?, PLID=?, IEXPT=?'
        READ(*,*) MMAX, NMAX, NLEV, PLID, IEXPT
      ENDIF
C     Initialize horizontal truncation
      CALL SPINI( MMAX, NMAX, A )
      KMID= (K2+1)/2
C     Initialize pressure-level values
C     Surface pressure (1000 mb)
      PBOT= 1000.0E2
C     Compute pressure level thicknesses
      IF ((PBOT/PLID).GT.FLOAT(NLEV)) THEN
C       Level thickness to increase with increasing pressure
C       Compute pressure-level thickness amplification factor PFAC
        PFAC= (PBOT/PLID)**(1.0/(NLEV-1))
        DO 10 I=1,10
   10     PFAC= (1.0 + (PFAC - 1.0)*(PBOT/PLID))**(1.0/NLEV)
C       Determine level thicknesses
        PTHICK(NLEV)= PLID * PFAC**(NLEV-1)
        TEMP= PTHICK(NLEV)
        DO 20 L=NLEV-1,2,-1
          PTHICK(L)= PTHICK(L+1) / PFAC
          TEMP= TEMP + PTHICK(L)
   20     CONTINUE
C       Ensure that the thicknesses add up to the surface pressure
        PTHICK(1)= PBOT - TEMP
      ELSE IF ((PBOT/PLID).GT.0.99*FLOAT(NLEV)) THEN
C       Equally spaced pressure levels
        PFAC= 1.0
        TEMP= PBOT / NLEV
        DO 30 L=1,NLEV
   30     PTHICK(L)= TEMP
      ELSE
        CALL SPERR( 'PTEST', 'Uppermost pressure level too thick')
      ENDIF
C
      PP(NLEV+1)= PBOT
      PHALFL(NLEV)= PBOT
      DO 40 L=NLEV-1,1,-1
        PHALFL(L)= PHALFL(L+1) - PTHICK(L+1)
   40   PP(L+1)= PHALFL(L+1) - 0.5*PTHICK(L+1)
      PHALFL(0)= 0.0
      PP(1)= PHALFL(1) - 0.5*PTHICK(1)
C     Equatorial surface temperature
      TBOT= 288.15
C     Temperature change per scale height
      DTHETZ= 43.0
C     Equator to pole temperature difference
      DTHETY= 60.0
C     Radiative equilibrium THETA (see Held and Hou, JAS 1980)
      DO 70 L=1,NLEV
        TZMEAN(L)= (TBOT-DTHETZ*AMIN1(12.0/6.6,ALOG(PP(NLEV+1)/PP(L))) )
     1             * (PP(NLEV+1)/PP(L))**(R/CPRESS)
        IF (IEXPT.EQ.1) THEN
C         Standard experiment
          TEMP= DTHETY
        ELSE IF (IEXPT.EQ.2) THEN
C         Zero stratospheric meridional gradient experiment
          TEMP= DTHETY*AMAX1(0.0,
     1                       (1.0-(6.6/12.0)*ALOG(PP(NLEV+1)/PP(L))) )
        ELSE IF (IEXPT.EQ.3) THEN
C         Reversed stratospheric meridional gradient experiment
          TEMP= DTHETY*AMAX1(-1.0,
     1                       (1.0-(6.6/12.0)*ALOG(PP(NLEV+1)/PP(L))) )
        ENDIF
        DO 60 K=1,K2
          TRADEQ(K,L)= TZMEAN(L) - TEMP*SIN(PHI(K))**2
   60   CONTINUE
   70   CONTINUE
C     Initialize damping parameters
      TSTEEP= 1.5
      DEL8DF= DAYINV / FLOAT(NMAX*(NMAX+1))**4
      SFRIC= DAYINV / 1.
      VERVIS= 5.0
      DO 80 L=1,NLEV
C       10 day radiative relaxation
        TDAMP(L)= DAYINV / 10.
        UDAMP(L)= 0.0
   80   CONTINUE
      CALL VERINI( NLEV, PTHICK, TZMEAN, TSTEEP, DEL8DF,
     1             TRADEQ, TDAMP, UDAMP, SFRIC, VERVIS )
#ifdef GRAPHICS
C     Initialize graphics
      CALL GSETUP
C     Display mean THETA cross section
      CALL DISCRS( TRADEQ, 0, 0.0, 0.0, 'THETA' )
      CALL ZFRAME
#endif
C     Initialize time-stepping scheme
      IMPLCT= 0.5
      DT1= 86400. / 24.
      ROBFAC= 0.01
C     Determine initial state
      IF (FILNAM.NE.' ') THEN
C       Read in initial state
        DO 110 L=1,NLEV
          DO 110 IPGQ=JVOR,JPOT
            READ(11,FMT)((PGQSP0(M,N,IPGQ,L),M=0,MIN0(N,MMAX)),N=0,NMAX)
            READ(11,FMT)((PGQSP1(M,N,IPGQ,L),M=0,MIN0(N,MMAX)),N=0,NMAX)
  110       CONTINUE
        CLOSE( 11 )
      ELSE
C       Isothermal state of rest
        DO 140 L=1,NLEV
          DO 120 IPGQ=1,NPGQ
  120       CALL ZEROSP( PGQSP0(0,0,IPGQ,L), N1 )
C         Initial potential temperature distribution (isothermal)
          TEMP= (PSURF/PLV(L))**KAPPA
          DO 130 K=1,K2
            DO 130 J=1,K1
  130         FPH(J,K)= TEMP
            CALL SPECTR( PGQSP0(0,0,JPOT,L), FPH )
  140       CONTINUE
C       Initialize backward scheme for first time-step
        TIM= 0.0
        CALL DDTINI( 0.5 * DT1, IMPLCT )
C       Compute backward time-tendency
        CALL DDTPGQ( DPGQSP, PGQSP0, PGQSP0 )
        CALL DDTFRC( DPGQSP, PGQSP0, PGQSP0 )
        CALL DDTIMP( DPGQSP )
        DO 150 L=1,NLEV
          DO 150 IPGQ=JVOR,JPOT
            DO 150 N=0,N1
              DO 150 M=0,MIN0(N,M1)
              PGQSP1(M,N,IPGQ,L)= PGQSP0(M,N,IPGQ,L)
     1                            + DT1 * DPGQSP(M,N,IPGQ,L)
  150         CONTINUE
      ENDIF
C     Initialize leap-frog time-stepping
      CALL DDTINI( DT1, IMPLCT )
C
  200 WRITE(*,*) 'NSTEP=?, NSKIP=?'
      READ(*,*) NSTEP, NSKIP
      IF (NSTEP.LT.0) GOTO 700
#ifdef GRAPHICS
C     Read display parameters
      WRITE(*,*) 'IDISP=?, FDISP=?'
      READ(*,*) IDISP, FDISP
#endif
      WRITE(*,'(1X,A7,3A15)') 'TIM', 'UTROP', 'EKE', 'APE'
C     Time-march
      DO 600 IT=0,NSTEP
        IF (IT.GT.0) THEN
          TIM= TIM + DT1
C         Compute leap-frog "adiabatic" time tendency
          CALL DDTPGQ( DPGQSP, PGQSP0, PGQSP1 )
C         Include effects of forcing/damping
          CALL DDTFRC( DPGQSP, PGQSP0, PGQSP1 )
C         Make implicit corrections to tendency
          CALL DDTIMP( DPGQSP )
C         Leap-frog to next time-step
          DO 220 L=1,NLEV
            DO 220 IPGQ=JVOR,JPOT
              DO 210 N=0,N1
                DO 210 M=0,MIN0(N,M1)
                  FSP1(M,N)= PGQSP0(M,N,IPGQ,L)
     1         + 2.0 * DT1 * DPGQSP(M,N,IPGQ,L)
  210             CONTINUE
C             Apply Robert filter
              CALL ROBFIL( PGQSP0(0,0,IPGQ,L), PGQSP1(0,0,IPGQ,L),
     1                     FSP1, ROBFAC)
              CALL SPCOPY( PGQSP0(0,0,IPGQ,L), PGQSP1(0,0,IPGQ,L), N1)
              CALL SPCOPY( PGQSP1(0,0,IPGQ,L), FSP1, N1)
  220         CONTINUE
        ENDIF
C
        IF (MOD(IT,NSKIP).EQ.0) THEN
C         Calculate and display diagnostics
C         Initialize omega at the upper boundary
          CALL ZEROSP( WSP, N1 )
          EKE= 0.0
          APE= 0.0
          PIETEN= 0.0
          UTROP= 0.0
          DO 390 L=1,L1
C           Compute omega at level L
C           (Actually at level L+1/2)
            DO 310 N=0,N1
              DO 310 M=0,MIN0(N,M1)
  310           WSP(M,N)= WSP(M,N) - DP(L) * PGQSP1(M,N,JDIV,L)
            CALL PHYSIC( FPH, WSP )
            CALL ZAVGE( WBAR(1,L), FPH )
C           Compute horizontal velocity at level L
            CALL SPCOPY( FSP1, PGQSP1(0,0,JVOR,L), N1)
            CALL SPCOPY( FSP2, PGQSP1(0,0,JDIV,L), N1)
            CALL IDELSQ( FSP1, FSP1, N1)
            CALL IDELSQ( FSP2, FSP2, N1)
            CALL HVELOC( VEL(1,1,JX), VEL(1,1,JY), FSP1, FSP2 )
C           Compute zonal-mean u
            CALL ZAVGE( UBAR(1,L), VEL(1,1,JX) )
            CALL ZAVGE( VBAR(1,L), VEL(1,1,JY) )
C           Compute vertically averaged u-bar in the deep tropics
            UTROP= UTROP + ( UBAR(KMID-2,L)+UBAR(KMID-1,L)+UBAR(KMID,L)
     1                      +UBAR(KMID+1,L)+UBAR(KMID+2,L) ) / (5.*L1)
C           Compute eddy kinetic energy at level L
            CALL EDDY( VEL(1,1,JX), VEL(1,1,JX) )
            CALL EDDY( VEL(1,1,JY), VEL(1,1,JY) )
            DO 320 K=1,K2
              DO 320 J=1,K1
  320           FPH(J,K)= 0.5 * (VEL(J,K,JX)**2 + VEL(J,K,JY)**2)
            EKE= EKE + (DP(L)/PSURF) * GAVGE( FPH )
C           Compute THETA
            CALL PHYSIC( FPH, PGQSP1(0,0,JPOT,L) )
C           Compute zonal-mean THETA
            CALL ZAVGE( TBAR(1,L), FPH )
C           Compute moments of THETA
            THET1L= GAVGE( FPH )
            DO 330 K=1,K2
              DO 330 J=1,K1
  330           FPH(J,K)= FPH(J,K)**2
            THET2L= GAVGE( FPH )
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
  390       CONTINUE
          WRITE(*,'(1X,F7.2,1P,3E15.6)') TIM/86400., UTROP, EKE, APE
#ifdef GRAPHICS
C         Display diagnostics
          GOTO (410, 420, 430, 440, 450, 460, 470, 480, 490), IDISP
C         Display u-bar cross section
  410     CALL DISCRS( UBAR, 0, 0.0, 0.0, 'U bar' )
          GOTO 590
C         Display THETA cross section
  420     CALL DISCRS( TBAR, 0, 0.0, 0.0, 'T bar' )
          GOTO 590
C         Display W cross section
  430     CALL DISCRS( WBAR, 0, 0.0, 0.0, 'W bar' )
          GOTO 590
C         Display streamfunction
  440     DO 445 K=1,K2
  445       VCROSS(K,L1)= 0.0
          DO 447 L=L1-1,1,-1
            DO 447 K=1,K2
  447         VCROSS(K,L)= VCROSS(K,L+1)-DP(L+1)*COSPHI(K)*VBAR(K,L+1)
          CALL DISCRS( VCROSS, 0, 0.0, 0.0, 'Psi-bar' )
          GOTO 590
C         Display MMC cross section
  450     DO 455 K=1,K2
  455       VCROSS(K,L1)= 0.0
          DO 457 L=1,L1-1
            TEMP= (ZSTDLV(L+1)-ZSTDLV(L))/(PLV(L+1)-PLV(L))*A0/90000.
            DO 457 K=1,K2
              VCROSS(K,L)= TEMP * WBAR(K,L)
  457         CONTINUE
          CALL DISVCS( VBAR, VCROSS, FDISP, 'MMC' )
          GOTO 590
C         Display U bar at specified level
  460     CALL DISPRF( UBAR(1,NINT(FDISP)), -20.0, 60.0, 'U bar' )
          GOTO 590
C         Display V bar at specified level
  470     CALL DISPRF( VBAR(1,NINT(FDISP)), -1.0, 1.0, 'V bar' )
          GOTO 590
C         Display rel-vorticity at specified level
  480     CALL PHYSIC( FPH, PGQSP1(0,0,JVOR,NINT(FDISP)) )
          CALL DISFLD( FPH, 0, 0.0, 0.0, 'rel-vor' )
          GOTO 590
C         Display divergence at specified level
  490     CALL PHYSIC( FPH, PGQSP1(0,0,JDIV,NINT(FDISP)) )
          CALL DISFLD( FPH, 0, 0.0, 0.0, 'diverg' )
          GOTO 590
C
  590     CALL ZFRAME
#endif
        ENDIF
  600   CONTINUE
      GOTO 200
C     Termination sequence
  700 WRITE(*,*) 'Save file name? (blank for no save)'
      READ(*,'(A)') FILNAM
      IF (FILNAM.NE.' ') THEN
        OPEN( 11, FILE=FILNAM )
        WRITE(11,'(1X,A72)') RUNTTL
        WRITE(11,'(1X,A72)') PRGSTR
        WRITE(11,'(1X,1P,4I4,2E15.6)')MMAX,NMAX,NLEV,IEXPT,PLID,TIM
        DO 710 L=1,NLEV
          DO 710 IPGQ=JVOR,JPOT
           WRITE(11,FMT)((PGQSP0(M,N,IPGQ,L),M=0,MIN0(N,MMAX)),N=0,NMAX)
           WRITE(11,FMT)((PGQSP1(M,N,IPGQ,L),M=0,MIN0(N,MMAX)),N=0,NMAX)
  710      CONTINUE
        CLOSE( 11 )
        WRITE(*,*) 'Current model state saved to ', FILNAM
      ENDIF
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
        VCOORD(L1+1-L)= ZSTDLV(L)/1000.
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
   10   VCOORD(L1+1-L)= ZSTDLV(L)/1000.
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
   10   VCOORD(L1+1-L)= ZSTDLV(L)/1000.
      DO 20 L=1,L1
        DO 20 K=1,K2
          VCROS1(K,L1+1-L)= VPHX(K,L)
          VCROS2(K,L1+1-L)= VPHY(K,L)
   20     CONTINUE
C     (Round off range to tens of kilometres)
      TEMP= INT(0.9999 + ZSTDLV(1)/10000.)*10.0
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
