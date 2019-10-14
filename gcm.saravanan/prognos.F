#ifndef MCONS
#   include "mcons.h"
#endif
C Prognos - version 2.3, created November 19, 1990
C A mechanistic spectral primitive equation model, using pressure coordinates
C
C Author: R. Saravanan, D.A.M.T.P., University of Cambridge,
C         Silver Street, Cambridge CB3 9EW, U.K.
C         E-mail: svn@atm.amtp.cam.ac.uk
C (Please send bug-reports/comments to above address)
C
C This software is distributed free for research purposes. It comes with
C absolutely no warranties whatsoever. It may be freely copied, and may also
C be modified, provided the modified software continues to be freely
C available under the same terms. This software may not be used for any
C commercial purpose.
C
C ---------------------------------------------------------------------
C
C The following C preprocessor parameters are used by this module
C They may be redefined by the user.
C
C L1MAX    -- maximum no. of pressure-levels (>=2)
C             (Default value is 20)
C NTRACE   -- number of "passive" tracers (>= 0)
C             (Default value is 0)
C FIXTRUNC -- This parameter, if defined, fixes the vertical truncation
C             to be equal to L1MAX. This may lead to more efficient
C             execution in some cases.
C(Note: This parameter also affects the spectral transform module)
C
C DEBUG    -- This parameter, if defined, produces debugging diagnostics
C
C Derived parameters
C
C NPGQ     -- number of prognostic quantities per level (3+NTRACE)
C
C "Enumeration literals"
C
C Prognostic quantities
C JVOR   -- denotes relative vorticity
C JDIV   -- denotes horizontal divergence
C JPOT   -- denotes potential temperature
C JTR1   -- denotes tracer #1
C
C Vector components
C JX     -- denotes zonal direction
C JY     -- denotes meridional direction
C
C     Planetary parameters
C     REAL RADIUS, OMEGA0, F0, FSP01, RGAS, CP, KAPPA, G0
C     COMMON/PLPARM/  RADIUS, OMEGA0, F0, FSP01, RGAS, CP, KAPPA, G0
C
C RADIUS -- planetary radius (in metres)
C OMEGA0 -- angular velocity of planetary rotation (in 1/sec)
C F0     -- coriolis parameter (= 2 * OMEGA0)
C FSP01  -- F0 / SQRT(3.0)
C          (F0 expressed as coefficient of spherical harmonic Y[0,1])
C RGAS   -- specific gas constant (in J/(kg*K))
C CP     -- specific heat at constant pressure (in J/(kg*K))
C KAPPA  -- RGAS / CP (an adiabatic exponent)
C G0     -- gravitational acceleration (in m/s**2)
C
#ifndef FIXTRUNC
C     INTEGER L1
C     COMMON/VGRID1/ L1
#endif
C     REAL PSURF, DP, HDP, PLV, PHLV, PKLV, PKCHLV
C     REAL TSTDLV, ZSTDLV, TREFLV
C     COMMON/VGRID2/ PSURF, DP(L1MAX), HDP(L1MAX), PLV(L1MAX),
C    1               PHLV(0:L1MAX), PKLV(L1MAX), PKCHLV(L1MAX-1),
C    2               TSTDLV(L1MAX), ZSTDLV(L1MAX), TREFLV(L1MAX)
C
C     REAL TRFFAC, QHDAMP, UD8FAC, TD8FAC
C     REAL DCAP2D, D2DCAP, D2W, D2TT, T2GPCP, NNT2DT, IMPCOR
C     COMMON/VGRID3/ TRFFAC, QHDAMP, UD8FAC(0:N1MAX), TD8FAC(0:N1MAX),
C    1  DCAP2D(L1MAX,L1MAX-1), D2DCAP(L1MAX-1,L1MAX),
C    2  D2W(L1MAX-1,L1MAX), D2TT(L1MAX,L1MAX),
C    3  T2GPCP(L1MAX-1,L1MAX), NNT2DT(L1MAX,L1MAX),
C    4  IMPCOR(L1MAX,L1MAX,0:N1MAX)
C
C L1       -- number of pressure-levels ( >= 2)
C PSURF    -- surface pressure (in N/m**2)
C DP(L)    -- pressure-thickness of L-th level
C HDP(L)   -- (1/2) * DP(L)
C PLV(L)   -- pressure in the middle of L-th level
C PHLV(L)  -- pressure values at half-levels (PLV(L)+HDP(L))
C             (PHLV(0)= 0.0, PHLV(L1)= PSURF)
C PKLV(L)  -- (PLV(L)/PSURF)**KAPPA at L-th pressure level
C PKCHLV(L)-- vertical differential of PKLV (PK-cap)
C             {i.e. (1/2)*(PKLV(L+1) - PKLV(L)) }
C TSTDLV(L)-- standard value of THETA at L-th level
C ZSTDLV(L)-- standard height above surface (in metres) of L-th level
C             (computed using the standard value of THETA)
C TREFLV(L)-- reference value of THETA at L-th level
C             (used to linearize THETA for semi-implicit stepping)
C TRFFAC   -- steepness factor for reference THETA profile used in
C             semi-implicit scheme (as compared to the standard
C             THETA profile)
C QHDAMP   -- Del**8 diffusion coefficient (in units of A0**8/sec)
C UD8FAC(N)-- Del**8 damping factor for momentum
C TD8FAC(N)-- Del**8 damping factor for pot. temperature
C DCAP2D   -- DCAP to D conversion matrix
C D2DCAP   -- D to DCAP conversion matrix
C D2W      -- D to W conversion matrix
C D2TT     -- D to THETA tendency conversion matrix
C T2GPCP   -- THETA to GeoPotential-CAP conversion matrix
C NNT2DT   -- THETA to D-tendency (without Del-2 factor) conversion
C              matrix (= DCAP2D . T2GPCP)
C IMPCOR   -- implicit correction matrix
C
C     REAL IMPFAC, DT, DTFAC, UD8COR, TD8COR
C     COMMON/TMAR1/ IMPFAC, DT, DTFAC, UD8COR(0:N1MAX), TD8COR(0:N1MAX)
C     REAL USDRAG, URLXLV, VVISC, TRLXLV, TMNLV
C     COMMON/CDAMP/ USDRAG, URLXLV(L1MAX), VVISC(L1MAX-1),
C    1              TRLXLV(L1MAX), TMNLV(0:N1MAX,L1MAX)
C
C IMPFAC   -- implicitness factor (0.0=explicit ... 1.0=fully implicit)
C DT       -- time-step (in seconds)
C DTFAC    -- IMPFAC * 2.0 * DT
C UD8COR(N)-- Del**8 implicit correction factor for vorticity/divergence
C TD8COR(N)-- Del**8 implicit correction factor for THETA
C USDRAG   -- surface drag coefficient (in units of pressure/time)
C URLXLV(L)-- vertical profile of Rayleigh damping coefficient
C VVISC(L) -- vertical viscosity coefficient at level L+1/2
C             (in units of (pressure)**2/time)
C TRLXLV(L)-- vertical profile of Newtonian damping coefficient
C TMNLV(N,L)--spectral Legendre-pressure coeffs. of zonally symmetric
C             mean THETA distribution
C
C ------------------- End of comments ---------------------------------
C
      SUBROUTINE PLINI(OMEGA, R, CPRESS, GRAVIT)
C PLINI initializes the common block /PLPARM/ which contains the
C planetary parameters.
C Input:  REAL OMEGA -- angular velocity of planetary rotation (in 1/s)
C         REAL R -- gas constant {in J/(kg*K)}
C         REAL CPRESS -- specific heat at const. pressure {in J/(kg*K)}
C         REAL GRAVIT -- gravitational acceleration (in m/s**2)
C Output: Common block /PLPARM/ is initialized
C
C
#include "mgrid.h"
      REAL OMEGA, R, CPRESS, GRAVIT
C
      OMEGA0= OMEGA
      F0= 2.0 * OMEGA0
      FSP01= F0 / SQRT(3.0)
      RGAS= R
      CP= CPRESS
      KAPPA= RGAS / CP
      G0= GRAVIT
      RETURN
      END
 
      SUBROUTINE VERINI( NLEV, PTHICK, TZSTD, TSTEEP, DEL8DF,
     1                   TRADEQ, TDAMP, UDAMP, SFRIC, VERVIS)
C
C VERINI initializes the vertical truncation for a spectral pressure
C coordinate model (using the Lorenz (1960) finite-differencing, with
C vertically averaged divergence set to zero).
C
C Note: Prior to calling VERINI, the horizontal truncation should have
C       been initialized by a call to routine SPINI in module sptran.F.
C       Also, the planetary parameters should have been initialized by
C       a call to routine PLINI in this module.
C
C The prognostic quantities are vorticity, divergence, and
C potential temperature (THETA) in each level.
C
C NLEV is the number of pressure levels in the vertical truncation.
C    (NLEV >= 2)
C PTHICK(NLEV) is the pressure-thickness of each level (in SI units).
C    (Note: The upper boundary boundary is always assumed to be at zero
C     pressure. This means that the surface pressure is simply the
C     sum of all pressure thicknesses)
C TZSTD(NLEV) is the pressure profile of the mean THETA distribution.
C    (Note: A more stably stratified version of TZSTD is used to define
C    the reference THETA profile for the semi-implicit time-stepping.
C    TZSTD is also used to calculate standard height-levels
C    corresponding to the pressure levels)
C TSTEEP is the steepness factor for the reference vertical THETA
C    profile used for semi-implicit time-stepping. i.e. The reference
C    THETA profile is steeper than TZSTD by a factor TSTEEP.
C    (One would typically choose TSTEEP > 1.0)
C DEL8DF is the del-8 (quad-harmonic) diffusion coefficient
C    (in units of A0**8/sec, where A0 is the planetary radius)
C TRADEQ(K2MAX,NLEV) is the latitude-pressure field of the "radiative
C    equilibrium" THETA distribution.
C TDAMP(NLEV) is the pressure dependent radiative damping coefficient
C    (in units of 1/sec) that determines relaxation of THETA back to
C    TRADEQ.
C UDAMP(NLEV) is the pressure dependent Rayleigh friction coefficient
C    (in units of  1/sec) that damps velocity.
C SFRIC is the surface rayleigh friction coefficient (in units of 1/sec)
C    that damps momentum in the lowermost level.
C VERVIS is the vertical viscosity coefficient (in units of m**2/s) that
C    diffuses momentum.
C
C Lorenz, E.N. 1960: Energy and numerical weather prediction,
C    TELLUS,vol.12,pp.364-373.
C
#include "mgrid.h"
#include "tmarch.h"
      INTEGER NLEV
      REAL PTHICK(NLEV), TZSTD(NLEV), TSTEEP, DEL8DF
      REAL TRADEQ(K2MAX, NLEV), TDAMP(NLEV), UDAMP(NLEV), SFRIC, VERVIS
      INTEGER J, K, L, L0, N
      REAL W2TT(L1MAX,L1MAX-1), FPH(K1MAX,K2MAX)
      COMPLEX FSP(0:M1MAX,0:N1MAX)
#ifdef DEBUG
      REAL W2D(L1MAX,L1MAX-1), WK(L1MAX,L1MAX)
#endif
 
#ifdef FIXTRUNC
      IF (NLEV.NE.L1)
     1  CALL SPERR( 'VERINI', 'Incompatible vertical truncation')
#else
 
      IF (NLEV.GT.L1MAX)
     1  CALL SPERR( 'VERINI', 'Too many pressure levels')
      L1= NLEV
#endif
 
C     Initialize pressure related parameters
      PHLV(0)= 0.0
      DO 10 L=1,L1
        DP(L)= PTHICK(L)
        HDP(L)= 0.5 * DP(L)
        PLV(L)= PHLV(L-1) + HDP(L)
        PHLV(L)= PHLV(L-1) + DP(L)
   10   CONTINUE
      PSURF= PHLV(L1)
C     Standard vertical THETA profile, and coordinate PK
      DO 20 L=1,L1
        PKLV(L)= (PLV(L)/PSURF)**KAPPA
        TSTDLV(L)= TZSTD(L)
   20   CONTINUE
C     Compute PK-cap
      DO 30 L=1,L1-1
   30   PKCHLV(L)= 0.5 * (PKLV(L+1) - PKLV(L))
C     Compute standard height values, using the hydrostatic relation.
C     Also initialize reference THETA profile(for semi-implicit scheme).
C     The reference THETA profile is simply a steeper version of the
C     standard THETA profile (steeper by a factor TRFFAC).
C
      TRFFAC= TSTEEP
      ZSTDLV(L1)= CP*(1.0-PKLV(L1))*TSTDLV(L1) / G0
C     Reference THETA profile coincides with standard profile at surface
      TREFLV(L1)= TSTDLV(L1)
      DO 40 L=L1-1,1,-1
        ZSTDLV(L)= ZSTDLV(L+1) +
     1             CP*PKCHLV(L)*(TSTDLV(L+1)+TSTDLV(L)) / G0
        TREFLV(L)= TREFLV(L+1) + TRFFAC * (TSTDLV(L)-TSTDLV(L+1))
   40   CONTINUE
#ifdef DEBUG
      CALL MATOUT(PLV, 1, 1, L1, 'PLV')
      CALL MATOUT(PKLV, 1, 1, L1, 'PKLV')
      CALL MATOUT(ZSTDLV, 1, 1, L1, 'ZSTDLV')
      CALL MATOUT(TREFLV, 1, 1, L1, 'TREFLV')
#endif
C     DCAP to D conversion matrix
      DO 50 L=1,L1
        DO 50 L0=1,L1-1
          IF (L0.LT.L) THEN
            DCAP2D(L,L0)= 2.0 *  PHLV(L0)/PHLV(L1)
          ELSE
            DCAP2D(L,L0)= 2.0 * (PHLV(L0)/PHLV(L1) - 1.0)
          ENDIF
   50     CONTINUE
C     D to DCAP conversion matrix ("vertical derivative")
      DO 60 L=1,L1-1
        DO 60 L0=1,L1
          IF (L0.EQ.(L+1)) THEN
            D2DCAP(L,L0)=  0.5
          ELSE IF (L0.EQ.L) THEN
            D2DCAP(L,L0)= -0.5
          ELSE
            D2DCAP(L,L0)=  0.0
          ENDIF
   60     CONTINUE
#ifdef DEBUG
      CALL MATOUT(D2DCAP, L1MAX-1, L1-1, L1,   'D2DCAP')
      CALL MATOUT(DCAP2D, L1MAX,   L1,   L1-1, 'DCAP2D')
      CALL MATMUL(WK(1,1), L1MAX, D2DCAP, L1MAX-1, DCAP2D, L1MAX,
     1            L1-1, L1, L1-1 )
      CALL MATOUT(WK, L1MAX, L1-1, L1-1, 'D2DCAP*DCAP2D')
#endif
C     D to W conversion matrix
      DO 70 L=1,L1-1
        DO 70 L0=1,L1
          IF (L0.LE.L) THEN
            D2W(L,L0)= -DP(L0)
          ELSE
            D2W(L,L0)= 0.0
          ENDIF
   70     CONTINUE
#ifdef DEBUG
C     W to D conversion matrix
      DO 75 L=1,L1
        DO 75 L0=1,L1-1
          IF (L0.EQ.(L-1)) THEN
            W2D(L,L0)=  1.0/DP(L)
          ELSE IF (L0.EQ.L) THEN
            W2D(L,L0)= -1.0/DP(L)
          ELSE
            W2D(L,L0)=  0.0
          ENDIF
   75     CONTINUE
      CALL MATOUT(D2W, L1MAX-1, L1-1, L1,   'D2W')
      CALL MATOUT(W2D, L1MAX,   L1,   L1-1, 'W2D')
      CALL MATMUL(WK(1,1), L1MAX, D2W, L1MAX-1, W2D, L1MAX,
     1            L1-1, L1, L1-1 )
      CALL MATOUT(WK, L1MAX, L1-1, L1-1, 'D2W*W2D')
#endif
C     W to THETA tendency conversion matrix (Static stability)
      DO 80 L=1,L1
        DO 80 L0=1,L1-1
          IF (L0.EQ.(L-1)) THEN
            W2TT(L,L0)= -0.5 * (TREFLV(L)-TREFLV(L-1)) / DP(L)
          ELSE IF (L0.EQ.L) THEN
            W2TT(L,L0)= -0.5 * (TREFLV(L+1)-TREFLV(L)) / DP(L)
          ELSE
            W2TT(L,L0)= 0.0
          ENDIF
   80     CONTINUE
C     D to THETA-tendency conversion matrix
C     (Composite of W2TT and D2W)
      CALL MATMUL(D2TT, L1MAX, W2TT, L1MAX, D2W, L1MAX-1,
     1            L1, L1-1, L1)
C     THETA to GeoPotential-CAP conversion matrix (Hydrostatic relation)
      DO 90 L=1,L1-1
        DO 90 L0=1,L1
          IF ( (L0.EQ.L) .OR. (L0.EQ.(L+1)) ) THEN
            T2GPCP(L,L0)= -0.5 * CP * PKCHLV(L)
          ELSE
            T2GPCP(L,L0)=  0.0
          ENDIF
   90     CONTINUE
C     THETA to D-tendency (without -Del**2 factor) conversion matrix
C     (Composite of DCAP2D and T2GPCP)
      CALL MATMUL(NNT2DT, L1MAX, DCAP2D, L1MAX, T2GPCP, L1MAX-1,
     1            L1, L1-1, L1)
#ifdef DEBUG
      CALL MATOUT(W2TT,    L1MAX,   L1,   L1-1, 'W2TT'  )
      CALL MATOUT(D2TT,    L1MAX,   L1,   L1,   'D2TT'  )
      CALL MATOUT(T2GPCP,  L1MAX-1, L1-1, L1,   'T2GPCP')
      CALL MATOUT(NNT2DT,  L1MAX,   L1,   L1,   'NNT2DT')
#endif
C     Del**8 sub-grid-scale diffusion
      QHDAMP= DEL8DF
      DO 100 N=0,N1
C       Scale-dependent del**8 damping factor
C       (Note: QHDAMP is in units such that A0 = 1)
        TD8FAC(N) =  QHDAMP * FLOAT( N*(N+1)   )**4
        UD8FAC(N) =  QHDAMP * FLOAT( N*(N+1)-2 )**4
  100   CONTINUE
#ifdef DEBUG
      CALL MATOUT(TD8FAC(0), 1, 1, N1+1, 'TD8FAC')
      CALL MATOUT(UD8FAC(0), 1, 1, N1+1, 'UD8FAC')
#endif
C     Mean thermal state, and Rayleigh/Newtonian damping coefficients
      DO 130 L=1,L1
        URLXLV(L)= UDAMP(L)
        TRLXLV(L)= TDAMP(L)
C       Compute spectral coefficients of zonally symmetric mean THETA
        DO 110 K=1,K2
          DO 110 J=1,K1
  110       FPH(J,K)= TRADEQ(K,L)
        CALL ZEROSP(FSP, N1)
        CALL SPECTR(FSP, FPH)
        DO 120 N=0,N1
  120     TMNLV(N,L)= REAL( FSP(0,N) )
  130   CONTINUE
C     Surface drag (in units of (pressure/time) )
      USDRAG= DP(L1) * SFRIC
C     Convert viscosity coefficient from z-coordinate to p-coordinate,
C     using the standard height values
      DO 140 L=1,L1-1
        VVISC(L)= VERVIS * ( (HDP(L+1)  + HDP(L)     ) /
     1                       (ZSTDLV(L) - ZSTDLV(L+1))  )**2
  140   CONTINUE
#ifdef DEBUG
      CALL MATOUT(VVISC, 1, 1, L1-1, 'VVISC')
#endif
      RETURN
      END
 
      SUBROUTINE MATMUL(A, NA0, B, NB0, C, NC0, NROWB, NCOLB, NCOLC)
C MATMUL multiplies two matrices together to produce a third matrix.
C  A = B . C
C Input:  INTEGER NA0, NB0, NC0 -- inner dimension of matrices A, B, C
C         INTEGER NROWB, NCOLB -- no. of rows and columns of matrix B
C         INTEGER NCOLC -- no. of columns of matrix C
C         REAL B(NB0,NCOLB), C(NC0,NCOLC) -- matrices to be multiplied
C Output: REAL A(NA0,NCOLC) -- matrix A = B . C (with NROWB rows)
C
      INTEGER NA0, NB0, NC0, NROWB, NCOLB, NCOLC
      REAL A(NA0,NCOLC), B(NB0,NCOLB), C(NC0,NCOLC)
      INTEGER I, J, K
      REAL TEMP
      DO 20 I=1,NROWB
        DO 20 K=1,NCOLC
          TEMP= 0.0
          DO 10 J=1,NCOLB
   10       TEMP= TEMP + B(I,J) * C(J,K)
          A(I,K)= TEMP
   20     CONTINUE
      RETURN
      END
 
      SUBROUTINE MATINV(AINV, NAINV0, A, NA0, NA)
C MATINV finds the inverse of a square matrix A.
C Input:  INTEGER NAINV0, NA0 -- inner dimension of matrices AINV, A
C         INTEGER NA -- no. of rows/columns in A
C (Note: NA must be <= L1MAX)
C         REAL A(NA0,NA) -- matrix A
C Output: REAL AINV(NAINV0,NA) -- inverse of A
C
      INTEGER NAINV0, NA0, NA
      REAL AINV(NAINV0,NA), A(NA0,NA)
#include "mgrid.h"
      REAL8 A2(L1MAX,L1MAX), B(L1MAX,L1MAX), C(L1MAX,L1MAX)
      REAL8 WKSPCE(L1MAX), AA(L1MAX,L1MAX), BB(L1MAX,L1MAX)
      INTEGER I, J, IFAIL
#ifdef DEBUG
      REAL WK(L1MAX,L1MAX)
#endif
C
C     IF (NA.GT.L1MAX) CALL SPERR( 'MATINV', 'Matrix too large')
C     Initialize temporary identity matrix
      DO 10 I=1,NA
        DO 10 J=1,NA
C         Copy matrix to correct precision
          A2(I,J)= A(I,J)
C         Create Identity matrix for RHS
          IF (I.EQ.J) THEN
            B(I,J)= 1.0
          ELSE
            B(I,J)= 0.0
          ENDIF
   10     CONTINUE
C     Call Nag routine
      IFAIL= 0
      CALL F04AEF(A2, L1MAX, B, L1MAX, NA, NA, C, L1MAX,
     1            WKSPCE, AA, L1MAX, BB, L1MAX, IFAIL)
      IF (IFAIL.NE.0)
     1  CALL SPERR( 'MATINV', 'Unable to invert matrix')
C     Copy inverse back to original precision
      DO 20 I=1,NA
        DO 20 J=1,NA
          AINV(I,J)= C(I,J)
   20     CONTINUE
#ifdef DEBUG
C     Check that the inverse is actually the inverse
      CALL MATMUL(WK, L1MAX, A, NA0, AINV, NAINV0, NA, NA, NA)
      CALL MATOUT(WK, L1MAX, NA, NA, 'A*AINV')
#endif
      RETURN
      END
 
#ifdef DEBUG
      SUBROUTINE MATOUT(A, NA0, NROWA, NCOLA, STR)
C MATOUT prints out a matrix A(NA0,NCOLA), with NROWA rows on
C standard output. STR is the name of the matrix.
C (This routine is used for debugging purposes)
      INTEGER NA0, NROWA, NCOLA
      REAL A(NA0,NCOLA)
      CHARACTER*(*) STR
      INTEGER I, J
      WRITE(*,'(1X,A,A,A,I3,A,I3,A)') 'Matrix ', STR,
     1                 '(', NROWA, 'x', NCOLA, ')'
      DO 10 I=1,NROWA
        WRITE(*,'(1X,1P,5E15.6)') (A(I,J), J=1,NCOLA)
        IF (NCOLA.GT.5) WRITE(*,*)
   10   CONTINUE
      WRITE(*,*)
      RETURN
      END
#endif
 
      SUBROUTINE DDTINI( DT1, IMPLCT )
C DDTINI sets up coefficients for time integration using routine
C DDTPGQ. DT1 is the time-step (in seconds). IMPLCT is the fractional
C implicitness (between 0.0 and 1.0) for time-stepping. The coefficients
C are stored in common module tmarch.h.
C **Note: Normally DDTPGQ uses a partially-implicit leap-frog scheme
C         for time-stepping (using PGQSP0, PGQSP1). But for start-up
C         purposes, you can set DT1 = 0.5 DT, and set PGQSP0 = PGQSP1
C         (i.e. half the actual time step) to get the effect of a
C         partially-implicit forward scheme.
#include "mgrid.h"
#include "tmarch.h"
      REAL DT1, IMPLCT
      REAL RTEMP
      REAL TMPMAT(L1MAX,L1MAX), TMPMA2(L1MAX,L1MAX), TMPMA3(L1MAX,L1MAX)
      INTEGER L, L0, N
#ifdef DEBUG
      INTEGER IFAIL
      REAL8 DET, WK(L1MAX,L1MAX), WK2(L1MAX)
#endif
C     Store time-step
      DT= DT1
C     Store implicitness factor
      IMPFAC= IMPLCT
C     Fractional implicit time-step
      DTFAC= IMPFAC * 2.0 * DT
C     Compute effective ( (-(Del**2)*Dcap) -> (d**2)Dcap/dt**2) matrix
      CALL MATMUL( TMPMA2, L1MAX, T2GPCP, L1MAX-1, D2TT, L1MAX,
     1             L1-1, L1, L1)
      CALL MATMUL( TMPMAT, L1MAX, TMPMA2, L1MAX, DCAP2D, L1MAX,
     1             L1-1, L1, L1-1)
#ifdef DEBUG
      CALL MATOUT(TMPMAT,  L1MAX,   L1-1,   L1-1,   'TMPMAT')
#endif
C     Initialize useful wave-no dependent quantities
      DO 30 N=0,N1
C       Del**8 damping correction factor
        TD8COR(N) =  1.0 / (1.0 + DTFAC * TD8FAC(N))
        UD8COR(N) =  1.0 / (1.0 + DTFAC * UD8FAC(N))
C
C       Compute implicit correction matrix
        RTEMP= (A0QINV*N*(N+1)) * UD8COR(N) * TD8COR(N) * DTFAC**2
        DO 20 L=1,L1-1
          DO 10 L0=1,L1-1
            TMPMA2(L,L0)= -RTEMP * TMPMAT(L,L0)
   10       CONTINUE
C         Add identity matrix
          TMPMA2(L,L)= TMPMA2(L,L) + 1.0
   20     CONTINUE
C       Invert matrix
        CALL MATINV( TMPMA3, L1MAX, TMPMA2, L1MAX, L1-1 )
#ifdef DEBUG
        CALL MATOUT(TMPMA3,  L1MAX,   L1-1,   L1-1,   'TMPINV')
        DO 25 L=1,L1-1
          DO 25 L0=1,L1-1
   25       WK(L,L0)= TMPMA3(L,L0)
        IFAIL= 0
        CALL F03AAF( WK, L1MAX, L1-1, DET, WK2, IFAIL )
        WRITE(*,'(A,I3,A,1PD15.6,A,I2)') 'Determinant of IMPCOR(,,',
     1                  N, ')= ', DET, '   ;IFAIL= ', IFAIL
#endif
        CALL MATMUL( TMPMA2, L1MAX, TMPMA3, L1MAX, D2DCAP, L1MAX-1,
     1               L1-1, L1-1, L1)
        CALL MATMUL( IMPCOR(1,1,N), L1MAX, DCAP2D, L1MAX, TMPMA2, L1MAX,
     1               L1, L1-1, L1)
   30   CONTINUE
      RETURN
      END
 
      SUBROUTINE DDTPGQ( DPGQSP, PGQSP0, PGQSP1 )
C DDTPGQ calculates DPGQSP, the "adiabatic" time tendency of prognostic
C quantities in spectral space at some model time. (i.e. The only
C damping effect included in these calculations is the scale-selective
C Del**8 damping. Any other damping effects will need to be evaluated
C separately)
C Note: The coefficients required for time stepping must be initialized
C        by calling DDTINI before the first call to DDTPGQ.
C Input:  COMPLEX PGQSP0(0:M1MAX,0:N1MAX,NPGQ,L1MAX) -- spectral
C         representation of prognostic quantity at (current time) - DT1
C         COMPLEX PGQSP1(0:M1MAX,0:N1MAX,NPGQ,L1MAX) -- spectral
C         representation of prognostic quantity at current model time
C Output: COMPLEX DPGQSP(0:M1MAX,0:N1MAX,NPGQ,L1MAX) -- spectral
C                   "adiabatic" time-tendency of prognostic quantities
C
#include "mgrid.h"
#include "tmarch.h"
      COMPLEX DPGQSP(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      COMPLEX PGQSP0(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      COMPLEX PGQSP1(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      REAL U(K1MAX, K2MAX, JX:JY, 0:1), TP(K1MAX, K2MAX, 0:1)
      REAL VUFLX(K1MAX, K2MAX, JX:JY, 0:1), VTPFLX(K1MAX, K2MAX, 0:1)
      REAL W(K1MAX, K2MAX, 0:1)
      REAL UBAR(K1MAX, K2MAX, JX:JY), TPBAR(K1MAX, K2MAX)
      REAL ABSVOR(K1MAX, K2MAX), DIV(K1MAX, K2MAX)
      REAL FLXPH(K1MAX, K2MAX, JX:JY), FPH(K1MAX, K2MAX), TEMP
      COMPLEX FSP1(0:M1MAX,0:N1MAX), FSP2(0:M1MAX,0:N1MAX)
      INTEGER LMHALF, LPZERO, LPHALF, LPONE
C     (Mnemonic for L-(1/2), L+0, L+(1/2), L+1)
      INTEGER I, J, K, L, L0, M, N, IPGQ
#if (NTRACE > 0)
      REAL TRC(K1MAX, K2MAX, 0:1), VTRCFX(K1MAX, K2MAX, 0:1)
      REAL TRCBAR(K1MAX, K2MAX)
#endif
C ----------------------------------------------------------------------
C     Initialize all spectral time-tendencies to zero
      DO 10 L=1,L1
        DO 10 IPGQ=1,NPGQ
          CALL ZEROSP( DPGQSP(0,0,IPGQ,L), N1)
   10     CONTINUE
C ----------------------------------------------------------------------
      LMHALF= 0
      LPZERO= 0
C **Note Values of prognostic quantities at current model time
C        (PGQSP1) are used in the following calculations. i.e. The
C        treatment is leap-frog in time.
      DO 300 L=0,L1
        LMHALF= MOD(LMHALF+1, 2)
        LPHALF= MOD(LMHALF+1, 2)
        LPZERO= MOD(LPZERO+1, 2)
        LPONE=  MOD(LPZERO+1, 2)
        IF (L.LT.L1) THEN
C ----------------------------------------------------------------------
C         Compute hor. velocity and "perturbation" THETA at level L+1
C
C         First compute streamfunction PSI and velocity potential CHI
          CALL IDELSQ( FSP1, PGQSP1(0,0,JVOR,L+1), N1)
          CALL IDELSQ( FSP2, PGQSP1(0,0,JDIV,L+1), N1)
C         Then compute horizontal velocity from PSI and CHI
          CALL HVELOC( U(1,1,JX,LPONE), U(1,1,JY,LPONE), FSP1, FSP2 )
C
          CALL SPCOPY( FSP1, PGQSP1(0,0,JPOT,L+1), N1 )
C         Subtract reference value of THETA
          FSP1(0,0)= FSP1(0,0) - TREFLV(L+1)
C         Compute "perturbation" THETA in physical space (at level L+1)
          CALL PHYSIC( TP(1,1,LPONE), FSP1 )
#if (NTRACE > 0)
C         Compute tracer concentration in physical space (at level L+1)
          CALL PHYSIC( TRC(1,1,LPONE), PGQSP1(0,0,JTR1,L+1) )
#endif
C ----------------------------------------------------------------------
        ENDIF
C
        IF (L.GT.0) THEN
C ----------------------------------------------------------------------
C         Compute divergence at level L
          CALL PHYSIC( DIV, PGQSP1(0,0,JDIV,L) )
C ----------------------------------------------------------------------
        ENDIF
C
C       Compute omega and vertical fluxes at level L+1/2
        IF ( (L.EQ.0) .OR. (L.EQ.L1) ) THEN
C ----------------------------------------------------------------------
C         Omega is zero at the boundary half-levels 1/2 and L1+1/2
C         => All vertical fluxes are also zero
          DO 110 K=1,K2
            DO 110 J=1,K1
              W(J,K,LPHALF)= 0.0
              VTPFLX(J,K,LPHALF)= 0.0
#if (NTRACE > 0)
              VTRCFX(J,K,LPHALF)= 0.0
#endif
  110         CONTINUE
          DO 120 I=JX,JY
            DO 120 K=1,K2
              DO 120 J=1,K1
  120           VUFLX(J,K,I,LPHALF)= 0.0
C ----------------------------------------------------------------------
        ELSE
C ----------------------------------------------------------------------
C         L+1/2 is not at the boundaries. Compute omega and vertical
C         fluxes at level L+1/2. First, compute vertically averaged
C         ("bar") quantities at level L+1/2
          DO 130 I=JX,JY
            DO 130 K=1,K2
              DO 130 J=1,K1
                UBAR(J,K,I)= 0.5 * (U(J,K,I,LPZERO) + U(J,K,I,LPONE))
  130           CONTINUE
          DO 140 K=1,K2
            DO 140 J=1,K1
              TPBAR(J,K)=    0.5 * ( TP(J,K,LPZERO) +  TP(J,K,LPONE))
#if (NTRACE > 0)
              TRCBAR(J,K)=   0.5 * ( TRC(J,K,LPZERO) +  TRC(J,K,LPONE))
#endif
  140         CONTINUE
C
C         Compute "pressure velocity" omega at L+1/2, using DIV at L
          DO 150 K=1,K2
            DO 150 J=1,K1
              W(J,K,LPHALF)= W(J,K,LMHALF) - DP(L)*DIV(J,K)
  150         CONTINUE
C         Compute vertical fluxes at level L+1/2
          DO 160 I=JX,JY
            DO 160 K=1,K2
              DO 160 J=1,K1
                VUFLX(J,K,I,LPHALF)= - W(J,K,LPHALF) * UBAR(J,K,I)
  160           CONTINUE
          DO 170 K=1,K2
            DO 170 J=1,K1
              VTPFLX(J,K,LPHALF)= - W(J,K,LPHALF) * TPBAR(J,K)
#if (NTRACE > 0)
              VTRCFX(J,K,LPHALF)= - W(J,K,LPHALF) * TRCBAR(J,K)
#endif
  170         CONTINUE
C ----------------------------------------------------------------------
        ENDIF
C
        IF (L.GT.0) THEN
C ----------------------------------------------------------------------
C         Computing vorticity/divergence tendencies at level L
C
C         Computing absolute vorticity at level L
          CALL SPCOPY( FSP1, PGQSP1(0,0,JVOR,L), N1 )
C         Add planetary vorticity to relative vorticity
          FSP1(0,1)= FSP1(0,1) + CMPLX(FSP01, 0.)
          CALL PHYSIC( ABSVOR, FSP1 )
C         Computing horizontal "flux convergence" of momentum at level L
          DO 210 K=1,K2
            DO 210 J=1,K1
              FLXPH(J,K,JX)= + ABSVOR(J,K) * U(J,K,JY,LPZERO)
     1                          - DIV(J,K) * U(J,K,JX,LPZERO)
              FLXPH(J,K,JY)= - ABSVOR(J,K) * U(J,K,JX,LPZERO)
     1                          - DIV(J,K) * U(J,K,JY,LPZERO)
  210         CONTINUE
C         Add contribution from vertical flux divergence
          TEMP= 1.0 / DP(L)
          DO 220 I=JX,JY
            DO 220 K=1,K2
              DO 220 J=1,K1
                FLXPH(J,K,I)= FLXPH(J,K,I) +
     1               TEMP * (VUFLX(J,K,I,LPHALF)-VUFLX(J,K,I,LMHALF))
  220           CONTINUE
C         Computing vorticity/divergence tendencies from "flux conv."
          CALL CURLZ( DPGQSP(0,0,JVOR,L), FLXPH(1,1,JX), FLXPH(1,1,JY))
          CALL DIVERG(DPGQSP(0,0,JDIV,L), FLXPH(1,1,JX), FLXPH(1,1,JY))
C
C         Compute kinetic energy at level L
          DO 230 K=1,K2
            DO 230 J=1,K1
              FPH(J,K)= 0.5*(U(J,K,JX,LPZERO)**2 + U(J,K,JY,LPZERO)**2)
  230         CONTINUE
C         Convert to spectral space
          CALL ZEROSP( FSP1, N1 )
          CALL SPECTR( FSP1, FPH )
C         Compute the Laplacian, and add contribution to D tendency
          CALL DELSQ( FSP1, FSP1, N1 )
          DO 240 N=0,N1
            DO 240 M=0,MIN0(N,M1)
              DPGQSP(M,N,JDIV,L)= DPGQSP(M,N,JDIV,L) - FSP1(M,N)
  240         CONTINUE
C
C **Note: Contribution from geopotential still to be added to D tendency
C
C         Computing THETA tendency at level L
C
C         Compute horizontal "flux" of "perturbation" THETA
          DO 250 I=JX,JY
            DO 250 K=1,K2
              DO 250 J=1,K1
                FLXPH(J,K,I)= - TP(J,K,LPZERO) * U(J,K,I,LPZERO)
  250           CONTINUE
C         Compute divergence of horizontal "flux"
          CALL DIVERG(DPGQSP(0,0,JPOT,L), FLXPH(1,1,JX), FLXPH(1,1,JY))
C
C         Compute vertical "perturbation" THETA flux convergence
          TEMP= 1.0 / DP(L)
          DO 260 K=1,K2
            DO 260 J=1,K1
              FPH(J,K)= TEMP * (VTPFLX(J,K,LPHALF)-VTPFLX(J,K,LMHALF))
  260         CONTINUE
C         Convert to spectral space and add to THETA tendency
          CALL SPECTR( DPGQSP(0,0,JPOT,L), FPH )
C
C     All adiabatic tendency terms, excluding the geopotential in the
C     D tendency equation and the linearized heating term in the THETA
C     tendency equation, have now been computed for level L.
C
#if (NTRACE > 0)
C         Compute tracer time tendency at level L
C
C         Calculate "flux" of tracer
          DO 270 I=JX,JY
            DO 270 K=1,K2
              DO 270 J=1,K1
                FLXPH(J,K,I)= - TRC(J,K,LPZERO) * U(J,K,I,LPZERO)
  270           CONTINUE
C         Add horizontal convergence of tracer flux to the tendency
          CALL DIVERG(DPGQSP(0,0,JTR1,L), FLXPH(1,1,JX), FLXPH(1,1,JY))
C         Compute vertical tracer flux convergence
          TEMP= 1.0 / DP(L)
          DO 280 K=1,K2
            DO 280 J=1,K1
              FPH(J,K)= TEMP * (VTRCFX(J,K,LPHALF)-VTRCFX(J,K,LMHALF))
  280         CONTINUE
C         Convert to spectral space and add to THETA tendency
          CALL SPECTR( DPGQSP(0,0,JTR1,L), FPH )
#endif
C ----------------------------------------------------------------------
        ENDIF
C
  300   CONTINUE
C
C     Now we compute the remaining terms of the tendency equation,
C     including the damping terms.
C **Note: Values of prognostic quantities at model time TIM - DT1
C        (PGQSP0) are used in the following calculations. i.e. The
C        treatment is explicit in time.
C
      DO 500 L=1,L1
        DO 500 N=0,N1
C ----------------------------------------------------------------------
C         Add geopotential contribution to D tendency
          TEMP= A0QINV*N*(N+1)
          DO 410 L0=1,L1
            DO 410 M=0,MIN0(N,M1)
              DPGQSP(M,N,JDIV,L)= DPGQSP(M,N,JDIV,L)
     1              + TEMP * NNT2DT(L,L0) * PGQSP0(M,N,JPOT,L0)
  410         CONTINUE
C
C         Add contribution from reference static stability to THETA
C         tendency
          DO 420 L0=1,L1
            DO 420 M=0,MIN0(N,M1)
              DPGQSP(M,N,JPOT,L)= DPGQSP(M,N,JPOT,L)
     1             + D2TT(L,L0) * PGQSP0(M,N,JDIV,L0)
  420         CONTINUE
C
C         Add Del**8 damping terms to VOR/DIV/POT tendencies
          DO 430 M=0,MIN0(N,M1)
            DPGQSP(M,N,JVOR,L)= DPGQSP(M,N,JVOR,L)
     1            - UD8FAC(N) * PGQSP0(M,N,JVOR,L)
            DPGQSP(M,N,JDIV,L)= DPGQSP(M,N,JDIV,L)
     1            - UD8FAC(N) * PGQSP0(M,N,JDIV,L)
            DPGQSP(M,N,JPOT,L)= DPGQSP(M,N,JPOT,L)
     1            - TD8FAC(N) * PGQSP0(M,N,JPOT,L)
  430       CONTINUE
#if (NTRACE > 0)
C
C         Add Del**8 damping term to tracer tendency
          DO 440 M=0,MIN0(N,M1)
            DPGQSP(M,N,JTR1,L)= DPGQSP(M,N,JTR1,L)
     1            - TD8FAC(N) * PGQSP0(M,N,JTR1,L)
  440       CONTINUE
#endif
C ----------------------------------------------------------------------
  500     CONTINUE
C
      RETURN
      END
 
      SUBROUTINE DDTFRC( DPGQSP, PGQSP0, PGQSP1 )
C DDTFRC adds forcing/damping terms to the spectral time tendency DPGQSP
C (A call to DDTFRC, if any, should immediately follow the call to
C  DDTPGQ that computes DPGQSP)
C Input:  COMPLEX DPGQSP(0:M1MAX,0:N1MAX,NPGQ,L1MAX) --
C         "adiabatic" spectral time-tendency of prognostic quantities
C         COMPLEX PGQSP0(0:M1MAX,0:N1MAX,NPGQ,L1MAX) --
C          spectral representation of prognostic quantity at all
C          levels at (current model time) - DT1
C         COMPLEX PGQSP1(0:M1MAX,0:N1MAX,NPGQ,L1MAX) --
C          spectral representation of prognostic quantity at all
C          levels at current model time
C Output: COMPLEX DPGQSP(0:M1MAX,0:N1MAX,NPGQ,L1MAX) --
C          spectral time-tendency, with effects of forcing/damping added
C
#include "mgrid.h"
#include "tmarch.h"
      COMPLEX DPGQSP(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      COMPLEX PGQSP0(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      COMPLEX PGQSP1(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      REAL TEMP
      COMPLEX STRESS(0:M1MAX, 0:N1MAX, 0:1)
      INTEGER L, M, N, IPGQ
      INTEGER LMHALF, LPHALF
C     (Mnemonic for L-(1/2), L+(1/2))
C
C **Note: Values of prognostic quantities at current model time - DT1
C        (PGQSP0) are used in the following calculations. i.e. The
C        treatment is explicit in time.
C ----------------------------------------------------------------------
C
      DO 200 L=1,L1
        DO 200 N=0,N1
C         Forcing of zonal-mean potential temperature
          DPGQSP(0,N,JPOT,L)= DPGQSP(0,N,JPOT,L)
     1          + TRLXLV(L) * CMPLX(TMNLV(N,L), 0.0)
C         Include effects of Rayleigh friction & Newtonian cooling
          DO 110 M=0,MIN0(N,M1)
            DPGQSP(M,N,JVOR,L)= DPGQSP(M,N,JVOR,L)
     1            - URLXLV(L) * PGQSP0(M,N,JVOR,L)
            DPGQSP(M,N,JDIV,L)= DPGQSP(M,N,JDIV,L)
     1            - URLXLV(L) * PGQSP0(M,N,JDIV,L)
            DPGQSP(M,N,JPOT,L)= DPGQSP(M,N,JPOT,L)
     1            - TRLXLV(L) * PGQSP0(M,N,JPOT,L)
  110       CONTINUE
  200     CONTINUE
C
C ----------------------------------------------------------------------
C     Compute vertical viscous damping (for vorticity/divergence)
      DO 400 IPGQ=JVOR,JDIV
        LMHALF= 1
        LPHALF= 0
C       No stress at uppermost level
        CALL ZEROSP( STRESS(0,0,LPHALF), N1 )
        DO 400 L=1,L1
          LMHALF= MOD(LMHALF+1, 2)
          LPHALF= MOD(LMHALF+1, 2)
          IF (L.LT.L1) THEN
C           Compute stress at level L+1/2
            TEMP= VVISC(L) / (HDP(L+1) + HDP(L))
            DO 310 N=0,N1
              DO 310 M=0,MIN0(N,M1)
                STRESS(M,N,LPHALF)= TEMP *
     1                       (PGQSP0(M,N,IPGQ,L+1) - PGQSP0(M,N,IPGQ,L))
  310           CONTINUE
          ELSE
C           Compute stress at surface (level L1+1/2)
            DO 320 N=0,N1
              DO 320 M=0,MIN0(N,M1)
                STRESS(M,N,LPHALF)= -USDRAG * PGQSP0(M,N,IPGQ,L)
  320           CONTINUE
          ENDIF
C         Add contribution from viscous stress divergence
          TEMP= 1.0 / DP(L)
          DO 330 N=0,N1
            DO 330 M=0,MIN0(N,M1)
              DPGQSP(M,N,IPGQ,L)= DPGQSP(M,N,IPGQ,L) +
     1                 TEMP * (STRESS(M,N,LPHALF) - STRESS(M,N,LMHALF))
  330         CONTINUE
C
  400   CONTINUE
C ----------------------------------------------------------------------
      RETURN
      END
 
      SUBROUTINE DDTIMP( DPGQSP )
C DDTIMP makes implict corrections to the spectral time tendency DPGQSP.
C (DDTIMP should be called after the calls to DDTPGQ and DDTFRC to
C compute the "adiabatic" time-tendency and to include the effects of
C forcing/damping)
C Input:  COMPLEX DPGQSP(0:M1MAX,0:N1MAX,NPGQ,L1MAX) --
C          explicit spectral time-tendency of prognostic quantities
C Output: COMPLEX DPGQSP(0:M1MAX,0:N1MAX,NPGQ,L1MAX) --
C          implicit spectral time-tendency of prognostic quantities
C
#include "mgrid.h"
#include "tmarch.h"
      COMPLEX DPGQSP(0:M1MAX,0:N1MAX,NPGQ,L1MAX)
      INTEGER L0, L, M, N
      REAL RTEMP
      COMPLEX TEM(0:M1MAX,L1MAX)
      DO 200 N=0,N1
C
C       Making implicit corrections to D tendency
        DO 30 L=1,L1
C         Contribution from explicit D tendency, with Del**8 correction
          DO 10 M=0,MIN0(N,M1)
   10       TEM(M,L)= UD8COR(N) * DPGQSP(M,N,JDIV,L)
C         Add contribution from explicit THETA tendency
          RTEMP= UD8COR(N) * (A0QINV*N*(N+1)) * TD8COR(N) * DTFAC
          DO 20 L0=1,L1
            DO 20 M=0,MIN0(N,M1)
   20         TEM(M,L)=TEM(M,L) + RTEMP*NNT2DT(L,L0)*DPGQSP(M,N,JPOT,L0)
   30     CONTINUE
C
C       Multiply TEM by implicit correction matrix to find implicit D
C       tendency
        DO 60 L=1,L1
          DO 40 M=0,MIN0(N,M1)
   40       DPGQSP(M,N,JDIV,L)= (0., 0.)
          DO 50 L0=1,L1
            DO 50 M=0,MIN0(N,M1)
   50         DPGQSP(M,N,JDIV,L)=  DPGQSP(M,N,JDIV,L)
     1                           + IMPCOR(L,L0,N)*TEM(M,L0)
   60     CONTINUE
C
        DO 100 L=1,L1
C
C         Make implicit correction to THETA tendency due to
C         implicit D tendency
          DO 70 L0=1,L1
            DO 70 M=0,MIN0(N,M1)
              DPGQSP(M,N,JPOT,L)= DPGQSP(M,N,JPOT,L)
     1              + DTFAC * D2TT(L,L0) * DPGQSP(M,N,JDIV,L0)
   70         CONTINUE
C
C         Make implicit correction due to Del**8 damping of THETA
          DO 80 M=0,MIN0(N,M1)
   80       DPGQSP(M,N,JPOT,L)= TD8COR(N) * DPGQSP(M,N,JPOT,L)
#if (NTRACE > 0)
C
C         Make implicit correction due to Del**8 damping of tracer
          DO 90 M=0,MIN0(N,M1)
   90       DPGQSP(M,N,JTR1,L)= TD8COR(N) * DPGQSP(M,N,JTR1,L)
#endif
  100     CONTINUE
  200   CONTINUE
      RETURN
      END
 
      SUBROUTINE ROBFIL( FSP0, FSP1, FSP2, ROBFAC )
C Given the spectral states FSP0, FSP1, FSP2 at three successive
C time-steps, ROBFIL performs a Robert-filter operation on the
C states, with filter weight ROBFAC.
C Input:  COMPLEX FSP0(0:M1MAX,0:N1MAX) -- spectral
C         representation of prognostic quantity at model time TIM-DT
C         COMPLEX FSP1(0:M1MAX,0:N1MAX) -- spectral
C         representation of prognostic quantity at model time TIM
C         COMPLEX FSP2(0:M1MAX,0:N1MAX) -- spectral
C         representation of prognostic quantity at model time TIM+DT
C         REAL ROBFAC -- filter weighting factor
C Output: (Filtered versions of FSP0, FSP1, FSP2)
C
#include "spcons.h"
#include "spgrid.h"
      COMPLEX FSP0(0:M1MAX,0:N1MAX)
      COMPLEX FSP1(0:M1MAX,0:N1MAX), FSP2(0:M1MAX,0:N1MAX)
      REAL ROBFAC
      INTEGER M, N
      REAL TEMP
      TEMP= 1.0 - 2.0 * ROBFAC
      DO 10 N=0,N1
        DO 10 M=0,MIN0(N,M1)
          FSP1(M,N)=  TEMP  *  FSP1(M,N)
     1              + ROBFAC * (FSP0(M,N) + FSP2(M,N))
   10     CONTINUE
      RETURN
      END
 
