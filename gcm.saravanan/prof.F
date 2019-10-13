C This module computes reference vertical profiles of temperature.
C (See BLOCK DATA subprogram TCONSB for reference)
C
      REAL FUNCTION FSEASN( MU, ISEASN, T00, TS30, TW30, TS60, TW60 )
C Given the values of a seasonally varying function f(mu) in the
C tropics (T00), subtropical summer/winter (TS30, TW30) and subarctic
C summer/winter (TS60/TW60), FSEASN returns the value at mu=MU for
C the season denoted by ISEASN.
C (ISEASN=1/2/3, 1=>annual mean, 2=>N.H.Winter, 3=>N.H.Summer)
C
      REAL MU, T00, TS30, TW30, TS60, TW60
      INTEGER ISEASN
      REAL FYPROF
C
      GOTO (10, 20, 30), ISEASN
C     Annual mean [ = (summer + winter)/2 ]
   10 FSEASN= FYPROF( MU, 0.5*(TS60+TW60), 0.5*(TS30+TW30), T00,
     1                    0.5*(TS30+TW30), 0.5*(TS60+TW60)      )
      RETURN
C     Northern Hemisphere Winter
   20 FSEASN= FYPROF( MU, TS60, TS30, T00, TW30, TW60 )
      RETURN
C     Northern Hemispere Summer
   30 FSEASN= FYPROF( MU, TW60, TW30, T00, TS30, TS60 )
      RETURN
      END
 
      REAL FUNCTION FYPROF( MU, F60S, F30S, F0, F30N, F60N )
C Given the values of a function f(mu) at the latitudes 60S, 30S, 0,
C 30N, and 60N, FYPROF returns the interpolated value of f(mu) at
C mu=MU. (mu = sin(latitude))
C {The interpolation is carried out using Lagrange's formula for a
C fourth-order polynomial in mu. This formula preserves the value of
C the function at 30S, 0, and 30N latitudes, but does not preserve
C the values at 60S and 60N. The value at 60N in conjunction with
C the value at 30N is actually used to linearly extrapolate a value
C at 90N. It is this value that is preserved by the Lagrange formula.
C (Similarly for the value at 60S)
C
C
      REAL MU, F60S, F30S, F0, F30N, F60N
      REAL F90S, F90N
      F90S= F30S - (F30S - F60S)/0.732
      F90N= F30N + (F60N - F30N)/0.732
      FYPROF= 4.0*(MU**2 - 0.25)*(MU**2 - 1.0)*F0  +  (2./3.)*MU*(
     1           (MU**2 - 0.25)*( F90S*(MU-1.0) + F90N*(MU+1.0) )
     2     - 4.0*(MU**2 - 1.0) *( F30S*(MU-0.5) + F30N*(MU+0.5) ))
      RETURN
      END
 
      SUBROUTINE TPPROF( T, Z, P, NP, IPROF, R, G)
C Given an array of pressure values P (in ascending order), TPPROF
C computes the temperatures T and heights Z at these pressure levels,
C using standard temperature profile IPROF and the hydrostatic relation.
C
C **Note** P(NP) is assumed to correspond to Z=0 (surface).
C
C Input:  INTEGER NP -- no. of pressure values
C         REAL P(NP) -- pressure values (ascending order, in SI units)
C         INTEGER IPROF -- standard temperature profile number
C                          (see function TZPROF for its meaning)
C         REAL R -- Gas constant (in J/(kgK))
C         REAL G -- Gravitational acceleration (in m/s**2)
C Output: REAL T(NP) -- Temperature values (in K) at pressure levels
C         REAL Z(NP) -- Height values (in m) at pressure levels
C
      INTEGER NP, IPROF
      REAL T(NP), Z(NP), P(NP), R, G
C
      INTEGER I0
      REAL R0, G0
      COMMON/TCONS2/ I0, R0, G0
C
      INTEGER NMAX
      PARAMETER (NMAX=10)
C     (No. of sub-intervals for Runge-Kutta integration)
      REAL Z0, P0, DP
      INTEGER IP, I
      REAL TZPROF
      EXTERNAL HYDRO
C     Initialize common block /TCONS2/
      I0= IPROF
      R0= R
      G0= G
C     Initialize hydrostatic integration at surface (P(NP))
      Z0= 0.0
      Z(NP)= Z0
      T(NP)= TZPROF( Z(NP)/1000.0, IPROF )
      DO 20 IP=NP-1,1,-1
C       (negative pressure increment)
        DP= (P(IP) - P(IP+1)) / NMAX
        P0= P(IP+1)
C       Integrate hydrostatic relation to obtain height
        DO 10 I=1,NMAX
   10     CALL RKUTTA( Z0, P0, DP, Z0, P0, HYDRO )
C       Use height of pressure level to determine temperature
        Z(IP)= Z0
        T(IP)= TZPROF( Z(IP)/1000.0, IPROF )
   20   CONTINUE
      RETURN
      END
 
      SUBROUTINE RKUTTA(Y,X,H,YOUT,XOUT,DERIV)
C Given the solution Y at X to a first order differential equation,
C use the fourth-order Runge-Kutta method to advance the solution
C over an interval H and return the incremented solution YOUT at
C XOUT(=X+H). (YOUT,XOUT may be the same variables as Y,X). The user
C supplies the subroutine DERIV which returns the derivative DYDX at X.
C (Adapted from Numerical Recipes, W.H.Press et al, p.553)
C
      REAL Y, X, H, YOUT, XOUT
      REAL HH, H6, XH, DY, YT, DYT, DYM
#ifdef UDEF
      INTEGER DERIV
#endif
      HH= H*0.5
      H6= H/6.
      XH= X + HH
C     First step
      CALL DERIV(X, Y, DY)
      YT= Y + HH*DY
C     Second step
      CALL DERIV(XH, YT, DYT)
      YT= Y + HH*DYT
C     Third step
      CALL DERIV(XH, YT, DYM)
      YT= Y + H*DYM
      DYM= DYT + DYM
C     Fourth step
      CALL DERIV(X+H, YT, DYT)
      YOUT= Y + H6*( DY + DYT + 2.*DYM )
      XOUT= X + H
      RETURN
      END
 
      SUBROUTINE HYDRO(P, Z, DZ)
C HYDRO computes dZ/dP, given the pressure P and height Z.
C (Common block /TCONS2/ contains the relevant parameters)
C (All quantities in S.I. units)
      REAL P, Z, DZ
      INTEGER I0
      REAL R0, G0
      COMMON/TCONS2/ I0, R0, G0
C     I0= standard temperature profile number (see TZPROF)
C     R0=Gas constant (J/(kgK)), G0=Gravitational acceleration (m/s**2)
C
      REAL TZPROF
C
      DZ= -R0 * TZPROF(Z/1000., I0) / (G0*P)
      RETURN
      END
 
C
      REAL FUNCTION TZPROF( Z, N )
C TZPROF returns the temperature value at height Z km, for standard
C atmospheric profile N (=JT00, ..., JTUSS).
C **NOTE** Z should be specified in kilometres.
C (See block data subprogram TCONSB for more information)
      INTEGER JT00, JTS30, JTW30, JTS60, JTW60, JTUSS
      PARAMETER (JT00=1, JTS30=2, JTW30=3, JTS60=4, JTW60=5, JTUSS=6)
      REAL T0, Z0, C0, D0
      COMMON/TCONS/ T0(6), Z0(10,6), C0(11,6), D0(10,6)
      REAL Z
      INTEGER N
      INTEGER I
      REAL TEMP
      TEMP= T0(N) + 0.5 * C0(1,N) * Z
      DO 10 I=1,10
        TEMP= TEMP + 0.5 * D0(I,N) * (C0(I+1,N) - C0(I,N)) *
     1      DLOG(  DCOSH( DBLE((Z-Z0(I,N))/D0(I,N)) ) /
     2             DCOSH( DBLE(Z0(I,N)/D0(I,N))     )       )
   10   CONTINUE
      TZPROF= TEMP
      RETURN
      END
 
      BLOCK DATA TCONSB
C Initializes common block TCONS containing the coefficients T0, z(i),
C C(i), and delta(i) for analytic representation of various standard
C atmospheric temperature profiles.
C
C Fels, S.B., 1986: Analytic representation of standard atmosphere
C     temperature profiles, Journal of Atmospheric Sciences,
C     Vol. 43, 219-221.
C
C The outer array index values 1 ... 6 correspond to
C 1 : Tropical             (JT00 )
C 2 : Subtropical summer   (JTS30)
C 3 : Subtropical winter   (JTW30)
C 4 : Subarctic   summer   (JTS60)
C 5 : Subarctic   winter   (JTW60)
C 6 : U.S.Standard(1976)   (JTUSS)
C
      INTEGER JT00, JTS30, JTW30, JTS60, JTW60, JTUSS
      PARAMETER (JT00=1, JTS30=2, JTW30=3, JTS60=4, JTW60=5, JTUSS=6)
      REAL T0, Z0, C0, D0
      COMMON/TCONS/ T0(6), Z0(10,6), C0(11,6), D0(10,6)
C
      DATA T0/300.0, 294.0, 272.2, 287.0, 257.1, 288.15/
C
      DATA Z0/
     1  2.0,  3.0, 16.5, 21.5, 45.0, 51.0, 70.0, 100.0, 200.0, 300.0,
     2  1.5,  6.5, 13.0, 18.0, 26.0, 36.0, 48.0,  50.0,  70.0, 100.0,
     3  3.0, 10.0, 19.0, 25.0, 32.0, 44.5, 50.0,  71.0,  98.0, 200.0,
     4  4.7, 10.0, 23.0, 31.8, 44.0, 50.2, 69.2, 100.0, 200.0, 300.0,
     5  1.0,  3.2,  8.5, 15.5, 25.0, 30.0, 35.0,  50.0,  70.0, 100.0,
     6 11.0, 20.1, 32.1, 47.4, 51.4, 71.7, 85.7, 100.0, 200.0, 300.0/
C
      DATA C0/
     1 -6.0, -4.0, -6.7,  4.0,  2.2,  1.0, -2.8, -0.27,   0.0,   0.0,0.,
     2 -4.0, -6.0, -6.5,  0.0,  1.2,  2.2,  2.5,   0.0,  -3.0, -0.25,0.,
     3 -3.5, -6.0, -0.5,  0.0,  0.4,  3.2,  1.6,  -1.8,  -0.7,   0.0,0.,
     4 -5.3, -7.0,  0.0,  1.4,  3.0,  0.7, -3.3,  -0.2,   0.0,   0.0,0.,
     5  3.0, -3.2, -6.8,  0.0, -0.6,  1.0,  1.2,   2.5,  -0.7,  -1.2,0.,
     6 -6.5,  0.0,  1.0, 2.75,  0.0,-2.75,-1.97,   0.0,   0.0,   0.0,0./
C
      DATA D0/
     1  0.5,  0.5,  0.3,  0.5,  1.0,  1.0,  1.0,   1.0,   1.0,   1.0,
     2  0.5,  1.0,  0.5,  0.5,  1.0,  1.0,  2.5,   0.5,   1.0,   1.0,
     3  0.5,  0.5,  1.0,  1.0,  1.0,  1.0,  1.0,   1.0,   1.0,   1.0,
     4  0.5,  0.3,  1.0,  1.0,  2.0,  1.0,  1.5,   1.0,   1.0,   1.0,
     5  0.4,  1.5,  0.3,  0.5,  1.0,  1.0,  1.0,   1.0,   1.0,   1.0,
     6  0.3,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,   1.0,   1.0,   1.0/
C
      END
