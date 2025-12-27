       SUBROUTINE INTERCON(IK,RK,BITK,XPT,NPT,X,NX,NKX,EXTRAP)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INTERCON    GET INTERPOLATION CONSTANTS
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 88-08-08
C
C ABSTRACT: DO ONE ITERATION OF INITIALIZATION.
C ABSTRACT: OBTAIN INTERPOLATION CONSTANTS FOR POINTS XPT ON
C   INTERVAL OF NX POINTS WHICH ARE MONOTONIC.
C
C PROGRAM HISTORY LOG:
C   88-08-08  PARRISH
C
C USAGE:    CALL INTERCON(IK,RK,BITK,XPT,NPT,X,NX,NKX,EXTRAP)
C   INPUT ARGUMENT LIST:
C     BITK     - =1--INTERPOLATEE.
C              - =0--EXTRAPOLATEE.
C     XPT      - COORDINATES OF INTERPOLATEE POINTS.
C     NPT      - NUMBER OF INTERPOLATEE POINTS.
C     X        - COORDINATES OF INTERPOLATOR POINTS (MONOTONIC)
C     NX       - NUMBER OF INTERPOLATOR POINTS.
C     NKX      - ORDER OF INTERPOLATION
C              -    =1--ASSIGN VALUE OF NEAREST POINT
C              -    =2--2-POINT (LINEAR)
C              -    =3--3-POINT (QUADRATIC)
C              -    =4--4-POINT (CUBIC)
C     EXTRAP   - FRACTION OF FIRST AND LAST INTERVAL WE ARE ALLOWED TO
C              - EXTRAPOLATE TO.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     IK       - GATHER INDEX FOR INTERPOLATORS
C     RK       - INTERPOLATOR WEIGHTS
C     BITK     - =1--INTERPOLATEE
C              - =0--EXTRAPOLATEE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN200
C   MACHINE:  CYBER
C
C$$$
C--------
         include "myparam"
         REAL RK(NPT,NKX),XPT(NPT),X(NX)
         INTEGER IK(NPT)
         LOGICAL BITK(NPT)
C--------
         IF(NX.EQ.1) THEN
           DO 10002 I=1,NPT
             IK(I)=0
             RK(I,1)=1.
10002      CONTINUE
           RETURN
         END IF
         MKX=MIN(NX,NKX,4)
C--------
C-------- DETERMINE ORDERING OF INTERPOLATOR POINTS (THEY MUST BE
C-------- EITHER MONOTONIC INCREASING OR DECREASING)
C--------
         DIRX=X(2)-X(1)
C        IF(DIRX.EQ.0.) STOP 'DIRX=0. IN INTERCON'
         IF(DIRX.EQ.0.) WRITE(6,99471)
99471    FORMAT(' DIRX = 0 IN INTERCON')
         DELX1=EXTRAP*ABS(DIRX)
         DELXN=EXTRAP*ABS(X(NX)-X(NX-1))
C        XPTMAX=Q8SMAX(XPT(1;NPT))
C        XPTMIN=Q8SMIN(XPT(1;NPT))
C        XMAX=Q8SMAX(X(1;NX))
C        XMIN=Q8SMIN(X(1;NX))
C        WRITE(6,21)XPTMAX,XPTMIN,XMAX,XMIN,DELX1,DELXN,EXTRAP,DIRX
C21      FORMAT(' XPX,XPMN,XX,XMN,DLX1,DLXN,EXTP,DIRX=',8E11.3)
C--------
C-------- FLAG EXTRAPOLATEES
C--------
         IF(DIRX.GT.0.) THEN
           DO 10004 I=1,NPT
             BITK(I)=BITK(I).AND.
     *                       (XPT(I).GT.X(1)-DELX1).AND.
     *                       (XPT(I).LT.X(NX)+DELXN)
10004      CONTINUE
         END IF
         IF(DIRX.LT.0.) THEN
           DO 10006 I=1,NPT
             BITK(I)=BITK(I).AND.
     *                       (XPT(I).LT.X(1)+DELX1).AND.
     *                       (XPT(I).GT.X(NX)-DELXN)
10006      CONTINUE
         END IF
C--------
C-------- DETERMINE GATHER INDEX.
C--------
C        IF(DIRX.GT.0.) IK(1;NPT)=Q8VLTI(XPT(1;NPT),X(1;NX);IK(1;NPT))
         DO 10008 I=1,NPT
           IK(I)=0
10008    CONTINUE
         IF(DIRX.GT.0.) THEN
c          DO 10012 K=1,NX-1
c            DO 10010 I=1,NPT
c              IF(X(K).LE.XPT(I).AND.XPT(I).LT.X(K+1)) IK(I)=K
c              if(xpt(i).le.x(1)) ik(i)=1
c              if(xpt(i).ge.x(nx)) ik(i)=nx
c0010        CONTINUE
c0012      CONTINUE
           do 10012 i=1,npt
             if(xpt(i).le.x(1)) then
               ik(i)=1
             else
               ik(i)=isrchfge(nx-1,x,1,xpt(i))-1
             end if
10012      continue
         END IF
C        IF(DIRX.LT.0.) IK(1;NPT)=Q8VGEI(XPT(1;NPT),X(1;NX);IK(1;NPT))
         IF(DIRX.LT.0.) THEN
c          DO 19012 K=1,NX-1
c            DO 19010 I=1,NPT
c              IF(X(K).GE.XPT(I).AND.XPT(I).GT.X(K+1)) IK(I)=K
c              if(xpt(i).ge.x(1)) ik(i)=1
c              if(xpt(i).le.x(nx)) ik(i)=nx
c9010        CONTINUE
c9012      CONTINUE
           do 19012 i=1,npt
             if(xpt(i).ge.x(1)) then
               ik(i)=1
             else
               ik(i)=isrchfle(nx-1,x,1,xpt(i))-1
             end if
19012      continue
         END IF
         DO 10014 I=1,NPT
           IK(I)=MAX(1,IK(I))
10014    CONTINUE
C--------
C-------- NOW DETERMINE INTERPOLATION WEIGHTS
C--------
         GO TO (100,200,300,400),MKX
100      CONTINUE
C----------
C---------- 0 ORDER INTERPOLATION
C----------
C---------- FIND NEAREST POINT, AND ADJUST IK ACCORDINGLY
C----------
           DO 198 I=1,NPT
             IK(I)=MIN(IK(I),NX-1)
             W1D=X(IK(I))
             W2D=X(IK(I)+1)
             W1D=ABS(W1D-XPT(I))
             W2D=ABS(W2D-XPT(I))
             IF(W2D.LT.W1D) IK(I)=IK(I)+1
             RK(I,1)=1.
198        CONTINUE
           GO TO 500
200      CONTINUE
C----------
C---------- 1ST ORDER (LINEAR) INTERPOLATION
C----------
           DO 298 I=1,NPT
             IK(I)=MIN(IK(I),NX-1)
             W1D=X(IK(I))
             W2D=X(IK(I)+1)
             W12ID=1./(W1D-W2D)
             RK(I,1)=(XPT(I)-W2D)*W12ID
             RK(I,2)=(W1D-XPT(I))*W12ID
298        CONTINUE
           GO TO 500
300      CONTINUE
C----------
C---------- 2ND ORDER (QUADRATIC) INTERPOLATION
C----------
C---------- COMPUTE WEIGHTS RK, AND ADJUST IK ACCORDINGLY
C----------
           DO 398 I=1,NPT
             IK(I)=MIN(IK(I),NX-1)
             W1D=X(IK(I))
             W2D=X(IK(I)+1)
             W1D=ABS(W1D-XPT(I))
             W2D=ABS(W2D-XPT(I))
             IF(W2D.LT.W1D) IK(I)=IK(I)+1
             IK(I)=IK(I)-1
             IK(I)=MAX(1,MIN(IK(I),NX-2))
             W1D=X(IK(I))
             W2D=X(IK(I)+1)
             W3D=X(IK(I)+2)
             W12ID=1./(W1D-W2D)
             W13ID=1./(W1D-W3D)
             W23ID=1./(W2D-W3D)
             RK(I,1)=(XPT(I)-W2D)*(XPT(I)-W3D)*W12ID*W13ID
             RK(I,2)=(W1D-XPT(I))*(XPT(I)-W3D)*W12ID*W23ID
             RK(I,3)=(W1D-XPT(I))*(W2D-XPT(I))*W13ID*W23ID
398        CONTINUE
           GO TO 500
400      CONTINUE
C----------
C---------- 3RD ORDER (CUBIC) INTERPOLATION
C----------
C---------- IF MCUBIC = 1, THEN
C---------- USE CUBIC FITTED TO NEAREST 4 POINTS
C----------
C---------- IF MCUBIC = 2, THEN
C---------- USE QUASI-HERMITE CUBIC SPLINE IN INTERIOR
C---------- USE QUADRATIC IN END INTERVALS
C----------
C          IK(1;NPT)=IK(1;NPT)-2
           MCUBIC = IMCUBIC
           DO 498 I=1,NPT
             IK(I)=IK(I)-1
             IF (MCUBIC.EQ.2) THEN
               IEND=0
               IF(IK(I).LT.1) IEND=-1
               IF(IK(I).GT.NX-3) IEND=1
             END IF
             IK(I)=MAX(1,MIN(IK(I),NX-3))
             W1D=X(IK(I))
             W2D=X(IK(I)+1)
             W3D=X(IK(I)+2)
             W4D=X(IK(I)+3)
             W12ID=1./(W1D-W2D)
             W13ID=1./(W1D-W3D)
             W14ID=1./(W1D-W4D)
             W23ID=1./(W2D-W3D)
             W24ID=1./(W2D-W4D)
             W34ID=1./(W3D-W4D)
             IF (MCUBIC.EQ.2) THEN
               WM21=(W2D-W3D)*W12ID*W13ID
               WM22=(W1D+W3D-2.*W2D)*W12ID*W23ID
               WM23=(W2D-W1D)*W13ID*W23ID
               WM24=0.
               WM31=0.
               WM32=(W3D-W4D)*W23ID*W24ID
               WM33=(W2D+W4D-2.*W3D)*W23ID*W34ID
               WM34=(W3D-W2D)*W24ID*W34ID
               IF(IEND.EQ.-1) THEN
                 WM31=(W3D-W2D)*W12ID*W13ID
                 WM32=(W1D-W3D)*W12ID*W23ID
                 WM33=(2.*W3D-W1D-W2D)*W13ID*W23ID
                 WM34=0.
               END IF
               IF(IEND.EQ.1) THEN
                 WM21=0.
                 WM22=(2.*W2D-W3D-W4D)*W23ID*W24ID
                 WM23=(W4D-W2D)*W23ID*W34ID
                 WM24=(W2D-W3D)*W24ID*W34ID
               END IF
               WM2=(XPT(I)-W2D)*(((XPT(I)-W3D)*W23ID)**2)
               WM3=(XPT(I)-W3D)*(((XPT(I)-W2D)*W23ID)**2)
               RK(I,1)=WM2*WM21+WM3*WM31
               RK(I,2)=WM2*WM22+WM3*WM32-
     *               ((XPT(I)-W3D)**2)*
     *               (2.*XPT(I)-3.*W2D+W3D)*W23ID**3
               RK(I,3)=WM2*WM23+WM3*WM33+
     *               ((XPT(I)-W2D)**2)*
     *               (2.*XPT(I)-3.*W3D+W2D)*W23ID**3
               RK(I,4)=WM2*WM24+WM3*WM34
             ELSE
               RK(I,1)=(XPT(I)-W2D)*(XPT(I)-W3D)
     *               *(XPT(I)-W4D)*W12ID*W13ID*W14ID
               RK(I,2)=(W1D-XPT(I))*(XPT(I)-W3D)
     *               *(XPT(I)-W4D)*W12ID*W23ID*W24ID
               RK(I,3)=(W1D-XPT(I))*(W2D-XPT(I))
     *               *(XPT(I)-W4D)*W13ID*W23ID*W34ID
               RK(I,4)=(W1D-XPT(I))*(W2D-XPT(I))
     *               *(W3D-XPT(I))*W14ID*W24ID*W34ID
             END IF
498        CONTINUE
500      CONTINUE
C--------
C-------- FINALLY SUBTRACT ONE FROM IK
C--------
         DO 60002 I=1,NPT
           IK(I)=IK(I)-1
60002    CONTINUE
       RETURN
       END
