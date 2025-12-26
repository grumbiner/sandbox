       SUBROUTINE TEMPERTN(UIO,VIO,PIO,IMG,JMG,XPOLE,YPOLE,DXA2,LOOP,
     *                     NUMVERT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    TEMPERTN    GET INCREMENT FOR ONE ITERATION OF INI
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 88-08-08
C
C ABSTRACT: COMPUTE INCREMENT FOR ONE ITERATION OF TEMPERTON
C   INITIALIZATION SCHEME.
C
C PROGRAM HISTORY LOG:
C   88-08-08  PARRISH
C
C USAGE:    CALL TEMPERTN(UIO,VIO,PIO,IMG,JMG,XPOLE,YPOLE,DXA2,LOOP)
C-I/O----  UIO,VIO,PIO: U,V,P TENDENCIES ON INPUT, INCREMENTS ON OUTPUT
C-INPUT--  IMG,JMG:     GRID DIMENSIONS FOR EACH GRID.
C-INPUT--  XPOLE,YPOLE: POLE COORDINATES OF EACH GRID.
C-INPUT--  DXA2:        TWICE THE A-GRID GRID INCREMENT (METERS)
C-INPUT--  LOOP:        NON-LINEAR ITERATION NUMBER.
C   INPUT ARGUMENT LIST:
C     UIO      - U-TENDENCIES ON NGM A-B-C GRIDS, DECOMPOSED IN VERT.
C     VIO      - V-TENDENCIES
C     PIO      - MASS-VARIABLE TENDENCIES
C     IMG      - X-DIRECTION GRID DIMENSIONS
C     JMG      - Y-DIRECTION GRID DIMENSIONS
C     XPOLE    - X-COORDINATES OF POLE ON EACH GRID
C     YPOLE    - Y-COORDINATES OF POLE ON EACH GRID
C     DXA2     - TWICE THE A-GRID INCREMENT (METERS)
C     LOOP     - NON-LINEAR ITERATION NUMBER
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     UIO      - U-INCREMENTS ON NGM A-B-C GRIDS, DECOMPOSED IN VERT.
C     VIO      - V-INCREMENTS
C     PIO      - MASS-VARIABLE INCREMENTS
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       COMMON   - LIST COMMON LIBRARY ROUTINES, E.G., CONSOL
C       W3LIB    -
C       W4LIB    - DELETE THE CORRESPONDING LINE OR LINES
C       GRAPHICS - IF LIBRARY IS UNNEEDED
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN200
C   MACHINE:  CYBER
C
C$$$
C--------
         include "myparam"
         REAL UIO(IIJMAX,INUMVERT,INGRDUSE)
         REAL VIO(IIJMAX,INUMVERT,INGRDUSE)
         REAL PIO(IIJMAX,INUMVERT,INGRDUSE)
         double precision RLATH(INLATDP),RLONH(INLONEX)
         real rlathxy(inlatdp)
         double precision wgts(inlatdp)
         REAL XPOLE(1),YPOLE(1)
         REAL DX(INGRDUSE)
         REAL RKPO(IIJMAX,16)
         REAL RKLO(INLATDP,INLONEX,16)
         REAL RKPOWORK(IIJMAX,4)
         REAL RKLOWORK(INLATDP,INLONEX,4)
         REAL XPOLA(IIJMAX),YPOLA(IIJMAX)
         REAL XLOLA(INLONEX,INLATDP)
         REAL YLOLA(INLONEX,INLATDP)
         REAL WPOLA1(IIJMAX),WPOLA2(IIJMAX)
         REAL WLOLA1(INLATDP,INLONEX)
         REAL WLOLA2(INLATDP,INLONEX)
         REAL WLOLA12(INLATDP,INLONEX,2)
         EQUIVALENCE (WLOLA1(1,1),WLOLA12(1,1,1))
         EQUIVALENCE (WLOLA2(1,1),WLOLA12(1,1,2))
         REAL WLOLA34(INLATDP,INLONEX,2)
         REAL ULALO(INLATDP,INLONEX)
         REAL VLALO(INLATDP,INLONEX)
         REAL HLALO(INLATDP,INLONEX)
         REAL UPOLA(IIJMAX),VPOLA(IIJMAX),HPOLA(IIJMAX)
         REAL ZHAT(incoefs+1),ZHAT2(incoefs+1)
         REAL ZHAT3(incoefs+1),ZHAT4(incoefs+1)
         REAL DHAT(incoefs+1),DHAT2(incoefs+1)
         REAL DHAT3(incoefs+1),DHAT4(incoefs+1)
         REAL HHAT(incoefs+1),HHAT2(incoefs+1)
         REAL HHAT3(incoefs+1),HHAT4(incoefs+1)
         double precision ap(0:ijcapdp,0:ijcapdp)
         double precision bp(0:ijcapdp,0:ijcapdp)
         double precision aqr(0:ijcapdp,0:ijcapdp)
         double precision bqr(0:ijcapdp,0:ijcapdp)
         double precision gr(0:ijcapdp,0:ijcapdp)
         double precision slat(inlath),clat(inlath)
         double precision pe0(inlath,0:ijcapdp)
         double precision qe0(inlath,0:ijcapdp)
         double precision ro0(inlath,0:ijcapdp)
         real trigs(2*inlonex)
C--------
         INTEGER IMG(1),JMG(1)
         INTEGER IKPO(IIJMAX)
         INTEGER IKPOWORK(IIJMAX)
         INTEGER IKLO(INLATDP,INLONEX)
         INTEGER IKLOWORK(INLATDP,INLONEX),ifax(10)
         integer irs(0:ijcapdp,0:ijcapdp),iis(0:ijcapdp,0:ijcapdp)
         LOGICAL BKPO(IIJMAX)
         LOGICAL BKLO(INLATDP,INLONEX)
C--------
         ERAD=6371.22E3
         OMEGA=7.292116E-5
         OMEGAINV=1./OMEGA
         G=9.8
         GINV=1./G
         NGRDUSE=INGRDUSE
         NLON=INLON
         NLONEX=INLONEX
         NLAT=INLATDP
         NLATH=INLATH
         JCAP=IJCAPDP
         NCOEFS=INCOEFS
         NLOLA=NLAT*NLONEX
         DXA=.5*DXA2
         DLON=8.*ATAN(1.)/NLON
         DO 10 NG=1,NGRDUSE
           DX(NG)=DXA
           DXA=.5*DXA
10       CONTINUE
C--------
C-------- PICK UP GAUSSIAN LATITUDES FROM SPECTRAL PACKAGE
         CALL GETLALO(RLATH,RLONH,wgts,jcap,nlath,nlon,nlonex,
     *      irs,iis,ap,bp,aqr,bqr,gr,slat,clat,pe0,qe0,ro0,trigs,ifax)
         rlathxy=rlath
         rlathxy(1)=.5*(rlath(1)+rlath(2))
C--------
         DO 9000 K=1,NUMVERT
C----------
C---------- 1.  INTERPOLATE FROM POLA GRIDS TO LOLA GRID
C----------
           IORDPL = IIORDPL
           IORBPL = IIORBPL
           DO 110 NG=1,NGRDUSE
            IORB = IORDPL
            IF (NG.EQ.1) IORB = IORBPL
            DO 110 IORD=IORB,IORDPL
             CALL HLPO2LOL(UIO(1,K,NG),IMG(NG),JMG(NG),DX(NG),
     *              XPOLE(NG),YPOLE(NG)-.5,
     *              ULALO,NLONEX,NLAT,
     *                     RKLO,IKLO,BKLO,WLOLA1,
     *         XPOLA,YPOLA,XLOLA,YLOLA,RKLOWORK,IKLOWORK,RLATHxy,DLON,
     *         IORD)
             CALL HLPO2LOL(VIO(1,K,NG),IMG(NG),JMG(NG),DX(NG),
     *              XPOLE(NG)-.5,YPOLE(NG),
     *              VLALO,NLONEX,NLAT,
     *                     RKLO,IKLO,BKLO,WLOLA1,
     *         XPOLA,YPOLA,XLOLA,YLOLA,RKLOWORK,IKLOWORK,RLATHxy,DLON,
     *         IORD)
             CALL HLPO2LOL(PIO(1,K,NG),IMG(NG),JMG(NG),DX(NG),
     *              XPOLE(NG),YPOLE(NG),
     *              HLALO,NLONEX,NLAT,
     *                     RKLO,IKLO,BKLO,WLOLA1,
     *      XPOLA,YPOLA,XLOLA,YLOLA,RKLOWORK,IKLOWORK,RLATHxy,DLON,
     *         IORD)
110        CONTINUE
C----------
C---------- 2.  ROTATE WINDS
C----------
           DO 220 I=1,NLONEX
             ANGLE=(I-1.)*DLON
             COSLON=COS(ANGLE)
             SINLON=SIN(ANGLE)
             DO 218 J=1,NLAT
               WLOLA1(J,1)=-ULALO(J,I)*SINLON
     *                        +VLALO(J,I)*COSLON
               WLOLA2(J,1)=-ULALO(J,I)*COSLON
     *                        -VLALO(J,I)*SINLON
               ULALO(J,I)=WLOLA1(J,1)
               VLALO(J,I)=WLOLA2(J,1)
218          CONTINUE
220        CONTINUE
C----------
C---------- 3.  REFLECT ABOUT EQUATOR (U,H SYM, V ASYM)
C----------
           DO 320 I=1,NLONEX
             DO 310 J=NLATH+1,NLAT
               JS=NLAT+2-J
               HLALO(JS,I)=HLALO(J,I)
               ULALO(JS,I)=ULALO(J,I)
               VLALO(JS,I)=-VLALO(J,I)
310          CONTINUE
320        CONTINUE
C----------     DIVIDE BY G TO GET HEIGHT TENDENCY
           DO 322 I=1,NLOLA
             HLALO(I,1)=GINV*HLALO(I,1)
322        CONTINUE
C----------
C---------- 4.  TRANSFORM FROM GRID TO SPECTRAL, AND SCALE
C----------
           CALL GETDEPTH(DEPTH,K)
           call g2s1(zhat2,dhat2,hhat2,zhat,dhat,hhat,
     *       ulalo,vlalo,hlalo,rklowork,rklowork(1,1,2),rklowork(1,1,3),
     *      1,jcap,nlon,nlath,wgts,irs,iis,ap,bp,aqr,bqr,gr,slat,clat,
     *         pe0,qe0,ro0,trigs,ifax)
c          CALL SGRD2COF(HLALO,HHAT2,ITRANS,JCAP,NLATH,NLON)
c         CALL VGRD2COF(ULALO,VLALO,DHAT2,ZHAT2,ITRANS,JCAP,NLATH,NLON)
           CALL SCALCOFS(ZHAT2,DHAT2,HHAT2,ZHAT,DHAT,HHAT,DEPTH,JCAP)
C----------
C---------- 5.  EXTRACT FAST MODE TENDENCIES (ONLY FOR DIAGNOSTICS)
C----------
           CALL SANDF(ZHAT,DHAT,HHAT,ZHAT2,DHAT2,HHAT2,DEPTH,JCAP)
C----------  SLOW MODES FIRST
           DO 502 I=1,NCOEFS
             ZHAT3(I)=ZHAT2(I)
             DHAT3(I)=0.
             HHAT3(I)=0.
502        CONTINUE
           TRANS=1.
           CALL SPLUSF(ZHAT3,DHAT3,HHAT3,ZHAT4,DHAT4,HHAT4,DEPTH,TRANS,
     *                     JCAP)
           ISLOW=1
           CALL BALDIAG(ZHAT4,DHAT4,HHAT4,K,LOOP,ISLOW,JCAP)
C----------  NOW FAST MODES
           DO 504 I=1,NCOEFS
             ZHAT3(I)=0.
             DHAT3(I)=DHAT2(I)
             HHAT3(I)=HHAT2(I)
504        CONTINUE
           CALL SPLUSF(ZHAT3,DHAT3,HHAT3,ZHAT4,DHAT4,HHAT4,DEPTH,TRANS,
     *                     JCAP)
           ISLOW=2
           CALL BALDIAG(ZHAT4,DHAT4,HHAT4,K,LOOP,ISLOW,JCAP)
C----------
C---------- OBTAIN BALANCE INCREMENT
C----------
           CALL BALTEND(ZHAT,DHAT,HHAT,ZHAT2,DHAT2,HHAT2,JCAP,DEPTH)
           CALL USCLCOFS(ZHAT2,DHAT2,HHAT2,ZHAT,DHAT,HHAT,DEPTH,JCAP)
C----------
C---------- 8.  TRANSFORM FROM SPECTRAL TO GRID
C----------
           call s2g1(zhat,dhat,hhat,zhat2,dhat2,hhat2,
     *       ulalo,vlalo,hlalo,rklowork,rklowork(1,1,2),rklowork(1,1,3),
     *        1,jcap,nlon,nlath,irs,iis,ap,bp,aqr,bqr,gr,slat,clat,
     *         pe0,qe0,ro0,trigs,ifax)
           do 820 i=nlon+1,nlonex
             do 810 j=1,nlat
               ulalo(j,i)=ulalo(j,i-nlon)
               vlalo(j,i)=vlalo(j,i-nlon)
               hlalo(j,i)=hlalo(j,i-nlon)
810          continue
820        continue
c          CALL SCOF2GRD(HHAT,HLALO,JCAP,NLATH,NLON)
c          CALL VCOF2GRD(DHAT,ZHAT,ULALO,VLALO,JCAP,NLATH,NLON)
C----------
C---------- 9.  ROTATE WINDS FROM LOLA BACK TO POLA
C----------
           DO 920 I=1,NLONEX
             ANGLE=(I-1.)*DLON
             COSLON=COS(ANGLE)
             SINLON=SIN(ANGLE)
             DO 918 J=1,NLAT
               WLOLA1(J,1)=-ULALO(J,I)*SINLON
     *                        -VLALO(J,I)*COSLON
               WLOLA2(J,1)=ULALO(J,I)*COSLON
     *                        -VLALO(J,I)*SINLON
               ULALO(J,I)=WLOLA1(J,1)
               VLALO(J,I)=WLOLA2(J,1)
918          CONTINUE
920        CONTINUE
C---------- MULTIPLY H BY G TO GET GEOPOT INCREMENT
           DO 922 I=1,NLOLA
             HLALO(I,1)=G*HLALO(I,1)
922        CONTINUE
C----------
C---------- 10. INTERPOLATE LOLA GRID TO POLA GRIDS
C----------
           IORDLP = IIORDLP
           IORBLP = IIORBLP
           DO 1010 NG=1,NGRDUSE
            DO 1010 IORD=IORBLP,IORDLP
             CALL HLLO2POL(ULALO,NLONEX,NLAT,
     *           UIO(1,K,NG),IMG(NG),JMG(NG),DX(NG),
     *           XPOLE(NG),YPOLE(NG)-.5,RKPO,IKPO,BKPO,WLOLA1,
     *         XLOLA,YLOLA,XPOLA,YPOLA,RKPOWORK,IKPOWORK,rlathxy,DLON,
     *           IORD)
             CALL HLLO2POL(VLALO,NLONEX,NLAT,
     *           VIO(1,K,NG),IMG(NG),JMG(NG),DX(NG),
     *           XPOLE(NG)-.5,YPOLE(NG),RKPO,IKPO,BKPO,WLOLA1,
     *         XLOLA,YLOLA,XPOLA,YPOLA,RKPOWORK,IKPOWORK,rlathxy,DLON,
     *           IORD)
             CALL HLLO2POL(HLALO,NLONEX,NLAT,
     *           PIO(1,K,NG),IMG(NG),JMG(NG),DX(NG),
     *           XPOLE(NG),YPOLE(NG),RKPO,IKPO,BKPO,WLOLA1,
     *         XLOLA,YLOLA,XPOLA,YPOLA,RKPOWORK,IKPOWORK,rlathxy,DLON,
     *           IORD)
1010       CONTINUE
9000     CONTINUE
C        CALL BALPRNT(LOOP,NUMVERT)
       RETURN
       END
