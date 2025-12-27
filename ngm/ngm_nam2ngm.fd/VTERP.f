      SUBROUTINE VTERP
C                .      .    .                                       .
C SUBPROGRAM:    VTERP       INTERPOLATE ETA ANALYSIS ON PRESSURE
C                            TO NGM SIGMA LEVELS
C   PRGMMR: ROGERS           ORG: W/NP22     DATE: 98-11-05
C
C ABSTRACT: INTERPOLATE ETA ANALYSIS ON PRESSURE LEVELS TO NGM SIGMA
C           LEVELS. USE ANCIENT LOGIC WRITTEN BY GEOFF DIMEGO FOR GESPREP
C           CODE
C
C PROGRAM HISTORY LOG:
C   98-11-05  Eric Rogers  
C
C REMARKS:

C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  CRAY C-90
C$$$
      IMPLICIT REAL (A-H, O-Z)
      integer,parameter::real_32=selected_real_kind(6,30)
      PARAMETER (K15=SELECTED_REAL_KIND(15))
      INCLUDE "parmg104"
      INCLUDE 'parmodel'
C
      PARAMETER(ITOT=ILIM*JLIM,FACTOR=1005./9.81,R=287.05,
     1    G=9.81,CP=1005.0,ROG=R/G,GOR=G/R,RO2G=R/2.*G,
     2    ROCP=R/CP,GAMD=-9.81/1004.67,CPOR=CP/R,
     3    ALPHAR=.0065*R/G,IKMDBL=IKM+IKM+1,IKMQ=IKM-4)
      PARAMETER(IGOUT=ILIM,JGOUT=JLIM+1)
C
      REAL(KIND=K15) ALPINT(ILIM,JLIM,IKM+1),
     1    ALPMID(ILIM,JLIM,IKM),ALPTOP,RADICL,A,B,
     2    P3(51),PS3(51),ALNPB,ALNPT,
     3    ALPLEV(MAXLEV),PLEV(IKMDBL),P1(MAXLEV+1)
C
      REAL NGMZ(200,200),PINT(ILIM,JLIM,IKM+1),
     1    PMID(ILIM,JLIM,IKM)
      REAL UINT(ILIM,JLIM,IKM),ZNEWE(ILIM,JLIM,MAXLEV)
      REAL TBAR(ILIM,JLIM,KMAX+1),QBAR(ILIM,JLIM,KMAX)
      REAL TSOLD(ILIM,JlIM)
C
      INTEGER LEVATM(ILIM,JLIM),LEVSAV(ILIM,JLIM)
C
      COMMON /GRIDEF/ IMAX,JMAX,KMAX,ALAT1,ELON1,DXX,DYY,ELONV,
     1    ALATAN
C
C  N O T E : TEMPERATURE COMING OUT OF GETE104 IS ALREADY VIRTUAL!!!!!
C
      COMMON /EGRID /PS(ILIM,JLIM),ZS(ILIM,JLIM),
     1 Z(ILIM,JLIM,MAXLEV),TS(ILIM,JLIM),
     2 T(ILIM,JLIM,MAXLEV),QS(ILIM,JLIM),
     3 U(ILIM,JLIM,MAXLEV),
     4 V(ILIM,JLIM,MAXLEV),
     5 Q(ILIM,JLIM,MAXLEV),MASKE(ILIM,JLIM),
     6 MASKV(ILIM,JLIM)
C
      COMMON /CONSTS/ IH, IQ, IT, IU, IV, IZ,
     1                NLAT, LEVH, LEVS, LONF, NSIGSTP,
     2                SLAT( INLAT ), STRIPARA( INSIGSTP ,  INLAT )
C
      COMMON /CONSTN/ IMG( INGRDUSE ), JMG( INGRDUSE ),
     1                IAG( INGRDUS1 ), JAG( INGRDUS1 ),
     2                IBG( INGRDUS1 ), JBG( INGRDUS1 ),
     3                IADDRG( INIADDRS ,  INGRDUSE ),
     4                INDEXHU,  INDEXHV,  INDEXHTH, INDEXHQ,
     5                INDEXH,   INDEXST,  INDEXSQ,  INDEXALB,
     6                INDEXSLP, INDEXPSI, INDEXCD,  INDEXCC,  INDEXGSP,
     7                INDEXRAD, INDEXDLW, INDEXDSW, INDEXCPR, INDEXMA,
     8                INDEXSEC, INDEXZ0, INDEXTSS,  INDEXCG,  INDEXDHQ,
     9                INDEXTSD, KM, NH, NGRDUSE,
     1                SPECS( INSPECS ), DELSIG( IKM ),
     2                POVH( IKM ), PIOVHK( IKM ),
     3                CMUU  ( INGRDUSE ),
     4                XPOLEH( INGRDUSE ), YPOLEH( INGRDUSE ),
     5                XPOLEU( INGRDUSE ), YPOLEU( INGRDUSE ),
     6                XPOLEV( INGRDUSE ), YPOLEV( INGRDUSE ),
     7                DLAMNGM,
     8                BITSEA( IIJMAX ,  INGRDUSE ),
     9                BITSNO( IIJMAX ,  INGRDUSE ),
     1                BITWVL( IIJMAX ,  INGRDUSE )
C
      COMMON /NGMSIGS/ SIGMID(IKM), SIGINT(IKM+1)
      COMMON    VBL( INVBL )
C
      COMMON /NEWNGM/ZNEW(ILIM,JLIM,IKM),TNEW(ILIM,JLIM,IKM),
     1  UNEW(ILIM,JLIM,IKM),VNEW(ILIM,JLIM,IKM),
     2  QNEW(ILIM,JLIM,IKMQ),PSNEW(ILIM,JLIM),
     3  TSNEW(ILIM,JLIM)
C
      COMMON /VWGTS/ WU(IGOUT,JGOUT,6),WV(IGOUT,JGOUT,6)
C
      COMMON /IPOINT/ IPT,JPT
C
      DIMENSION GRID(ITOT),DIFF(5)
      REAL EPLEV(MAXLEV)
      REAL TLEV(MAXLEV+1),ZLEV(IKMDBL),Z3(MAXLEV+1)
      REAL PS1(MAXLEV),P2(IKM+1),PS2(IKM)
      REAL TEMP(IKMDBL),UTEMP(MAXLEV),QTEMP(MAXLEV)
      REAL UNTMP(IKM),PS1T(MAXLEV),QNTMP(IKMQ)
C
      DATA EPLEV/1000.,975.,950.,925.,900.,875.,850.,825.,
     1            800.,775.,750.,725.,700.,675.,650.,625.,
     1            600.,575.,550.,525.,500.,475.,450.,425.,
     1            400.,375.,350.,325.,300.,275.,250.,225.,
     1            200.,175.,150.,125.,100.,75.,50./
C
      IZTERP = 1
      PTOP = 10.0
      ALPTOP = LOG(PTOP)
C
C  COMPUTE LOGS OF PRESSURE LEVELS
C
      DO K = 1, MAXLEV
        ALPLEV(K) = LOG(EPLEV(K))
      ENDDO
C
C  EXTRACT SURFACE TERRAIN HEIGHT ON SUPER C-GRID FROM 
C  VBL
C
      IM = IMG(2)
      JM = JMG(2)
      ISTART = IADDRG(10,2)
      DO J = 1, JM
        DO I = 1, IM
          NGMZ(I,J)  = VBL(ISTART) * FACTOR
          IF(I.EQ.IPT .AND. J.EQ.JPT) THEN
            print *,'NGMZ AT ',I,J,' = ',NGMZ(I,J)
          ENDIF
          ISTART = ISTART + 1
        ENDDO
      ENDDO
C
C     CALL PRINTNGM(NGMZ)
C
C  GET INDEX OF FIRST PRESSURE LEVEL IN THE ETA 
C  ANALYSIS ABOVE GROUND (1=1000 MB,39=50 MB)
C  IF SURFACE IS < 7.5 MB FROM PRESSURE SURFACE
C  USE THE NEXT HIGHEST ONE TO PREVENT EXTRAPOLATION     
C
      DO J = 1, JLIM
       DO I = 1, ILIM
        IF(MASKE(I,J) .EQ. 1) THEN
         DO K = 1,MAXLEV
          IF(PS(I,J).GE.EPLEV(K)) THEN
            PDIFF = PS(I,J) - EPLEV(K)
            LEVATM(I,J) = K
            LEVSAV(I,J) = K
            IF(PDIFF.LE.7.5) LEVATM(I,J) = K + 1
            IF(I.EQ.IPT .AND. J.EQ.JPT) THEN
             write(6,95)i,j,ps(i,j),k,eplev(k),levatm(i,j)
 95          format(1x,2i4,1x,f9.2,1x,i2,1x,f9.2,1x,i3)
            ENDIF
            GO TO 100 
          ENDIF
         ENDDO
100      CONTINUE
        ENDIF
       ENDDO
      ENDDO
C
CTEST
C
C  RECOMPUTE HEIGHTS FROM ETA ANALYSIS
C
      DO J = 1, JLIM
       DO I = 1, ILIM
        IF(MASKE(I,J).EQ.1) THEN
         LEVA = LEVSAV(I,J)
         DO K = LEVA, MAXLEV
          IF(K.EQ.LEVA) THEN
            ZNEWE(I,J,K-1) = ZS(I,J)
            T2 = T(I,J,K)
            T3 = T(I,J,K+1)
            Q2 = Q(I,J,K)
            Q3 = Q(I,J,K+1)
            DELT = (LOG(PS(I,J))-ALPLEV(K))*
     1               ((T2-T3)/(ALPLEV(K)-ALPLEV(K+1)))
            DELQ = (LOG(PS(I,J))-ALPLEV(K))*
     1               ((Q2-Q3)/(ALPLEV(K)-ALPLEV(K+1)))
            T1 = T(I,J,K) + DELT
            Q1 = Q(I,J,K) + DELQ
            TSOLD(I,J) = TS(I,J)
            TS(I,J) = T1
            TBAR(I,J,K) = (TS(I,J) + T2) * 0.5
            PTOPT = ALPLEV(K)
            PBOT = LOG(PS(I,J))
          ELSE
            T1 = T(I,J,K-1)
            T2 = T(I,J,K)
            TBAR(I,J,K) = (T1 + T2) * 0.5
            IF(K.EQ.MAXLEV) TBAR(I,J,K+1) = (227.0 + T2) * 0.5
            PTOPT = ALPLEV(K)
            PBOT = ALPLEV(K-1)
          ENDIF
            ZNEWE(I,J,K) = ZNEWE(I,J,K-1)+(ROG*TBAR(I,J,K)*
     1         (PBOT-PTOPT))
            IF(I.EQ.IPT .AND. J.EQ.JPT) THEN
             print *,i,j,k,znewe(i,j,k),z(i,j,k),eplev(k),
     1          tbar(i,j,k),PTOPT,pbot,zs(i,j)
            ENDIF
          Z(I,J,K) = ZNEWE(I,J,K)     
         ENDDO
        ENDIF
       ENDDO
      ENDDO
C
C  DO VERTICAL INTERPOLATION USING LOGIC FROM GESPREP
C
C  RECOMPUTE SURFACE PRESSURE BASED ON NGM TERRAIN
C
      DO J = 1, JLIM
       DO I = 1, ILIM
        IF(MASKE(I,J).EQ.1) THEN
c        if(j.eq.1) write(0,*)i,j
         PSTAR = PS(I,J)
         KSTART = LEVATM(I,J)
         DO K = KSTART, MAXLEV
           TLEV(K) = T(I,J,K)
           IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
             WRITE(6,2350)I,J,K,LEVATM(I,J),TLEV(K),PSTAR
 2350        FORMAT(1X,4I4,2(1X,F12.3))
           ENDIF
         ENDDO
 2360    CONTINUE
         K = LEVATM(I,J)
 2370    CONTINUE
         PLO = EPLEV(K)
         ZLO = Z(I,J,K)        
         TLO = TLEV(K)
         K = K + 1
 2380    CONTINUE
         IF(NGMZ(I,J) .GT. Z(I,J,K)) GO TO 2370
         ALNPB = ALPLEV(K-1)
         ALNPT = ALPLEV(K)
         DLNP = ALNPB - ALNPT
         ALNP3 = 0.5*(ALNPB+ALNPT) 
         A = G * (Z(I,J,K) - ZLO) / DLNP
         B = R * (TLO - TLEV(K)) / DLNP
         PHI3 = G*0.5*(ZLO+Z(I,J,K)) + 0.125*B*DLNP*DLNP
         RADICL = A*A - 2.*B*(G*NGMZ(I,J) - PHI3) 
         IF(RADICL.GE.0.0.AND.I.EQ.IPT.AND.J.EQ.JPT) THEN
           WRITE(6,110) RADICL,I,J,A,B,NGMZ(I,J),K
  110      FORMAT(' radicl,I,J,A,B,ZS,K ',G15.5,2I4,3G15.5,I5)
           WRITE(6,115)EPLEV(K),Z(I,J,K),TLEV(K)
           WRITE(6,115)PLO,ZLO,TLO
  115      FORMAT(1X,3(G12.5))
         ENDIF
         IF(RADICL.LE.0.0) THEN
           WRITE(6,120) RADICL,I,J,A,B,NGMZ(I,J),K
  120      FORMAT(' radicl<0,I,J,A,B,ZS,K ',G15.5,2I4,3G15.5,I5)
           WRITE(6,115)EPLEV(K),Z(I,J,K),TLEV(K)
           WRITE(6,115)PLO,ZLO,TLO
           GO TO 2500
         ENDIF
         ALNPS = ALNP3-2.*(G*NGMZ(I,J)-PHI3) /
     1      (A + SQRT(RADICL))
         PSNEW(I,J) = EXP(ALNPS)
         TSNEW(I,J) = TLO - (ALNPB-ALNPS)*B/R
         GO TO 2600
C
C   NGM TERRAIN IS LOWER THAN ETA TERRAIN
C         
 2500    print *,'ngm is lower than eta ',i,j,zs(i,j),ngmz(i,j),
     1     ps(i,j),ts(i,j),tsold(i,j)
         PSNEW(I,J) = PS(I,J) 
         TSNEW(I,J) = TS(I,J)
 2600    CONTINUE
         IF(NGMZ(I,J).GE.1000. .OR.
     1      I.EQ.IPT .AND.J.EQ.JPT) THEN
c        IF(MOD(I,5).EQ.0 .AND. MOD(J,5).EQ.0) THEN
           WRITE(6,160)I,J,PS(I,J),PSNEW(I,J),TS(I,J),
     1       TSNEW(I,J),NGMZ(I,J)
  160      FORMAT(1X,2I4,5(1X,E12.5))
         ENDIF
C
C   ENDIF ON MASKE
C
        ENDIF
       ENDDO
      ENDDO
      write(6,*)' done with surface pressure computation'
C
C  COMPUTE PRESSURE/LOG OF PRESSURE ON NGM SIGMA INTERFACES/MIDLAYERS
C  USING RECOMPUTED SURFACE PRESSURE
C
      DO K = 1, IKM + 1
       DO J = 1, JLIM
        DO I = 1, ILIM
         IF(MASKE(I,J).EQ.1) THEN
          IF(K .LE. IKM) THEN
           PINT(I,J,K) = SIGINT(K) * PSNEW(I,J)
           ALPINT(I,J,K) = ALOG(PINT(I,J,K))
           PMID(I,J,K) = SIGMID(K) * PSNEW(I,J)
           ALPMID(I,J,K) = ALOG(PMID(I,J,K))
          ELSE
           PINT(I,J,K) = PTOP
           ALPINT(I,J,K) = ALPTOP
          ENDIF
          IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
            print *,'pint ',i,j,k,PSNEW(I,J),SIGINT(K),PINT(I,J,K),
     1                ALPINT(I,J,K)
            IF(K .LE. IKM) THEN
             print *,'pmid ',i,j,k,PSNEW(I,J),SIGMID(K),PMID(I,J,K),
     1                ALPMID(I,J,K)
            ENDIF
          ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C
C  RECOMPUTE LEVATM BASED ON NEW SURFACE PRESSURE
C  ONLY IF PSNEW < PSOLD
C
      DO J = 1, JLIM
       DO I = 1, ILIM
        IF(MASKE(I,J) .EQ. 1) THEN
         IF(PSNEW(I,J).LT.PS(I,J)) THEN
          DO K = 1,MAXLEV
           IF(PSNEW(I,J).GE.EPLEV(K)) THEN
            PDIFF = PS(I,J) - EPLEV(K)
            LEVATM(I,J) = K
            IF(PDIFF.LE.7.5) LEVATM(I,J) = K + 1
            IF(I.EQ.IPT .AND. J.EQ.JPT) THEN
             write(6,995)i,j,psnew(i,j),k,eplev(k),levatm(i,j)
995          format(1x,2i4,1x,f9.2,1x,i2,1x,f9.2,1x,i3)
            ENDIF
            GO TO 1000
           ENDIF
          ENDDO
         ENDIF
1000     CONTINUE
        ENDIF
       ENDDO
      ENDDO
CTEST
C
C  GET HEIGHTS/TEMPS ON NGM MID-LAYERS
C 
      DO J = 1, JLIM
       DO I = 1, ILIM
        IF(MASKE(I,J).EQ.1) THEN 
c        print *,'start of hgt/temp loop ',i,j
         LSFC = LEVATM(I,J)
         LN = 1
         POLD = PS(I,J)
         PNEW = PSNEW(I,J)
         DO K = 1, MAXLEV
          PS1(K) = EPLEV(K)
         ENDDO
         DO K = 1, IKM
          PS2(K) = PMID(I,J,K)
         ENDDO
         ZTOP = Z(I,J,MAXLEV) - 
     +     ROG * T(I,J,MAXLEV) * (ALPTOP - ALPLEV(MAXLEV)) 
         PDIV = POLD / PNEW
         IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
          write(6,*)'ztop ',i,j,ztop,Z(I,J,MAXLEV),T(I,J,MAXLEV),
     +     ALPTOP,ALPLEV(MAXLEV),POLD,PNEW
          write(6,*)'pdiv test ',pold,pnew,pdiv,sigmid(1)
         ENDIF
         IF(PDIV .GE. SIGMID(1)) GO TO 29
C
         LEV3 = MAXLEV + 1
         P3(1) = ALPINT(I,J,1)
         Z3(1) = NGMZ(I,J)
         TLEV(1) = GOR * (Z(I,J,LSFC)-Z3(1))/
     +                   (P3(1)-ALPLEV(LSFC))
         IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
          write(6,*)'tlev(1) ',i,j,tlev(1),Z(I,J,LSFC),
     +       Z3(1),P3(1),ALPLEV(LSFC)
         ENDIF
         KCNT = 1
         DO K = LSFC, MAXLEV
          P3(KCNT+1) = ALPLEV(K)
          Z3(KCNT+1) = Z(I,J,K)
          TLEV(KCNT+1) = T(I,J,K)
          IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
           write(6,*)'tlev ',i,j,kcnt+1,tlev(kcnt+1),Z3(KCNT+1),
     +        P3(KCNT+1)
          ENDIF
          KCNT = KCNT + 1
         ENDDO
         KCNT = KCNT - 1
         GO TO 31
   29    CONTINUE
         LEV3 = MAXLEV + 1 
c        LEV3 = MAXLEV + 1 - LSFC
         KCNT = 0
         DO K = LSFC, MAXLEV
          KCNT = KCNT + 1
          P3(KCNT) = ALPLEV(K)
          Z3(KCNT) = Z(I,J,K)
          TLEV(KCNT) = TBAR(I,J,K+1)
c         TLEV(KCNT) = TBAR(I,J,K)
c         TLEV(KCNT) = T(I,J,K)
          IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
           write(6,*)'kcnt ',kcnt,k,lsfc,maxlev,p3(kcnt),
     +       z3(kcnt),tlev(kcnt)
          ENDIF
         ENDDO
   31    CONTINUE
         KCNT = KCNT + 1
         P3(KCNT) = ALPTOP
         Z3(KCNT) = ZTOP
C
C  SET UP TARGET PROFILE  INTERFACE & MID-POINT LEVELS
C
         ZLEV(1) = NGMZ(I,J)
         PLEV(1) = ALPINT(I,J,1)
         KK = 1
         DO K = 1, IKM
           KK = KK + 1
           PLEV(KK) = ALPMID(I,J,K)
           KK = KK + 1
           PLEV(KK) = ALPINT(I,J,K+1)
         ENDDO
         IF(IZTERP.EQ.1) THEN
          DO K = 2, KK
           IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
             do ker = 1, kcnt
              print *,'before xintrp ',ker,kcnt,z3(ker),p3(ker),
     +           plev(k)
             enddo
           ENDIF
           CALL XINTRP(Z3,P3,PLEV(K),KCNT,ZLEV(K))
c          ZLEV(K) = XINTRP(Z3,P3,PLEV(K),KCNT)
           IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
             write(6,*)'zlevint ',I,J,k,kl,ZLEV(K),PLEV(K),
     +         Z3(KL),TLEV(KL),P3(KL)
           ENDIF
          ENDDO
         ELSE
          DO K = 2, KK
           DO KLK = LSFC+1, LEV3 
c          DO LK = LSFC, LEV3 
            LK = KLK - LSFC + 1
            IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
             print *,'p3 test ',k,klk,lk,lsfc,lev3,p3(lk),plev(k)
            ENDIF
             IF(P3(LK).GE.PLEV(K)) GO TO 24
c            KL = LK - LSFC
             KL = LK - 1
             GO TO 23 
  24         CONTINUE
           ENDDO
           KL = LEV3 - 1
  23       ZLEV(K) = Z3(KL) - ROG*TLEV(KL)*(PLEV(K)-P3(KL))
           IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
             write(6,*)'zlev ',I,J,k,kl,ZLEV(K),PLEV(K),
     +         Z3(KL),TLEV(KL),P3(KL)
           ENDIF
          ENDDO
         ENDIF
CDIAG
C
C  COMPUTE ORIGINAL TEMPERATURES
C
         DO K = 1, IKM
           KPK = K + K
           TEMP(K) = GOR*(ZLEV(KPK+1)-ZLEV(KPK-1))/
     +           (PLEV(KPK-1)-PLEV(KPK+1))
           IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
            write(6,*)'temp ',I,J,k,kpk,temp(k),ZLEV(KPK+1),
     +            ZLEV(KPK-1),PLEV(KPK-1),PLEV(KPK+1)
           ENDIF
         ENDDO
C
C  COMPUTE AND ADJUST FULL PROFILE OF TEMPERATURES
C
         DO K = 2, KK
           TLEV(K-1)=GOR*(ZLEV(K)-ZLEV(K-1))/
     +                   (PLEV(K-1)-PLEV(K))
           IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
             write(6,*)'tlev ',i,j,k,TLEV(K-1),ZLEV(K),
     +            ZLEV(K-1),PLEV(K-1),PLEV(K)
           ENDIF
         ENDDO
C
         ITER = 0
           NTOP = KK - 1
  25       KEY = 0
         ITER = ITER + 1
         DO K = 2, NTOP
          DT = TLEV(K) - TLEV(K-1)
          DZ =(ZLEV(K+1) - ZLEV(K-1)) * 0.5
          DTDZ = DT / DZ
C         IF(DTDZ.GE.GAMD)GO TO 35
          IF(DTDZ.LT.GAMD)THEN
           KEY = 99
           DELT = GAMD * DZ - DT
           PRAT = (PLEV(K)-PLEV(K+1))/(PLEV(K-1)-PLEV(K))
           CTOP = 1.0 / (1.0 + PRAT)
           CBOT = 1.0 - CTOP
           TLEV(K-1) = TLEV(K-1) - CBOT * DELT - .05
           TLEV(K) = TLEV(K) + CTOP * DELT + .05
          ENDIF
         ENDDO
         DO K = 2,KK
          OLDZEV = ZLEV(K)
          ZLEV(K)=ZLEV(K-1)+ROG*TLEV(K-1)*(PLEV(K-1)-PLEV(K))
          IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
           print *,'new zlev ',i,j,k,oldzev,zlev(k),ZLEV(K-1),
     +       TLEV(K-1),PLEV(K-1),PLEV(K)
          ENDIF
         ENDDO
         IF(KEY.NE.0 .AND. ITER.LE.15) GO TO 25
C
C  STORE HEIGHTS ON NGM INTERFACES
C 
         DO K = 1, IKM
           KPK = K + K
           ZNEW(I,J,K) = ZLEV(KPK)
           TNEW(I,J,K) = GOR*(ZLEV(KPK+1)-ZLEV(KPK-1))/
     +                       (PLEV(KPK-1)-PLEV(KPK+1))
           IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
             WRITE(6,1500)I,J,K,PMID(I,J,K),ZNEW(I,J,K),
     +                    TNEW(I,J,K)
 1500        FORMAT(1X,3I4,3(1X,F12.3))
           ENDIF
         ENDDO
C
C INTERPOLATE MOISTURE
C
        DO K = 1, MAXLEV
          QTEMP(K) = 0.0
          PS1T(K) = 0.0
        ENDDO
C
        KCNT = 0
        DO K = LSFC, MAXLEV
          KCNT = KCNT + 1
          QTEMP(KCNT) = Q(I,J,K)
          PS1T(KCNT) = PS1(K)
          IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
           write(6,*)'qtemp ',i,j,k,kcnt,qtemp(kcnt),
     +       ps1t(kcnt)
          ENDIF
        ENDDO
C
        DO K = 1, IKMQ
          PS2(K) = PMID(I,J,K)
          IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
            write(6,*)'ps2 ',k,ps2(k)
          ENDIF 
        ENDDO
C
        CALL VEXPND(QTEMP,PS1T,KCNT,QNTMP,PS2,IKMQ,1)
        IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
          write(6,*)'qntmp ',i,j,qntmp
        endif
C
        DO K = 1, IKMQ
          QNEW(I,J,K) = MAX(0.0,QNTMP(K))
          IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
            WRITE(6,*)'qnew ',I,J,K,PMID(I,J,K),QNEW(I,J,K)
          ENDIF
        ENDDO
C
C  COMPUTE DRY TEMP FOR ADIABATIC ADJUSTMENT
C
        DO K = 1, IKM
          IF(K.LE.IKMQ) THEN
            QTEP = MAX(0.0,QNTMP(K))
            TEMP(K) = TNEW(I,J,K)/(1.0+.61*QTEP)
          ELSE
            TEMP(K) = TNEW(I,J,K)
          ENDIF
          IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
           write(6,*)'temp before adabat ',i,j,k,temp(k)
          ENDIF
        ENDDO
C
        CALL ADABAT(TEMP,PS2,0)
C
        DO K = 1, IKM
          IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
           write(6,*)'temp after adabat ',i,j,k,temp(k)
          ENDIF
          IF(K.LE.IKMQ) THEN
            QSHOLD = QFRMTP(TEMP(K),PS2(K))
            QHOLD = MIN(QSHOLD,QNTMP(K))
            QNEW(I,J,K) = QHOLD
            TNEW(I,J,K) = TEMP(K)*(1.0+.61*QHOLD)
            IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
             write(6,*)'q after adabat ',i,j,k,qhold
            ENDIF
          ELSE
            TNEW(I,J,K) = TEMP(K)
          ENDIF
        ENDDO
C
C END TEST ON MASKE
        ENDIF
       ENDDO
      ENDDO
      print *,'get interpolation weights for wind'
C
      CALL VWEIGHTS
C
C INTERPOLATE U and V WIND COMPONENTS LINEAR IN LN P
C NOTE: CHECK ON BOTH MASS AND WIND MASKS SINCE THERE ARE
C       WIND POINTS WITHOUT MASS DATA AND VISA VERSA
C
      DO J = 1, JLIM
       DO I = 1, ILIM
        IF(MASKV(I,J).EQ.1 .AND. MASKE(I,J).EQ.1) THEN
         LSFC = LEVATM(I,J)
         DO K = 1, MAXLEV
           UTEMP(K) = 0.0
           P1(K) = 0.0
           PS1T(K) = 0.0
         ENDDO
C
         DO L = 1, 2
           KCNT = 0
           DO K = LSFC, MAXLEV
             KCNT = KCNT + 1
             IF(L.EQ.1) THEN
               UTEMP(KCNT) = U(I,J,K)
             ELSE
               UTEMP(KCNT) = V(I,J,K)
             ENDIF
             P1(KCNT) = ALPLEV(K)
             PS1T(KCNT) = PS1(K)
             IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
              write(6,*)'utemp ',i,j,k,l,kcnt,utemp(kcnt),
     +          p1(kcnt),ps1t(kcnt)
             ENDIF 
           ENDDO
C
           DO K = 1, IKM
             PS2(K) = PMID(I,J,K)
             IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
               write(6,*)'ps2 ',k,ps2(k)
             ENDIF 
           ENDDO
C           
           CALL VEXPND(UTEMP,PS1T,KCNT,UNTMP,PS2,IKM,1)
           IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
             write(6,*)'untmp ',i,j,k,l,ps2(k),untmp
           endif
           DO K = 1, IKM
             IF(L.EQ.1) THEN
               UNEW(I,J,K) = UNTMP(K)
               IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
                 WRITE(6,*)'unew ',I,J,K,PMID(I,J,K),UNEW(I,J,K)
               ENDIF
             ELSE
               VNEW(I,J,K) = UNTMP(K)
               IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
                 WRITE(6,*)'vnew ',I,J,K,PMID(I,J,K),VNEW(I,J,K)
               ENDIF
             ENDIF
           ENDDO
         ENDDO 
C
C END TEST ON MASKV
        ENDIF
       ENDDO
      ENDDO
C
C CONVERT SURFACE PRESSURE TO BARS, COMPUTE POTENTIAL
C TEMPERATURE AND MULTIPLY ALL VARIABLES BY SURFACE PRESSURE
C
      DO J = 1, JLIM
       DO I = 1, ILIM
        IF(MASKE(I,J).EQ.1) THEN
         PSNEW(I,J) = PSNEW(I,J) * 0.001
        ENDIF
       ENDDO
      ENDDO
C
      DO K = 1, IKM
       DO J = 1, JLIM
        DO I = 1, ILIM
         IF(MASKE(I,J).EQ.1) THEN
          IF(K.LE.IKMQ) THEN
           TEMP(K) = TNEW(I,J,K)/(1.0+.61*QNEW(I,J,K)) 
          ELSE
           TEMP(K) = TNEW(I,J,K)
          ENDIF
          THETA = TEMP(K)*(1000.0/PMID(I,J,K))**ROCP 
          TNEW(I,J,K) = THETA * PSNEW(I,J)
          IF(I.EQ.IPT.AND.J.EQ.JPT) THEN
           write(6,*)'theta ',i,j,k,temp(k),pmid(i,j,k),
     +       theta,tnew(i,j,k),psnew(i,j)
          ENDIF 
          IF(K.LE.IKMQ) THEN
           QNEW(I,J,K) = QNEW(I,J,K) * PSNEW(I,J)
          ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C
      DO K = 1, IKM
       DO J = 1, JLIM
        DO I = 1, ILIM
         IF(MASKE(I,J).EQ.1 .AND. MASKV(I,J).EQ.1) THEN
          UNEW(I,J,K) = UNEW(I,J,K) * PSNEW(I,J)
          VNEW(I,J,K) = VNEW(I,J,K) * PSNEW(I,J)
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C
C  INTERPOLATE U-COMP AT MASS POINTS TO U POINTS
C
      DO K = 1, IKM
       DO J = 1, JLIM
        DO I = 1, ILIM
         IF(MASKE(I,J).EQ.1 .AND. MASKV(I,J).EQ.1) THEN
           IF(I.EQ.1) THEN
            UINT(I,J,K) = UNEW(I,J,K)*WU(I,J,2) + 
     1                    UNEW(I+1,J,K)*WU(I,J,3) +
     2                    UNEW(I,J+1,K)*WU(I,J,5) + 
     3                    UNEW(I+1,J+1,K)*WU(I,J,6)   
           ENDIF
           IF(I.EQ.ILIM) THEN
            UINT(I,J,K) = UNEW(I-1,J,K)*WU(I,J,1) + 
     1                    UNEW(I,J,K)*WU(I,J,2) +
     2                    UNEW(I-1,J+1,K)*WU(I,J,4) + 
     3                    UNEW(I,J+1,K)*WU(I,J,5)
           ENDIF
           IF(I.GT.1 .AND. I.LT.ILIM) THEN
            UINT(I,J,K) = UNEW(I-1,J,K)*WU(I,J,1) + 
     1                    UNEW(I,J,K)*WU(I,J,2) + 
     2                    UNEW(I+1,J,K)*WU(I,J,3) + 
     3                    UNEW(I-1,J+1,K)*WU(I,J,4) +
     4                    UNEW(I,J+1,K)*WU(I,J,5) + 
     5                    UNEW(I+1,J+1,K)*WU(I,J,6)
           ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C
      DO K = 1, IKM
       DO J = 1, JLIM
        DO I = 1, ILIM
         IF(MASKE(I,J).EQ.1 .AND. MASKV(I,J).EQ.1) THEN
          IF(I.EQ.IPT .AND.J.EQ.JPT) THEN
           print *,'unew,uint = ',i,j,k,unew(i,j,k),uint(i,j,k)
          ENDIF
          UNEW(I,J,K) = UINT(I,J,K)
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C
C  INTERPOLATE V-COMP AT MASS POINTS TO V POINTS
C
      DO K = 1, IKM
       DO J = 1, JLIM
        DO I = 1, ILIM-1
         IF(MASKE(I,J).EQ.1 .AND. MASKV(I,J).EQ.1) THEN
          IF(J.EQ.1) THEN
           UINT(I,J,K) = VNEW(I,J,K)*WV(I,J,3) +
     1                   VNEW(I+1,J,K)*WV(I,J,4) +
     2                   VNEW(I,J+1,K)*WV(I,J,5) +
     3                   VNEW(I+1,J+1,K)*WV(I,J,6)
          ELSE
           UINT(I,J,K) = VNEW(I,J-1,K)*WV(I,J,1) +
     1                   VNEW(I+1,J-1,K)*WV(I,J,2) +
     2                   VNEW(I,J,K)*WV(I,J,3) +
     3                   VNEW(I+1,J,K)*WV(I,J,4) +
     4                   VNEW(I,J+1,K)*WV(I,J,5) +
     5                   VNEW(I+1,J+1,K)*WV(I,J,6)
          ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C
      I = ILIM
      DO K = 1, IKM
       DO J = 1, JLIM
        IF(MASKE(I,J).EQ.1 .AND. MASKV(I,J).EQ.1) THEN
         UINT(I,J,K) = VNEW(I,J,K)
        ENDIF
       ENDDO
      ENDDO
C
      DO K = 1, IKM
       DO J = 1, JLIM
        DO I = 1, ILIM
         IF(MASKE(I,J).EQ.1 .AND. MASKV(I,J).EQ.1) THEN
          IF(I.EQ.IPT .AND.J.EQ.JPT) THEN
           print *,'vnew,vint = ',i,j,k,vnew(i,j,k),uint(i,j,k)
          ENDIF
          VNEW(I,J,K) = UINT(I,J,K)
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C
C DONE WITH WIND INTERPOLATION : BEGIN UPDATE OF VBL
C SINCE WE HAVE NO ETA ANALYSIS INFORMATION ABOVE 50 MB, 
C DO NOT USE ANY ETA ANALYSIS INFORMATION AT K=16
C
      NG=2
C
C   U-COMPONENT
C
      ICOUNT=IADDRG(1,NG)
      print *,'start of u on grid 2 at ',icount
      DO K = 1, IKM
       DO J = 1, JGOUT
        DO I = 1, IGOUT
         IF(J.LE.JLIM) THEN
          IF(MASKE(I,J).EQ.1 .AND. MASKV(I,J).EQ.1) THEN
           IF(I.EQ.IPT .AND. J.EQ.JPT .AND. K.LT.IKM) THEN
            print *,'u update ',i,j,k,icount,vbl(icount),unew(i,j,k)
           ENDIF
           IF(K.LT.IKM) VBL(ICOUNT) = UNEW(I,J,K)
          ENDIF
         ENDIF
         ICOUNT = ICOUNT + 1
        ENDDO
       ENDDO
      ENDDO
C
C   V-COMPONENT
C
      ICOUNT=IADDRG(2,NG)
      print *,'start of v on grid 2 at ',icount
      DO K = 1, IKM
       DO J = 1, JGOUT
        DO I = 1, IGOUT
         IF(J.LE.JLIM) THEN
          IF(MASKE(I,J).EQ.1 .AND. MASKV(I,J).EQ.1) THEN
           IF(I.EQ.IPT .AND. J.EQ.JPT .AND. K.LT.IKM) THEN
            print *,'v update ',i,j,k,icount,vbl(icount),vnew(i,j,k)
           ENDIF
           IF(K.LT.IKM) VBL(ICOUNT) = VNEW(I,J,K)
          ENDIF
         ENDIF
         ICOUNT = ICOUNT + 1
        ENDDO
       ENDDO
      ENDDO
C
C   POTENTIAL TEMPERATURE
C
      ICOUNT=IADDRG(3,NG)
      print *,'start of theta on grid 2 at ',icount
      DO K = 1, IKM
       DO J = 1, JGOUT
        DO I = 1, IGOUT
         IF(J.LE.JLIM) THEN
          IF(MASKE(I,J).EQ.1) THEN
           IF(I.EQ.IPT .AND. J.EQ.JPT .AND. K.LT.IKM) THEN
            print *,'theta update ',i,j,k,icount,vbl(icount),tnew(i,j,k)
           ENDIF
           IF(K.LT.IKM) VBL(ICOUNT) = TNEW(I,J,K)
          ENDIF
         ENDIF
         ICOUNT = ICOUNT + 1
        ENDDO
       ENDDO
      ENDDO
C
C   SPECIFIC HUMIDITY
C
      ICOUNT=IADDRG(4,NG)
      print *,'start of q on grid 2 at ',icount
      DO K = 1, IKMQ
       DO J = 1, JGOUT
        DO I = 1, IGOUT
         IF(J.LE.JLIM) THEN
          IF(MASKE(I,J).EQ.1) THEN
           IF(I.EQ.IPT .AND. J.EQ.JPT) THEN
            print *,'q update ',i,j,k,icount,vbl(icount),qnew(i,j,k)
           ENDIF
           VBL(ICOUNT) = QNEW(I,J,K)
          ENDIF
         ENDIF
         ICOUNT = ICOUNT + 1
        ENDDO
       ENDDO
      ENDDO
C
C   SURFACE PRESSURE
C
      ICOUNT=IADDRG(5,NG)
      print *,'start of ps on grid 2 at ',icount
      DO J = 1, JGOUT
       DO I = 1, IGOUT
        IF(J.LE.JLIM) THEN
         IF(MASKE(I,J).EQ.1) THEN
          IF(mod(i,5).eq.0 .and. mod(j,5).eq.0) THEN
           psgbl=vbl(icount)*1000.
           pseta=psnew(i,j)*1000.
           print *,'ps update ',i,j,icount,psgbl,pseta
          ENDIF
          VBL(ICOUNT) = PSNEW(I,J)
         ENDIF
        ENDIF
        ICOUNT = ICOUNT + 1
       ENDDO
      ENDDO
     
      RETURN
      END
