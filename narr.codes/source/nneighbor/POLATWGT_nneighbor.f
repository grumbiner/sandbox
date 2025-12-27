C-----------------------------------------------------------------------
      SUBROUTINE POLATWGT(KGDSI,KGDSO,MI,MO,KGRIDOT,
     &                    N11,N21,N12,N22,NPP,NV11,NV21,NV12,NV22,
     &                    W11,W21,W12,W22,WV11,WV21,WV12,WV22,
     &                    C11,C21,C12,C22,S11,S21,S12,S22,
     &                    NO,RLAT,RLON,CROT,SROT,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  POLATWGT   COMPUTE INTERPOLATION/ROTATION WEIGHTS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS COMPUTES BILINEAR INTERPOLATION
C           WEIGHTS FROM ANY GRID TO ANY GRID FOR SCALR/VECTOR FIELDS.
C           NO OPTIONS ARE ALLOWED.
C           THE GRIDS ARE DEFINED BY THEIR GRID DESCRIPTION SECTIONS
C           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63).
C           THE CURRENT CODE RECOGNIZES THE FOLLOWING PROJECTIONS:
C             (KGDS(1)=000) EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=001) MERCATOR CYLINDRICAL
C             (KGDS(1)=003) LAMBERT CONFORMAL CONICAL
C             (KGDS(1)=004) GAUSSIAN CYLINDRICAL (SPECTRAL NATIVE)
C             (KGDS(1)=005) POLAR STEREOGRAPHIC AZIMUTHAL
C             (KGDS(1)=201) STAGGERED ROTATED EQUIDISTANT 
C                             CYLINDRICAL (ETA NATIVE)
C             (KGDS(1)=202) ROTATED EQUIDISTANT CYLINDRICAL (ETA NATIVE)
C             (KGDS(1)=203) STAGGERED ROTATED EQUIDISTANT 
C                             CYLINDRICAL (2-D ETA NATIVE)
C           WHERE KGDS COULD BE EITHER INPUT KGDSI OR OUTPUT KGDSO.
C           THE INPUT AND OUTPUT VECTORS ARE ROTATED SO THAT THEY ARE
C           EITHER RESOLVED RELATIVE TO THE DEFINED GRID
C           IN THE DIRECTION OF INCREASING X AND Y COORDINATES
C           OR RESOLVED RELATIVE TO EASTERLY AND NORTHERLY DIRECTIONS,
C           AS DESIGNATED BY THEIR RESPECTIVE GRID DESCRIPTION SECTIONS.
C           AS AN ADDED BONUS THE NUMBER OF OUTPUT GRID POINTS
C           AND THEIR LATITUDES AND LONGITUDES ARE ALSO RETURNED
C           ALONG WITH THEIR VECTOR ROTATION PARAMETERS.
C           ON THE OTHER HAND, THE OUTPUT CAN BE A SET OF STATION POINTS
C           IF KGDSO(1)<0, IN WHICH CASE THE NUMBER OF POINTS
C           AND THEIR LATITUDES AND LONGITUDES MUST BE INPUT 
C           ALONG WITH THEIR VECTOR ROTATION PARAMETERS.
C
C           BUDGET INTERPOLATION WEIGHTS ARE COMPUTED USING A 5x5
C           SUBGRID.  INTENDED FOR NEAREST NEIGHBOR TYPE INTERPOLATION.
C        
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C   97-12-02  BALDWIN - MODIFIED POLATEV0 FOR USE IN PRODUCT GENERATOR
C
C USAGE:    CALL POLATWGT(KGDSI,KGDSO,MI,MO,
C    &                    N11,N21,N12,N22,NPP,NV11,NV21,NV12,NV22,
C    &                    W11,W21,W12,W22,WV11,WV21,WV12,WV22,
C    &                    C11,C21,C12,C22,S11,S21,S12,S22,
C    &                    NO,RLAT,RLON,CROT,SROT,IRET)
C
C   INPUT ARGUMENT LIST:
C     KGDSI    - INTEGER (200) INPUT GDS PARAMETERS AS DECODED BY W3FI63
C     KGDSO    - INTEGER (200) OUTPUT GDS PARAMETERS
C                (KGDSO(1)<0 IMPLIES RANDOM STATION POINTS)
C     MI       - DIMENSION OF INPUT GRID FIELDS
C     MO       - DIMENSION OF OUTPUT GRID FIELDS
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)<0)
C     RLAT     - REAL (NO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)<0)
C     RLON     - REAL (NO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)<0)
C     CROT     - REAL (NO) VECTOR ROTATION COSINES (IF KGDSO(1)<0)
C     SROT     - REAL (NO) VECTOR ROTATION SINES (IF KGDSO(1)<0)
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C
C   OUTPUT ARGUMENT LIST:
C     N11,N21,N12,N22 - INTEGER (MO) SURROUNDING POINTS ON INPUT GRID
C     NV11,NV21,NV12,NV22 - INTEGER (MO) SURROUNDING V-POINTS ON INPUT GRID
C                 (SAME AS N11... EXCEPT FOR TYPES 201,203)
C     NPP      - INTEGER (MO,25) NEAREST NEIGHBOR OF 5x5 SUB-GRID
C                 FOR PRECIP (BUDGET) TYPE INTERPOLATION
C     W11,W21,W12,W22 - REAL (MO) INTERPOLATION WEIGHTS OF SURROUNDING
C                POINTS
C     WV11,WV21,WV12,WV22 - REAL (MO) INTERPOLATION WEIGHTS OF SURROUNDING
C                V-POINTS   (SAME AS W11... EXCEPT FOR TYPES 201,203)
C     C11,C21,C12,C22 - REAL (MO) VECTOR ROTATION COSINES
C     S11,S21,S12,S22 - REAL (MO) VECTOR ROTATION SINES
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)>=0)
C     RLAT     - REAL (MO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)>=0)
C     RLON     - REAL (MO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)>=0)
C     CROT     - REAL (NO) VECTOR ROTATION COSINES (IF KGDSO(1)>=0)
C     SROT     - REAL (NO) VECTOR ROTATION SINES (IF KGDSO(1)>=0)
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C     IRET     - INTEGER RETURN CODE
C                0    SUCCESSFUL INTERPOLATION WEIGHT CALC
C                2    UNRECOGNIZED INPUT GRID OR NO GRID OVERLAP
C                3    UNRECOGNIZED OUTPUT GRID
C
C SUBPROGRAMS CALLED:
C   GDSWIZ       GRID DESCRIPTION SECTION WIZARD
C   (IJKGDS)     RETURN FIELD POSITION FOR A GIVEN GRID POINT
C   (MOVECT)     MOVE A VECTOR ALONG A GREAT CIRCLE
C   POLFIXV      MAKE MULTIPLE POLE VECTOR VALUES CONSISTENT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
C
C   USE THESE INTERPOLATION WEIGHTS AS FOLLOWS:
C
C              U11=C11(I)*UIN(N11(I))-S11(I)*VIN(N11(I))
C              V11=S11(I)*UIN(N11(I))+C11(I)*VIN(N11(I))
C              U21=C21(I)*UIN(N21(I))-S21(I)*VIN(N21(I))
C              V21=S21(I)*UIN(N21(I))+C21(I)*VIN(N21(I))
C              U12=C12(I)*UIN(N12(I))-S12(I)*VIN(N12(I))
C              V12=S12(I)*UIN(N12(I))+C12(I)*VIN(N12(I))
C              U22=C22(I)*UIN(N22(I))-S22(I)*VIN(N22(I))
C              V22=S22(I)*UIN(N22(I))+C22(I)*VIN(N22(I))
C              UOUT(I)=W11(I)*U11+W21(I)*U21+
C     &                W12(I)*U12+W22(I)*U22
C              VOUT(I)=W11(I)*V11+W21(I)*V21+
C     &                W12(I)*V12+W22(I)*V22
C
C              U11=C11(I)*UIN(NV11(I))-S11(I)*VIN(NV11(I))
C              V11=S11(I)*UIN(NV11(I))+C11(I)*VIN(NV11(I))
C              U21=C21(I)*UIN(NV21(I))-S21(I)*VIN(NV21(I))
C              V21=S21(I)*UIN(NV21(I))+C21(I)*VIN(NV21(I))
C              U12=C12(I)*UIN(NV12(I))-S12(I)*VIN(NV12(I))
C              V12=S12(I)*UIN(NV12(I))+C12(I)*VIN(NV12(I))
C              U22=C22(I)*UIN(NV22(I))-S22(I)*VIN(NV22(I))
C              V22=S22(I)*UIN(NV22(I))+C22(I)*VIN(NV22(I))
C              UOUT(I)=WV11(I)*U11+WV21(I)*U21+
C     &                WV12(I)*U12+WV22(I)*U22
C              VOUT(I)=WV11(I)*V11+WV21(I)*V21+
C     &                WV12(I)*V12+WV22(I)*V22
C
C              UROT=CROT(I)*UOUT(I)-SROT(I)*VOUT(I)
C
C              UOUT(I)=UROT
C
CFPP$ EXPAND(IJKGDS,MOVECT)
      INTEGER KGDSI(200),KGDSO(200)
      REAL RLAT(MO),RLON(MO)
      REAL CROT(MO),SROT(MO)
      REAL XPTS(MO),YPTS(MO)
      REAL XPTI(MI),YPTI(MI),RLOI(MI),RLAI(MI),CROI(MI),SROI(MI)
      REAL XPTB(MO),YPTB(MO),RLOB(MO),RLAB(MO)
      REAL WO(MO)
      INTEGER N11(MO),N21(MO),N12(MO),N22(MO),NPP(MO,25)
      INTEGER NV11(MO),NV21(MO),NV12(MO),NV22(MO)
      REAL W11(MO),W21(MO),W12(MO),W22(MO)
      REAL WV11(MO),WV21(MO),WV12(MO),WV22(MO)
      REAL C11(MO),C21(MO),C12(MO),C22(MO)
      REAL S11(MO),S21(MO),S12(MO),S22(MO)
      REAL WLARGE,WVLARGE
      PARAMETER(FILL=-9999.)

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE NUMBER OF OUTPUT POINTS AND THEIR LATITUDES AND LONGITUDES.
      IRET=0

      IF(KGDSO(1).GE.0) THEN
        CALL GDSWIZ(KGDSO, 0,MO,FILL,XPTS,YPTS,RLON,RLAT,NO,1,CROT,SROT)
        IF(NO.EQ.0) IRET=3
      ENDIF
      CALL GDSWIZ(KGDSI, 0,MI,FILL,XPTI,YPTI,RLOI,RLAI,NV,1,CROI,SROI)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  LOCATE INPUT POINTS AND COMPUTE THEIR WEIGHTS AND ROTATIONS
      CALL GDSWIZ(KGDSI,-1,NO,FILL,XPTS,YPTS,RLON,RLAT,NV,0,DUM,DUM)
      IF(IRET.EQ.0.AND.NV.EQ.0) IRET=2
      DO N=1,NO
        XI=XPTS(N)
        YI=YPTS(N)
        IF(XI.NE.FILL.AND.YI.NE.FILL) THEN
          I1=XI
          I2=I1+1
          J1=YI
          J2=J1+1
          XF=XI-I1
          YF=YI-J1
          N11(N)=IJKGDS(I1,J1,KGDSI)
          N21(N)=IJKGDS(I2,J1,KGDSI)
          N12(N)=IJKGDS(I1,J2,KGDSI)
          N22(N)=IJKGDS(I2,J2,KGDSI)
          IF(MIN(N11(N),N21(N),N12(N),N22(N)).GT.0) THEN
            W11(N)=(1-XF)*(1-YF)
            W21(N)=XF*(1-YF)
            W12(N)=(1-XF)*YF
            W22(N)=XF*YF
C
C NEAREST NEIGHBOR CHECK
C
            WLARGE=AMAX1(W11(N),W21(N),W12(N),W22(N))
c            print *,'wlarge ',n,W11(N),W21(N),W12(N),W22(N),wlarge
            IF(WLARGE.eq.W11(N)) THEN
c             print *,'w11 ',n,W11(N),W21(N),W12(N),W22(N),wlarge
              W11(N)=1.0
              W21(N)=0.0
              W12(N)=0.0
              W22(N)=0.0
              CALL MOVECT(RLAI(N11(N)),RLOI(N11(N)),RLAT(N),RLON(N),
     &                  CM11,SM11)
              C11(N)=CM11*CROI(N11(N))+SM11*SROI(N11(N))
              S11(N)=SM11*CROI(N11(N))-CM11*SROI(N11(N))
              C21(N)=0.0
              S21(N)=0.0
              C12(N)=0.0
              S12(N)=0.0
              C22(N)=0.0
              S22(N)=0.0
            ENDIF
            IF(WLARGE.eq.W21(N)) THEN
c             print *,'w21 ',n,W11(N),W21(N),W12(N),W22(N),wlarge
              W11(N)=0.0
              W21(N)=1.0
              W12(N)=0.0
              W22(N)=0.0
              CALL MOVECT(RLAI(N21(N)),RLOI(N21(N)),RLAT(N),RLON(N),
     &                  CM21,SM21)
              C21(N)=CM21*CROI(N21(N))+SM21*SROI(N21(N))
              S21(N)=SM21*CROI(N21(N))-CM21*SROI(N21(N))
              C11(N)=0.0
              S11(N)=0.0
              C12(N)=0.0
              S12(N)=0.0
              C22(N)=0.0
              S22(N)=0.0
            ENDIF
            IF(WLARGE.eq.W12(N)) THEN
c             print *,'w12 ',n,W11(N),W21(N),W12(N),W22(N),wlarge
              W11(N)=0.0
              W21(N)=0.0
              W12(N)=1.0
              W22(N)=0.0
              CALL MOVECT(RLAI(N12(N)),RLOI(N12(N)),RLAT(N),RLON(N),
     &                  CM12,SM12)
              C12(N)=CM12*CROI(N12(N))+SM12*SROI(N12(N))
              S12(N)=SM12*CROI(N12(N))-CM12*SROI(N12(N))
              C11(N)=0.0
              S11(N)=0.0
              C21(N)=0.0
              S21(N)=0.0
              C22(N)=0.0
              S22(N)=0.0
            ENDIF
            IF(WLARGE.eq.W22(N)) THEN
c             print *,'w22 ',n,W11(N),W21(N),W12(N),W22(N),wlarge
              W11(N)=0.0
              W21(N)=0.0
              W12(N)=0.0
              W22(N)=1.0
              CALL MOVECT(RLAI(N22(N)),RLOI(N22(N)),RLAT(N),RLON(N),
     &                  CM22,SM22)
              C22(N)=CM22*CROI(N22(N))+SM22*SROI(N22(N))
              S22(N)=SM22*CROI(N22(N))-CM22*SROI(N22(N))
              C11(N)=0.0
              S11(N)=0.0
              C21(N)=0.0
              S21(N)=0.0
              C12(N)=0.0
              S12(N)=0.0
            ENDIF
C
          ELSE
            N11(N)=0
            N21(N)=0
            N12(N)=0
            N22(N)=0
          ENDIF
        ELSE
          N11(N)=0
          N21(N)=0
          N12(N)=0
          N22(N)=0
        ENDIF
      ENDDO
C
C  V-POINTS
C    SAME WGTS, ETC. AS MASS POINTS IF NOT 201,203
C
      IF(KGDSI(1).NE.201.AND.KGDSI(1).NE.203) THEN

        NV11=N11
        NV12=N12
        NV21=N21
        NV22=N22
        WV11=W11
        WV12=W12
        WV21=W21
        WV22=W22

      ELSE

      IF(KGDSO(1).GE.0) THEN
        CALL GDSWIZ(KGDSO, 0,MO,FILL,XPTS,YPTS,RLON,RLAT,NO,1,CROT,SROT)
        IF(NO.EQ.0) IRET=3
      ENDIF
C   TELL GDSWIZ THAT THIS IS A V-POINT FIELD
        KGDSI11=KGDSI(11)
        KGDSI(11)=IOR(KGDSI(11),256)
      CALL GDSWIZ(KGDSI, 0,MI,FILL,XPTI,YPTI,RLOI,RLAI,NV,1,CROI,SROI)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  LOCATE INPUT POINTS AND COMPUTE THEIR WEIGHTS AND ROTATIONS
      CALL GDSWIZ(KGDSI,-1,NO,FILL,XPTS,YPTS,RLON,RLAT,NV,0,DUM,DUM)
      IF(IRET.EQ.0.AND.NV.EQ.0) IRET=2
      DO N=1,NO
        XI=XPTS(N)
        YI=YPTS(N)
        IF(XI.NE.FILL.AND.YI.NE.FILL) THEN
          I1=XI
          I2=I1+1
          J1=YI
          J2=J1+1
          XF=XI-I1
          YF=YI-J1
          NV11(N)=IJKGDS(I1,J1,KGDSI)
          NV21(N)=IJKGDS(I2,J1,KGDSI)
          NV12(N)=IJKGDS(I1,J2,KGDSI)
          NV22(N)=IJKGDS(I2,J2,KGDSI)
          IF(MIN(NV11(N),NV21(N),NV12(N),NV22(N)).GT.0) THEN
            WV11(N)=(1-XF)*(1-YF)
            WV21(N)=XF*(1-YF)
            WV12(N)=(1-XF)*YF
            WV22(N)=XF*YF
C
C NEAREST NEIGHBOR CHECK
C
            WVLARGE=AMAX1(WV11(N),WV21(N),WV12(N),WV22(N))
            IF(WVLARGE.eq.WV11(N)) THEN
              WV11(N)=1.0
              WV21(N)=0.0
              WV12(N)=0.0
              WV22(N)=0.0
              CALL MOVECT(RLAI(NV11(N)),RLOI(NV11(N)),RLAT(N),RLON(N),
     &                  CM11,SM11)
              C11(N)=CM11*CROI(NV11(N))+SM11*SROI(NV11(N))
              S11(N)=SM11*CROI(NV11(N))-CM11*SROI(NV11(N))
              C21(N)=0.0
              S21(N)=0.0
              C12(N)=0.0
              S12(N)=0.0
              C22(N)=0.0
              S22(N)=0.0
            ENDIF
            IF(WVLARGE.eq.WV21(N)) THEN
              WV11(N)=0.0
              WV21(N)=1.0
              WV12(N)=0.0
              WV22(N)=0.0
              CALL MOVECT(RLAI(NV21(N)),RLOI(NV21(N)),RLAT(N),RLON(N),
     &                  CM21,SM21)
              C11(N)=0.0
              S11(N)=0.0
              C12(N)=0.0
              S12(N)=0.0
              C22(N)=0.0
              S22(N)=0.0
            ENDIF
            IF(WVLARGE.eq.WV12(N)) THEN
              WV11(N)=0.0
              WV21(N)=0.0
              WV12(N)=1.0
              WV22(N)=0.0
              CALL MOVECT(RLAI(NV12(N)),RLOI(NV12(N)),RLAT(N),RLON(N),
     &                  CM12,SM12)
              C12(N)=CM12*CROI(NV12(N))+SM12*SROI(NV12(N))
              S12(N)=SM12*CROI(NV12(N))-CM12*SROI(NV12(N))
              C11(N)=0.0
              S11(N)=0.0
              C21(N)=0.0
              S21(N)=0.0
              C22(N)=0.0
              S22(N)=0.0
            ENDIF
            IF(WVLARGE.eq.WV22(N)) THEN
              WV11(N)=0.0
              WV21(N)=0.0
              WV12(N)=0.0
              WV22(N)=1.0
              CALL MOVECT(RLAI(NV22(N)),RLOI(NV22(N)),RLAT(N),RLON(N),
     &                  CM22,SM22)
              C22(N)=CM22*CROI(NV22(N))+SM22*SROI(NV22(N))
              S22(N)=SM22*CROI(NV22(N))-CM22*SROI(NV22(N))
              C11(N)=0.0
              S11(N)=0.0
              C21(N)=0.0
              S21(N)=0.0
              C12(N)=0.0
              S12(N)=0.0
            ENDIF
C
          ELSE
            NV11(N)=0
            NV21(N)=0
            NV12(N)=0
            NV22(N)=0
          ENDIF
        ELSE
          NV11(N)=0
          NV21(N)=0
          NV12(N)=0
          NV22(N)=0
        ENDIF
      ENDDO
        KGDSI(11)=KGDSI11
      ENDIF

C
C   DO PRECIP INTERP
C

      IF(KGDSO(1).GE.0) THEN
        CALL GDSWIZ(KGDSO, 0,MO,FILL,XPTS,YPTS,RLON,RLAT,NO,0,DUM,DUM)
        IF(NO.EQ.0) IRET=3
      ENDIF

C  LOOP OVER SAMPLE POINTS IN OUTPUT GRID BOX
      DO NB=1,25
C  LOCATE INPUT POINTS AND COMPUTE THEIR WEIGHTS
        JB=(NB-1)/5-2
        IB=NB-(JB+2)*5-3
        LB=MAX(ABS(IB),ABS(JB))
          DO N=1,NO
            XPTB(N)=XPTS(N)+IB/5.
            YPTB(N)=YPTS(N)+JB/5.
          ENDDO
          CALL GDSWIZ(KGDSO, 1,NO,FILL,XPTB,YPTB,RLOB,RLAB,NV,0,DUM,DUM)
          CALL GDSWIZ(KGDSI,-1,NO,FILL,XPTB,YPTB,RLOB,RLAB,NV,0,DUM,DUM)
C   INPUT ARGUMENT LIST:
C     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
C     IOPT     - INTEGER OPTION FLAG
C                ( 0 TO COMPUTE EARTH COORDS OF ALL THE GRID POINTS)
C                (+1 TO COMPUTE EARTH COORDS OF SELECTED GRID COORDS)
C                (-1 TO COMPUTE GRID COORDS OF SELECTED EARTH COORDS)
C     NPTS     - INTEGER MAXIMUM NUMBER OF COORDINATES
C     FILL     - REAL FILL VALUE TO SET INVALID OUTPUT DATA
C                (MUST BE IMPOSSIBLE VALUE; SUGGESTED VALUE: -9999.)
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT>0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT>0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT<0
C                (ACCEPTABLE RANGE: -360. TO 360.)
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT<0
C                (ACCEPTABLE RANGE: -90. TO 90.)
C     LROT     - INTEGER FLAG TO RETURN VECTOR ROTATIONS IF 1
C
C   OUTPUT ARGUMENT LIST:
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT<=0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT<=0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT>=0
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT>=0
C     NRET     - INTEGER NUMBER OF VALID POINTS COMPUTED
C                (-1 IF PROJECTION UNRECOGNIZED)
C     CROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION COSINES IF LROT=1
C     SROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION SINES IF LROT=1
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
          IF(IRET.EQ.0.AND.NV.EQ.0.AND.LB.EQ.0) IRET=2
          DO N=1,NO
            XI=XPTB(N)
            YI=YPTB(N)
            IF(XI.NE.FILL.AND.YI.NE.FILL) THEN
              I1=NINT(XI)
              J1=NINT(YI)
              NPP(N,NB)=IJKGDS(I1,J1,KGDSI)
            ELSE
              NPP(N,NB)=0
            ENDIF
          ENDDO
      ENDDO
      RETURN
      END
