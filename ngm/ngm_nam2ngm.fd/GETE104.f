c     PROGRAM GETE104
      SUBROUTINE GETE104(LUGB,LUGI,JNCDAT,ISTAT)
C                .      .    .                                       .
C SUBPROGRAM:    GETE104     RETRIEVE ETA MODEL ANALYSIS
C   PRGMMR: ROGERS           ORG: W/NP22     DATE: 98-11-05
C
C ABSTRACT: RETRIEVES THE ON-TIME ETA ANALYSIS (POSTED ON THE NGM
C           SUPER C-GRID #104)
C
C PROGRAM HISTORY LOG:
C   98-11-05  Eric Rogers  
C
C USAGE:    CALL GETE104( LUGB, LUGI, JNCDAT, ISTAT)
C
C   INPUT FILES:
C     LUGB     - INTEGER unit for GRIB data file
C     LUGI     - INTEGER unit for GRIB Index file
C
C   OUTPUT ARGUMENT LIST:
C     JNCDAT   - DATE OF ETA ANALYSIS
C     ISTAT    - INTEGER =0  MEANS SUCCESSFUL COMPLETION
C                        =-1 MEANS GRID COULD NOT BE RETURNED
C   OUTPUT VIA COMMON BLOCK /GRID /
C     COMMON /EGRID /PS(ILIM,JLIM),ZS(ILIM,JLIM)
C    1 Z(ILIM,JLIM,MAXLEV), 
C    2 T(ILIM,JLIM,MAXLEV), 
C    3 U(ILIM,JLIM,MAXLEV), 
C    4 V(ILIM,JLIM,MAXLEV), 
C    5 Q(ILIM,JLIM,MAXLEV), MASKE(ILIM,JLIM),
C
C             REAL & INTEGER ARRAYS dimensioned (ILIM,JLIM,MAXLEV) 
C             Containing 3-D fields on arrays of (IMAX,JMAX,NUMLEV)
C             And the MASKE INTEGER ARRAY(IMAX,JMAX)
C             MASKE=0 INDICATES A GRIDPOINT WITH MISSING DATA 
C                PRESENT CONTENTS:
C                PS  SURFACE PRESSURE FROM GDAS GUESS (PA) 
C                ZS  SURFACE HEIGHT (K) 
C                Z   GEOPOTENTIAL HEIGHT (M)
C                T   TEMPERATURE (K)
C                U   WIND U-COMPONENT (M/S)
C                V   WIND V-COMPONENT (M/S)
C                Q   Specific HUMIDITY (G/G)
C
C REMARKS:

C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  CRAY C-90
C$$$
      INCLUDE "parmg104"
      PARAMETER(ITOT=ILIM*JLIM)
      COMMON /GBLDAT/ IAODAT(8)
      COMMON /GRIDEF/ IMAX,JMAX,KMAX,ALAT1,ELON1,DXX,DYY,ELONV,
     1    ALATAN
C
      COMMON /EGRID /PS(ILIM,JLIM),ZS(ILIM,JLIM),
     1 Z(ILIM,JLIM,MAXLEV),TS(ILIM,JLIM),
     2 T(ILIM,JLIM,MAXLEV),QS(ILIM,JLIM),
     3 U(ILIM,JLIM,MAXLEV),
     4 V(ILIM,JLIM,MAXLEV),
     5 Q(ILIM,JLIM,MAXLEV),MASKE(ILIM,JLIM),
     6 MASKV(ILIM,JLIM)
C
      COMMON /IPOINT/ IPT,JPT
C
      DIMENSION GRID(ITOT),DIFF(5)
      DIMENSION RINC(5),INCDAT(8),JNCDAT(8)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER LEVS(MAXLEV),IVAR(5)
      LOGICAL*1 MASK(ITOT)
C
      DATA LEVS/1000,975,950,925,900,875,850,825,800,775,750,725,
     X           700,675,650,625,600,575,550,525,500,475,450,425,
     X           400,375,350,325,300,275,250,225,200,175,150,125,
     X           100,75,50/
C
      DATA IVAR/007,011,033,034,051/
C
      PARAMETER(MBUF=2000000,JF=1000000)
      CHARACTER CBUF(MBUF)
      LOGICAL*1 LB(JF)
      REAL F(JF)
      PARAMETER(MSK1=32000,MSK2=4000)
      INTEGER JENS(200),KENS(200)
C
      NUMLEV=MAXLEV
C
      IPT=67
      JPT=16
C
      JJ1 = 1
      JJINC = 1
C
      DO K = 1, 8
        INCDAT(K) = 0
        JNCDAT(K) = 0
        IF(K .LE. 5) THEN
          RINC(K) = 0.0
        ENDIF
      ENDDO
C
      ISTAT = 0
C
C  READ INDEX FILE TO GET GRID SPECS 
C
      IRGI = 1
      IRGS = 1
      KMAX = 0
      JR=0
      KSKIP = 0
      CALL BAOPEN(13,'fort.13',IRETGB)
      CALL BAOPEN(14,'fort.14',IRETGI)
      print *,'after baopen ',iretgb, iretgi
      CALL GETGI(LUGI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
      write(6,*)' IRET FROM GETGI ',IRGI
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
        RETURN
      ENDIF 
      REWIND LUGI
C
      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        write(6,*)' IRET FROM GETGB1S ',IRGS
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          ISTAT = IRGS
          RETURN
        ENDIF 
C
C    USE  F I R S T  PDS TO GET START DATE & FORECAST LENGTH
C
        IF(K.EQ.1) THEN
          MODGEN = KPDS(2)
          IYS = KPDS(8)
          INCDAT(2) = KPDS(9)
          INCDAT(3) = KPDS(10)
          INCDAT(5) = KPDS(11)
          RINC(2) = FLOAT(KPDS(14))
          IGCENT = KPDS(21)
          IF(IYS .EQ. 100) THEN
            INCDAT(1) = IGCENT * 100
          ELSE
            INCDAT(1) = (IGCENT - 1) * 100 + IYS
          ENDIF
        ENDIF

        IF(KPDS(3).EQ.104) THEN
          WRITE(6,20) (KGDS(KER),KER=1,14)
          WRITE(6,10) (KPDS(KER),KER=1,22)
10        FORMAT(22I8)
20        FORMAT(14I8)
          GO TO 1001
        ENDIF
      ENDDO
1001  CONTINUE
C
C    FILL IN GRIDEF COMMON BLOCK
C    THE FOLLOWING DEFINED REGARDLESS OF GRID PROJECTION
C
      IMAX = KGDS(2)
      JMAX = KGDS(3)
C
      ALAT1 = KGDS(4) * 0.001
      ELON1 = KGDS(5) * 0.001 + 360.0
      ELONV = KGDS(7) * 0.001 + 360.0
      ALATAN = 0.0
      DXX = KGDS(8) * 0.001
      DYY = KGDS(9) * 0.001
C
      print *,'gridspecs ',ALAT1,ELON1,ELONV,ALATAN,DXX,DYY
C
C    ADD RINC FORECAST HOURS TO START DATE TO GET VALID DATE
C    IN CASE WE HAVE TO USE A ETA FORECAST TO START THE NGM
C    USE W3MOVDAT
C   
      CALL W3MOVDAT(RINC,INCDAT,JNCDAT)
      WRITE(6,*)' INPUT DATE FROM ETA ',INCDAT,RINC
      WRITE(6,*)' VALID DATE FROM ETA ',JNCDAT
C
C    USE W3DIFDAT TO ENSURE THAT BACKGROUND FROM GDAS HAS
C    SAME VALID DATE AS THE ETA ANALYSIS
C
      CALL W3DIFDAT(JNCDAT,IAODAT,2,DIFF)
      IF(DIFF(2) .EQ. 0.0) THEN
       WRITE(6,*)' GDAS AND ETA VALID DATES MATCH SO CONTINUE '
      ELSE
       WRITE(6,*)' VALID DATE FROM ETA ',JNCDAT
       WRITE(6,*)' VALID DATE FROM GDAS ',IAODAT
       WRITE(6,*)' GDAS AND ETA VALID DATES DO NOT MATCH SO ABORT '
       ISTAT = -1
       RETURN
      ENDIF
C
C    GET GRID NUMBER FROM PDS
C
      IGDNUM = KPDS(3)
C
C   PROCESS THE GRIB FILE
C
      NUMVAL = IMAX*JMAX
      KMAX = MAXLEV
C
C   INITIALIZE MASK ARRAY
C
      DO J = 1, JLIM
       DO I = 1, ILIM
        MASKE(I,J) = 0
        MASKV(I,J) = 0
       ENDDO
      ENDDO
C
      WRITE(6,280) IMAX,JMAX,NUMLEV,KMAX
  280 FORMAT(' IMAX,JMAX,NUMLEV,KMAX ',5I4)
  285 FORMAT(' IV, IVAR, L, IRET:  ',4I5)

C -== GET SURFACE FIELDS ==-
      L = 0
      IV= 0
C   SURFACE PRESSURE (MB)

c   to start each new file with its index, set J=-1 for sfc pressure
C
C   USE THIS FIELD TO SET UP MASK ARRAY
C
      J = -1
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 001
      JPDS(6) = 001
      JPDS(13) = 1
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        II = 1
        JJ = JJ1
        DO KK = 1, ITOT
          PS(II,JJ) = GRID(KK) * 0.01
          IF(MASK(KK)) THEN
            MASKE(II,JJ) = 1
          ENDIF
c         print *,ii,jj,maske(ii,jj),ps(ii,jj)
          IPREV=II
          II = II + 1
          IF(MOD(IPREV,IMAX).EQ.0) THEN
            II = 1
            JJ = JJ + JJINC
            IF(JJ.GT.JMAX.OR.JJ.LT.1) GO TO 110
          ENDIF
        ENDDO
  110 CONTINUE
      ELSE
        WRITE(6,285)IV,JPDS(5),L,IRET
        WRITE(6,*)' COULD NOT UNPACK GRID 104 FILE FROM
     1   ETA RUN SO ABORT '
         ISTAT = IRET
        RETURN
      ENDIF

C   SURFACE GEOPOTENTIAL HEIGHT (M)

      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 007
      JPDS(6) = 001
      JPDS(13) = 1
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        II = 1
        JJ = JJ1
        DO KK = 1, ITOT
          ZS(II,JJ) = GRID(KK)
          IPREV=II
          II = II + 1
          IF(MOD(IPREV,IMAX).EQ.0) THEN
            II = 1
            JJ = JJ + JJINC
            IF(JJ.GT.JMAX.OR.JJ.LT.1) GO TO 120
          ENDIF
        ENDDO
  120 CONTINUE
      ELSE
        WRITE(6,285)IV,JPDS(5),L,IRET
        WRITE(6,*)' COULD NOT UNPACK GRID 104 FILE FROM
     1   ETA RUN SO ABORT '
         ISTAT = IRET
         RETURN
      ENDIF

C   2-M TEMPERATURE (K)

      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 011
      JPDS(6) = 105
      JPDS(7) = 2
      JPDS(13) = 1
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        II = 1
        JJ = JJ1
        DO KK = 1, ITOT
          TS(II,JJ) = GRID(KK)
          IPREV=II
          II = II + 1
          IF(MOD(IPREV,IMAX).EQ.0) THEN
            II = 1
            JJ = JJ + JJINC
            IF(JJ.GT.JMAX.OR.JJ.LT.1) GO TO 130
          ENDIF
        ENDDO
  130 CONTINUE
      ELSE
        WRITE(6,285)IV,JPDS(5),L,IRET
        WRITE(6,*)' COULD NOT UNPACK GRID 104 FILE FROM
     1   ETA RUN SO ABORT '
         ISTAT = IRET
         RETURN
      ENDIF

C   2-M SPECIFIC HUMIDITY 

      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 051
      JPDS(6) = 105
      JPDS(7) = 2
      JPDS(13) = 1
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        II = 1
        JJ = JJ1
        DO KK = 1, ITOT
          QS(II,JJ) = GRID(KK)
          IPREV=II
          II = II + 1
          IF(MOD(IPREV,IMAX).EQ.0) THEN
            II = 1
            JJ = JJ + JJINC
            IF(JJ.GT.JMAX.OR.JJ.LT.1) GO TO 140
          ENDIF
        ENDDO
  140 CONTINUE
      ELSE
        WRITE(6,285)IV,JPDS(5),L,IRET
        WRITE(6,*)' COULD NOT UNPACK GRID 104 FILE FROM
     1   ETA RUN SO ABORT '
         ISTAT = IRET
         RETURN
      ENDIF

C  -== GET PRESSURE LEVEL VARIABLES Z, T, U, V & Q/RH ==-

      DO 200 IV = 1, 5
        J = 0
        JPDS = -1
        JPDS(3) = IGDNUM
        JPDS(5) = IVAR(IV)
        JPDS(6) = 100
        JPDS(13) = 1
        DO 100 L = 1,NUMLEV
          JPDS(7) = LEVS(L)
          CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X               KPDS,KGDS,MASK,GRID,IRET)
          IF(IRET.EQ.0) THEN
            II = 1
            JJ = JJ1
            DO KK = 1, ITOT
              IF(IV.EQ.1) THEN
                Z(II,JJ,L) = GRID(KK)
              ELSEIF(IV.EQ.2) THEN
                T(II,JJ,L) = GRID(KK)
              ELSEIF(IV.EQ.3) THEN
                U(II,JJ,L) = GRID(KK)
                IF(MASK(KK)) THEN
                  MASKV(II,JJ) = 1
                ENDIF
              ELSEIF(IV.EQ.4) THEN
                V(II,JJ,L) = GRID(KK)
              ELSEIF(IV.EQ.5) THEN
                Q(II,JJ,L) = GRID(KK)
              ENDIF
              IPREV=II
              II = II + 1
              IF(MOD(IPREV,IMAX).EQ.0) THEN
                II = 1
                JJ = JJ + JJINC
                IF(JJ.GT.JMAX.OR.JJ.LT.1) GO TO 100
              ENDIF
            ENDDO
          ELSE
            WRITE(6,285) IV,JPDS(5),L,IRET
            WRITE(6,*)' COULD NOT UNPACK GRID 104 FILE FROM
     1        ETA RUN SO ABORT '
            ISTAT = IRET
            RETURN
          ENDIF
  100   CONTINUE
  200 CONTINUE
C
C  CONVERT SENSIBLE TEMP TO VIRTUAL TEMP
C
      DO K = 1, MAXLEV
       DO J = 1, JLIM
        DO I = 1, ILIM
         IF(MASKE(I,J).EQ.1) THEN
          TV = T(I,J,K)*(1.0+(0.61*Q(I,J,K)))
          T(I,J,K) = TV
          IF(K.EQ.1) THEN
           TVS = TS(I,J)*(1.0+(0.61*QS(I,J)))
           TS(I,J) = TVS
          ENDIF 
         ENDIF 
        ENDDO
       ENDDO
      ENDDO

C  PRINT VALUES AT POINT IN MIDDLE OF GRID
         ier=ipt
         jer=jpt
          write(6,1234)ier,jer,ps(ier,jer),zs(ier,jer),
     1      ts(ier,jer),qs(ier,jer)
1234      format(2i4,3x,4(f7.1,1x))
           DO LER = 1, NUMLEV
            write(6,1235)ler,levs(ler),
     1       z(ier,jer,ler),t(ier,jer,ler),
     2       u(ier,jer,ler),v(ier,jer,ler),
     3       q(ier,jer,ler)
1235        format(1x,i2,2x,i5,2x,f7.1,1x,3f8.2,2x,f8.6)
           enddo
c      DO J = 1, JLIM
c       DO I = 1, ILIM
c         IF(MASKE(I,J).EQ.0) print *,'maske=0 at ',i,j
c         IF(MASKV(I,J).EQ.0) print *,'maskv=0 at ',i,j
c       ENDDO
c      ENDDO
C
      RETURN
      END
