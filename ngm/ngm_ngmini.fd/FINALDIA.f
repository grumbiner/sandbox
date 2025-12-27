       SUBROUTINE FINALDIA(F,FNEW,M,N,NG,K,Z)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FINALDIA    PRINTS DIAGNOSTICS FOR CHANGE BY INIT.
C   PRGMMR: PARRISH          ORG: W/NMC22    DATE: 88-08-08
C
C ABSTRACT: PRINT DIAGNOSTICS FOR DIFF BETWEEN VBL AND VBLCOPY.
C
C PROGRAM HISTORY LOG:
C   88-08-08  PARRISH
C
C USAGE:    CALL FINALDIA(F,FNEW,M,N,NG,K,Z)
C   INPUT ARGUMENT LIST:
C     F        - OLD ARRAY
C     FNEW     - NEW ARRAY
C     M        - X-DIMENSION
C     N        - Y-DIMENSION
C     NG       - GRID NUMBER FOR VARIABLES TO MOVE.
C     K        - VERTICAL MODE NUMBER
C     Z        - TERRAIN
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN200
C   MACHINE:  CYBER
C
C$$$
         include "myparam"
C--------
         REAL F(1),FNEW(1),Z(1)
         CHARACTER*4 LABEL(65)
         data LABEL/'HU1','HU2','HU3','HU4','HU5','HU6',
     *    'HU7','HU8','HU9','HU10','HU11','HU12','HU13','HU14',
     *    'HU15','HU16',
     *                         'HV1','HV2','HV3','HV4','HV5','HV6',
     *    'HV7','HV8','HV9','HV10','HV11','HV12','HV13','HV14',
     *    'HV15','HV16',
     *                         'HT1','HT2','HT3','HT4','HT5','HT6',
     *    'HT7','HT8','HT9','HT10','HT11','HT12','HT13','HT14',
     *    'HT15','HT16',
     *                         'HQ1','HQ2','HQ3','HQ4','HQ5','HQ6',
     *    'HQ7','HQ8','HQ9','HQ10','HQ11','HQ12','HQ13','HQ14',
     *    'HQ15','HQ16','H'/
         CHARACTER*6 GRID(3)
         data GRID/'A-GRID','B-GRID','C-GRID'/
C--------
         MN=M*N
         FMAX=-1.E30
         FMIN=1.E30
         FNEWMAX=-1.E30
         FNEWMIN=1.E30
         DIFRMS=0.
         DIFMEAN=0.
         DIFMAX=-1.E30
         DIFMIN=1.E30
         IDIFMAX=0
         IDIFMIN=0
         DO 50 I=1,MN
           FMAX=MAX(F(I),FMAX)
           FNEWMAX=MAX(FNEW(I),FNEWMAX)
           FMIN=MIN(F(I),FMAX)
           FNEWMIN=MIN(FNEW(I),FNEWMIN)
           DIFF=F(I)-FNEW(I)
           DIFRMS=DIFRMS+DIFF**2
           IF(DIFF.GT.DIFMAX) THEN
             DIFMAX=DIFF
             IDIFMAX=I
           END IF
           IF(DIFF.LT.DIFMIN) THEN
             DIFMIN=DIFF
             IDIFMIN=I
           END IF
           DIFMEAN=DIFMEAN+DIFF
50       CONTINUE
         DIFMEAN=DIFMEAN/MN
         DIFRMS=SQRT(DIFRMS/MN)
         JDMAX=IDIFMAX/M
         IDMAX=IDIFMAX-M*JDMAX
         JDMAX=JDMAX+1
         JDMIN=IDIFMIN/M
         IDMIN=IDIFMIN-M*JDMIN
         JDMIN=JDMIN+1
         WRITE(6,100)LABEL(K),GRID(NG),FMAX,FNEWMAX,FMIN,FNEWMIN,
     *      DIFRMS,DIFMAX,DIFMIN,DIFMEAN,IDMAX,JDMAX,IDMIN,JDMIN,
     *       Z(IDIFMAX),Z(IDIFMIN)
 100     FORMAT(1H ,A4,2X,A6,' OLD,NEW MAX=',2E12.5,' OLD,NEW MIN=',
     *      2E12.5,/,'          OLD-NEW RMS,MAX,MIN,MEAN=',4E12.5,/,
     *    '          I,JMAX=',2I5,'  I,JMIN=',2I5,
     *        '  TERRAIN AT MAX,MIN DIFF=',2E12.5)
       RETURN
       END
