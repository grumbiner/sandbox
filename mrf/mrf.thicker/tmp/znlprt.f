      SUBROUTINE ZNLPRT(NZNL,RHOUR,IDATE,KDT,NLV,ZNLM,ZNLS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ZNLPRT      PRINT ZONAL DIAGNOSTICS.
C   PRGMMR: MARK IREDELL     ORG: W/NMC23    DATE: 91-03-15
C
C ABSTRACT: PRINT ZONAL DIAGNOSTICS.
C
C PROGRAM HISTORY LOG:
C   91-03-15  MARK IREDELL
C
C USAGE:    CALL ZNLPRT (RHOUR,IDATE,KDT,NLB,NST,NLV,
C    &            NRD,NRM,NRS,CND,CNM,CNS,ISD,ISM,ISS,ZNLD,ZNLM,ZNLS)
C   INPUT ARGUMENT LIST:
C     RHOUR    - CURRENT FORECAST HOUR
C     IDATE    - INITIAL HOUR,MONTH,DAY,YEAR
C     KDT      - FORECAST STEP IF POSITIVE OR -100-DIAB.INI.STEP OR
C                -4 FOR INITIAL-DT DATA IN DIAB.INI
C                -3 FOR INITIAL    DATA IN DIAB.INI
C                -2 FOR INITIAL-DT DATA
C                -1 FOR INITIAL    DATA
C                0  FOR DATA AFTER INITIALIZATION
C     NLB      - NUMBER OF LATITUDE BANDS
C     NST      - NUMBER OF SURFACE TYPES
C     NLV      - NUMBER OF LEVELS
C     NRD      - NUMBER OF 3D DYNAMICS FIELDS
C     NRM      - NUMBER OF 3D PHYSICS FIELDS
C     NRS      - NUMBER OF 2D PHYSICS FIELDS
C     CND      - CHARACTER*8 IDENTIFICATIONS OF 3D DYNAMICS FIELDS
C     CNM      - CHARACTER*8 IDENTIFICATIONS OF 3D PHYSICS FIELDS
C     CNS      - CHARACTER*8 IDENTIFICATIONS OF 2D PHYSICS FIELDS
C     ISD      - STATUS FLAGS OF 3D DYNAMICS FIELDS
C     ISM      - STATUS FLAGS OF 3D PHYSICS FIELDS
C     ISS      - STATUS FLAGS OF 2D PHYSICS FIELDS
C     ZNLD     - ZONAL DIAGNOSTIC ARRAY OF 3D DYNAMICS FIELDS
C     ZNLM     - ZONAL DIAGNOSTIC ARRAY OF 3D PHYSICS FIELDS
C     ZNLS     - ZONAL DIAGNOSTIC ARRAY OF 2D PHYSICS FIELDS
C
C   SUBPROGRAMS CALLED:
C     IPWRCN   - FUNCTION TO DETERMINE ORDER OF MAGNITUDE OF OUTPUT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77.
C   MACHINE:  CRAY YMP.
C
C$$$
      PARAMETER(NRM=23,NRS=32)
      PARAMETER(NLB=6,NST=6)
      PARAMETER(NMU=1,NMV=2,NMTV=3,NMQ=4,NMVOT2=5,
     &          NMDIV2=6,NMOMEGA=7,NMT=8,NMRH=9,NMKE=10,
     &          NMTCONV=11,NMTLARG=12,NMTSHAL=13,NMTVRDF=14,NMQCONV=15,
     &          NMQSHAL=16,NMQVRDF=17,NMUVRDF=18,NMVVRDF=19,NMTHSW=20,
     &          NMTHLW=21,NMTCLD=22,NMTCCV=23)
      PARAMETER(NSRAIN=1,NSRAINC=2,NSTSFC=3,NSQSFC=4,NSUSFC=5,
     &          NSVSFC=6,NSRCOV=7,NSRCOVC=8,NSPS=9,NSTSKIN=10,
     &          NSWET=11,NSSNOW=12,NSTG1=13,NSTG2=14,NSTG3=15,
     &          NSSFCSW=16,NSSFCLW=17,NSRHS=18,NSTVS=19,NSTS=20,
     &          NSQS=21,NSZORL=22,NSSLMSK=23,NSUGWD=24,NSVGWD=25,
     &          NSUASFC=26,NSUAGWD=27,NSUAMTN=28,NSUA=29,NSUAP=30,
     &          NSEP=31,NSCLDWRK=32)
      COMMON /COMZNL/ ZDM(2, 28 ,NRM, 47 ),ZWM(2, 47 ),ZHM(NRM)
      COMMON /COMZNL/ ZDS(2,NST,NRS, 47 ),ZWS(2,NST, 47 )
      INTEGER IDATE(4)
      CHARACTER*8 CNM,CNS
      DIMENSION CNM(NRM),CNS(NRS)
      DIMENSION ZNLM(NLB,NLV,NRM)
      DIMENSION ZNLS(NLB,NST,NRS)
      DIMENSION WRKM(NLB,NLV),WRKS(NLB,NST)
      CHARACTER*32 LABZ
      CHARACTER*8 CLB(6)
      CHARACTER*4 CST(6)
      CHARACTER*80 CFMT
      DATA CNM/'U       ','V       ','TV      ','Q       ','VOT**2  ',
     &         'DIV**2  ','OMEGA   ','T       ','RH      ','KE      ',
     &         'DTCONV  ','DTLARG  ','DTSHAL  ','DTVRDF  ','DQCONV  ',
     &         'DQSHAL  ','DQVRDF  ','DUVRDF  ','DVVRDF  ','DTHSW   ',
     &         'DTHLW   ','CLOUD   ','CVCLD   '/
      DATA CNS/'RAIN    ','RAINC   ','DTSFC   ','DQSFC   ','DUSFC   ',
     &         'DVSFC   ','RCOV    ','RCOVC   ','PS      ','TSKIN   ',
     &         'WETNESS ','SNOW    ','TG1     ','TG2     ','TG3     ',
     &         'SFCSW   ','SFCLW   ','RHS     ','TVS     ','TS      ',
     &         'QS      ','ZORL    ','SLMSK   ','DUGWD   ','DVGWD   ',
     &         'DUASFC  ','DUAGWD  ','DUAMTN  ','UA      ','UAP     ',
     &         'EP      ','CLDWORK '/
      DATA CLB/'90N-90S ','90N-60N ','60N-30N ','30N-30S ',
     &                    '30S-60S ','60S-90S '/
      DATA CST/'MEAN',' LND','SLND',' ICE','SICE',' SEA'/
      DATA NCOL/12/
C
      PRINT 900,KDT
C
      DO 40 N=1,NRM
      IPWR=IPWRCN(CNM(N))
      PRINT 910,CNM(N),IPWR,RHOUR,IDATE,KDT
      NROW=(NLV-1)/NCOL+1
      DO 30 KROW=1,NROW
      K1=(KROW-1)*NCOL+1
      IF(KROW.LT.NROW) THEN
        K2=K1-1+NCOL
        WRITE(CFMT,921) NCOL
        PRINT CFMT,(K,K=K1,K2)
      ELSEIF(K1.LT.NLV) THEN
        K2=NLV
        WRITE(CFMT,922) K2-K1
        PRINT CFMT,(K,K=K1,K2-1)
      ELSE
        K2=NLV
        WRITE(CFMT,923)
        PRINT CFMT
      ENDIF
      WRITE(CFMT,930) -IPWR,K2-K1+1
      PRINT CFMT,(CLB(J),(ZNLM(J,K,N),K=K1,K2),J=1,NLB)
30    CONTINUE
40    CONTINUE
C
      DO 50 N=1,NRS
      IPWR=IPWRCN(CNS(N))
      PRINT 910,CNS(N),IPWR,RHOUR,IDATE,KDT
      WRITE(CFMT,920) NST
      PRINT CFMT,(CST(K),K=1,NST)
      WRITE(CFMT,930) -IPWR,NST
      PRINT CFMT,(CLB(J),(ZNLS(J,K,N),K=1,NST),J=1,NLB)
50    CONTINUE
C
      IF(NZNL.GT.0) THEN
        LABZ='ZNL 92/6'
        FSTEP=KDT
        WRKM=0.
        WRKS=0.
        WRITE(NZNL)LABZ
        WRITE(NZNL)RHOUR,FSTEP,IDATE,
     &             (((ZNLM(J,K,N),J=1,NLB),K=1,NLV),N= 1,10),
     &             (((ZNLM(J,K,N),J=1,NLB),K=1,NLV),N=20,21),
     &             (  WRKM                         ,N=13,30),
     &             (((ZNLS(J,K,N),J=1,NLB),K=1,NST),N= 9,15),
     &             (  WRKS                         ,N= 8,11),
     &             (((ZNLS(J,K,N),J=1,NLB),K=1,NST),N=16,22),
     &             (  WRKS                         ,N=19,29),
     &             (((ZNLS(J,K,N),J=1,NLB),K=1,NST),N=23,23),
     &             (((ZNLM(J,K,N),J=1,NLB),K=1,NLV),N=11,19),
     &             (  WRKM                         ,N=10,30),
     &             (((ZNLS(J,K,N),J=1,NLB),K=1,NST),N= 1, 8),
     &             (((ZNLS(J,K,N),J=1,NLB),K=1,NST),N=24,30),
     &             (  WRKS                         ,N=16,30)
        CLOSE(NZNL)
      ENDIF
      RETURN
900   FORMAT('0','ZONALLY AVERAGED DIAGNOSTICS',2X,'KDT=',I4)
910   FORMAT(1X,A8,' (10**',I3,')',
     &       ' FHOUR=',F6.1,' IDATE= (',4I4,')',' KDT=',I4)
920   FORMAT("(5X,'--LAT-- ',",I2,"(5X,A4))")
921   FORMAT("(5X,'--LAT-- ',",I2,"(5X,'K=',I2))")
922   FORMAT("(5X,'--LAT-- ',",I2,"(5X,'K=',I2),5X,' SUM')")
923   FORMAT("(5X,'--LAT-- ',                   5X,' SUM')")
930   FORMAT("(5X,A8,",I3,"P",I2,"F9.2))")
      END
