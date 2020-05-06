C
      SUBROUTINE GENPDS (IYY,IMM,IDD,IHR,IFT,KPD,LVL,PDS,KZ,Z,KB,IMJM,
     &                   FLD,RMASK,LAVG,IAVG,NC)
C     ===========================================
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GENPDS      GRIB EDITION 1 PRODUCT DEFINITION SECT.
C   PRGMMR: GERALD           ORG: W/NMC21    DATE: 93-11-01
C
C ABSTRACT: GENERATES THE GRIB PRODUCT DEFINITION ARRAY FOR EACH FLD.
C
C PROGRAM HISTORY LOG:
C   93-02-01  TEBOULLE    FORM PDS FOR GRID 255
C   93-11-01  GERALD      FORM PDS FOR GRID 214
C   95-02-22  GERALD      FORM PDS FOR GRID 2
C   95-06-02  C. PETERS   MODIFIED TO ALLOW DIFFERENT FIELDS TO BE
C                         DESCRIBED USING PARAMETERS KPD, LVL
C   95-09-21  C. PETERS   FORM PDS FOR COASTAL OCEAN MODEL GRID
C   96-03-08  L. LOBOCKI  EXTENDED TO INCLUDE LEVELS AT PRESCRIBED
C                         DEPTH
C
C USAGE:    CALL GENPDS(IYY,IMM,IDD,IHR,IFH,KPD,PDS)
C   INPUT ARGUMENT LIST:
C     IYY      - CURRENT YEAR
C     IMM      - CURRENT MONTH
C     IDD      - CURRENT DAY
C     IHR      - VERIFICATION HOUR(00 OR 12)
C     KPD      - PDS OCTET 9 - INDICATOR OF PARAMETER
C     LVL      - PDS OCTET 10 - INDICATOR OF LEVEL
C     KZ       - FLAG FOR PDS OCTETS 11-12
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     OUTPUT = PDS(1) =
C              PDS(2) =
C              PDS(3) = LENGTH OF PDS
C              PDS(4) = 1 = VERSION 1
C              PDS(5) = CENTER ID = NMC
C              PDS(6) = MODEL ID
C              PDS(7) = O.N. 29 GRID NO. 255=UNDEFINED
C              PDS(8) = 1/0 = GDS SECTION IS/ISN'T INCLUDED
C              PDS(9) = DATA ID (31=DIR,32=SPEED....)
C              PDS(10) = LEVEL (1=SEA SURFACE)
C              PDS(11) & PDS(12) =
C              PDS(13) = YEAR
C              PDS(14) = MONTH
C              PDS(15) = DAY
C              PDS(16) = HOUR: RUN OR CYCLE(00 OR 12)
C              PDS(17) =
C              PDS(18) = FORECAST TIME UNIT (1=HOUR)
C              PDS(19) = FORECAST HOUR(00,12,...,24,...,72)
C              PDS(20) = TIME INTERVAL BETWEEN SUCCESSIVE ANLYS
C              PDS(27-28) = PRECISION !!!!! CHECK LATER !!!!!
C
C ATTRIBUTES:
C   LANGUAGE: IBM 370 VS FORTRAN
C   MACHINE:  NAS, CRAY C-90
C
C$$$
C
      CHARACTER*1 PDS(28)
      DIMENSION Z(1),FLD(1),GROUND(IMJM),MASK(IMJM),RMASK(1)
      
C      DATA Z/0.000,0.004,0.010,0.020,0.040,0.060,0.080,0.100,
C     # 0.120,0.160,0.240,0.400,0.600,0.800,1.000/
C      DATA ZZ/0.002,0.007,0.015,0.030,0.050,0.070,0.090,0.110,
C     # 0.140,0.200,0.320,0.500,0.700,0.900,1.200/
C
C     WRITE (*,*) Z
      DO 1 I=1,28
       PDS(I) = CHAR(0)
 1    CONTINUE
C
      PDS(1) = CHAR(0)
      PDS(2) = CHAR(0)
      PDS(3) = CHAR(28)
      PDS(4) = CHAR(1)
      PDS(5) = CHAR(7)
      PDS(6) = CHAR(45)
C     PDS(6) = CHAR(85)
      PDS(7) = CHAR(255)
C     PDS(7) = CHAR(120)   !  ????
      PDS(8) = CHAR(192)
      PDS(9) =  CHAR(KPD)
      PDS(10) = CHAR(LVL)
      IF (LVL.EQ.128) THEN
        Z1 = Z(KZ)*1000.
        Z2 = Z(KZ+1)*1000.
        ISIGT = NINT(Z1)
        ISIGB = NINT(Z2)
        PDS(11) = CHAR(ISIGT)
        PDS(12) = CHAR(ISIGB)
      ELSEIF (LVL.EQ.108) THEN
        Z3 = Z(KZ)*100.
        Z4 = Z(KZ+1)*100.
        ISIGT = NINT(Z3)
        ISIGB = NINT(Z4)
        PDS(11) = CHAR(ISIGT)
        PDS(12) = CHAR(ISIGB)
      ELSEIF (LVL.EQ.160) THEN
        IZ = NINT(Z(1))
        PDS(12)=CHAR(MOD(IZ,256))
        PDS(11)=CHAR(INT(IZ/256))
      ENDIF
      if (IYY.eq.99) then
       IYY=1900+IYY
      endif
      if (IYY.lt.99) then
       IYY=2000+IYY
      endif
      if (IYY.eq.2000) then
         IY=100
        else
        IY=mod(IYY,100)
        endif
      PDS(13) = CHAR(IY)
      PDS(14) = CHAR(IMM)
      PDS(15) = CHAR(IDD)

      PDS(16) = CHAR(IHR)
      PDS(19) = CHAR(IFT)
                              
                              
      IF (IFT.LE.255) THEN                        
                              
        PDS(18) = CHAR(1)
C...... FILLED IN MAIN PROGRAM......
C....   PDS(19) = CHAR(IFH)(IFH = 00,12,24,...,72)

        IF ((LAVG.EQ.3).OR.(LAVG.EQ.4)) THEN
          PDS(19) = CHAR(IFT-IAVG)
          PDS(20) = CHAR(IFT)
          PDS(21) = CHAR(LAVG)
          PDS(23) = CHAR(MOD(NC,256))
          PDS(22) = CHAR(INT(NC/256))
        ENDIF
        
      ELSE    ! (IFT>255)  
                
        IF ((LAVG.EQ.3).OR.(LAVG.EQ.4)) THEN
          PDS(18) = CHAR(2)
          PDS(19) = CHAR(INT((IFT-IAVG)/24))
          PDS(20) = CHAR(INT(IFT/24))
          PDS(21) = CHAR(LAVG)
          PDS(23) = CHAR(MOD(NC,256))
          PDS(22) = CHAR(INT(NC/256))
        ENDIF                
             
        IF (LAVG.EQ.0) THEN
          PDS(18) = CHAR(1)
          PDS(19) = CHAR(INT(IFT/256))
          PDS(20) = CHAR(MOD(IFT,256))
          PDS(21) = CHAR(10)
          PDS(23) = CHAR(MOD(NC,256))
          PDS(22) = CHAR(INT(NC/256))
        ENDIF

      ENDIF
      
      if (iyy.le.2000) then
       icen=20
      else
       icen=21
      endif
      PDS(25) = CHAR(icen)
      IBM = 1                                                
C     HERE IS WHERE STIPULATED NUMBER OF SIGNIFICANT DIGITS IS HARDWIRED      
      IF (KPD.EQ.81) THEN
       ISGDS=1
      ELSE 
       IF (KPD.EQ.176.OR.KPD.EQ.177) THEN
        ISGDS=6
       ELSE
        IF (KPD.EQ.80.OR.KPD.EQ.88) THEN
         ISGDS = 5
        ELSE
         ISGDS = 4
        ENDIF
       ENDIF 
      ENDIF 
      DO I=1,IMJM
        MASK(I)=NINT(RMASK(I))
      ENDDO
      CALL GTBITS(IBM,ISGDS,IMJM,MASK,FLD,ISCALE,GROUND,GMIN,GMAX,NBIT)
      WRITE(71,*) 'AFTER GTBITS: ISCALE = ',ISCALE,' GMIN= ',GMIN,
     # ' GMAX= ',GMAX,' NBIT= ',NBIT
      IF (ISCALE.GE.0) THEN
       PDS(27) = CHAR(ISCALE/256)
      ELSE
       PDS(27) = CHAR(128)
       ISCALE =  ISCALE*(-1)
      ENDIF
      PDS(28) = CHAR(MOD(ISCALE,256))
C
      RETURN
      END
