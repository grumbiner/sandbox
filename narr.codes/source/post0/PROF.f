      SUBROUTINE PROF(NHB,LRSTRT,ITAG,LCLAS1)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  PROF        PROFILE SOUNDINGS
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-04-22
C
C ABSTRACT:  THIS ROUTINE GENERATES THE RAW PROFILE SOUNDING
C            OUTPUT FILES FROM THE FORECAST RESTRT FILE AND
C            AUXILIARY FILES
C
C PROGRAM HISTORY LOG:
C   99-04-22  T BLACK - ORIGINATOR
C
C USAGE:  CALL PROF FROM PROGRAM POST0
C
C   INPUT ARGUMENT LIST:
C     NHB    - THE UNIT NUMBER FOR READING THE NHB FILE
C     LRSTRT - THE UNIT NUMBER FOR READING THE RESTRT FILE
C     ITAG   - THE FORECAST HOUR WE ARE DEALING WITH
C     LCLAS1 - THE UNIT NUMBER FOR WRITING THE PROFILE DATA
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parmsoil"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (NSTAT=2000,LCL1ML=13,LCL1SL=50,NWORDM=(LCL1ML+1)*LM+2*LCL1SL
     &, LRECPR=4*(8+9+LCL1ML*LM+LCL1SL))
C-----------------------------------------------------------------------
C
C    PARMS FOR HOURLY PROFILER OUTPUT
C      NSTAT - MAX NUMBER OF STATIONS
C      NWORDM - DIMENSION OF OUTPUT ARRAY, MUST BE LARGE ENOUGH
C          TO HOLD ALL VARIABLES
C          (MAX NO MULTI-LAYER VARIABLES*LM + NO OF SINGLE LAYER VARS)
C      LCL1ML - NUMBER OF MULTI-LAYER VARIABLES OUTPUT FOR CLASS 1
C      LCL1SL - NUMBER OF SINGLE LAYER VARIABLES OUTPUT FOR CLASS 1
C
C------------------------------------------------------------------------
                             P A R A M E T E R
     & (ITB=76,JTB=134)
                             P A R A M E T E R
     & (A2=17.2693882,A3=273.16,A4=35.86,PQ0=379.90516,DTR=1.74532925E-2
     &, GI=1./9.8,RD=287.04,CP=1004.6,CAPA=RD/CP)
C------------------------------------------------------------------------
                             R E A L
     & STNLAT(NSTAT),STNLON(NSTAT)
                             R E A L
     & DETA(LM),RDETA(LM),AETA(LM),UL(2*LM)
C
                             R E A L
     & RES(NSTAT),FIS(NSTAT),THS(NSTAT),HBOT(NSTAT)
     &,CFRACL(NSTAT),CFRACM(NSTAT),CFRACH(NSTAT),SNO(NSTAT)
     &,SOILTB(NSTAT),SFCEXC(NSTAT),SMSTAV(NSTAT),SMSTOT(NSTAT)
     &,Z0(NSTAT),CZEN(NSTAT),CZMEAN(NSTAT),U00(NSTAT),SR(NSTAT)
     &,ACPREC(NSTAT),CUPREC(NSTAT),ACSNOW(NSTAT),ACSNOM(NSTAT)
     &,SSROFF(NSTAT),BGROFF(NSTAT),SFCSHX(NSTAT),SFCLHX(NSTAT)
     &,SUBSHX(NSTAT),SNOPCX(NSTAT),ASWIN(NSTAT),ASWOUT(NSTAT)
     &,ASWTOA(NSTAT),ALWIN(NSTAT),ALWOUT(NSTAT),ALWTOA(NSTAT)
     &,TSHLTR(NSTAT),QSHLTR(NSTAT)
     &,TH10(NSTAT),Q10(NSTAT),U10(NSTAT),V10(NSTAT)
     &,TLMIN(NSTAT),TLMAX(NSTAT)
     &,SMC(NSTAT,NSOIL),CMC(NSTAT),STC(NSTAT,NSOIL),SH2O(NSTAT,NSOIL)
     &,VEGFRC(NSTAT),POTFLX(NSTAT),PSLP(NSTAT),PDSL1(NSTAT)
     &,EGRID2(NSTAT),SM(NSTAT),SICE(NSTAT)
     &,HBM2(NSTAT),FACTR(NSTAT)
     &,PTBL(ITB,JTB),TTBL(JTB,ITB)
C
                             R E A L
     & T(NSTAT,LM),Q(NSTAT,LM),U(NSTAT,LM),V(NSTAT,LM),Q2(NSTAT,LM)
     &,OMGALF(NSTAT,LM),CWM(NSTAT,LM),TRAIN(NSTAT,LM),TCUCN(NSTAT,LM)
     &,RSWTT(NSTAT,LM),RLWTT(NSTAT,LM),CCR(NSTAT,LM),RTOP(NSTAT,LM)
     &,HTM(NSTAT,LM)
C  
                             R E A L
     & PRODAT(NWORDM),FPACK(NWORDM)
     &,STATPR(NSTAT),STACPR(NSTAT),STAEVP(NSTAT)
     &,STAPOT(NSTAT),STASHX(NSTAT),STASUB(NSTAT),STAPCX(NSTAT)
     &,STASWI(NSTAT),STASWO(NSTAT),STALWI(NSTAT),STALWO(NSTAT)
     &,STALWT(NSTAT),STASWT(NSTAT),STASNM(NSTAT),STASRF(NSTAT)
     &,STABRF(NSTAT),STASNO(NSTAT),DHCNVC(LM,NSTAT),DHRAIN(LM,NSTAT)
     &,STADHC(LM),STADHR(LM)
     &,ACPREC0(NSTAT),CUPREC0(NSTAT),SFCLHX0(NSTAT),POTFLX0(NSTAT)
     &,SFCSHX0(NSTAT),SUBSHX0(NSTAT),SNOPCX0(NSTAT),ASWIN0(NSTAT)
     &,ASWOUT0(NSTAT),ALWIN0(NSTAT),ALWOUT0(NSTAT),ALWTOA0(NSTAT)
     &,ASWTOA0(NSTAT),ACSNOW0(NSTAT),ACSNOM0(NSTAT),SSROFF0(NSTAT)
     &,BGROFF0(NSTAT)
     &,TCUCN0(NSTAT,LM),TRAIN0(NSTAT,LM)
C
                             R E A L
     & DUM(IM,JM,8)
     &,PD(IM,JM),PDS(IM,JM)
C------------------------------------------------------------------------
                             I N T E G E R
     & IDSTN(NSTAT),IHINDX(NSTAT),JHINDX(NSTAT)
     &,             IVINDX(NSTAT),JVINDX(NSTAT)
C
                             I N T E G E R
     & IW(NSTAT,LM),IDAT(3),IDAT0(3)
C
                             I N T E G E R
     & IDUM(IM,JM),LMH(IM,JM)
C------------------------------------------------------------------------
                             L O G I C A L
     & RUN,RESTRT
C------------------------------------------------------------------------
                             C H A R A C T E R
     & RSTFIL*50,RESTHR*4,LABEL*32,CISTAT*8,CIDSTN(NSTAT)*8
C------------------------------------------------------------------------
C***
C***  READ IN THE INFORMATION FILE ABOUT THE SOUNDINGS
C***
      REWIND 19
C
      READ(19)NUMSTA,IDSTN,STNLAT,STNLON
     1,       IHINDX,JHINDX,IVINDX,JVINDX,CIDSTN
      WRITE(6,20)NUMSTA
   20 FORMAT('INIT:  NUMBER OF PROFILE STATIONS ',I5)
	if (ITAG .eq. 0) then
      WRITE(6,30)(IDSTN(N),STNLAT(N)/DTR,STNLON(N)/DTR
     1,               IHINDX(N),JHINDX(N),IVINDX(N),JVINDX(N)
     2,               CIDSTN(N),N=1,NUMSTA)
	endif
   30 FORMAT(2X,I6,2F8.2,4I8,4X,A8)
C------------------------------------------------------------------------
C***
C***  READ QUANTITIES NEEDED FROM THE NHB FILE
C***
      REWIND NHB
C
      READ(NHB)NFCST,NBC,LIST,DT
      READ(NHB)LMH
C
      READ(NHB)
      READ(NHB)((DUM(I,J,1),I=1,IM),J=1,JM)
      DO N=1,NUMSTA
        HBM2(N)=DUM(IHINDX(N),JHINDX(N),1)
      ENDDO
C
      READ(NHB)
      READ(NHB)
      READ(NHB)((DUM(I,J,1),I=1,IM),J=1,JM)
      DO N=1,NUMSTA
        SM(N)=DUM(IHINDX(N),JHINDX(N),1)
      ENDDO
C
      READ(NHB)((DUM(I,J,1),I=1,IM),J=1,JM)
      DO N=1,NUMSTA
        SICE(N)=DUM(IHINDX(N),JHINDX(N),1)
      ENDDO
C
      DO L=1,LM
        READ(NHB)((DUM(I,J,1),I=1,IM),J=1,JM)
        DO N=1,NUMSTA
          HTM(N,L)=DUM(IHINDX(N),JHINDX(N),1)
        ENDDO
      ENDDO
C
      DO L=1,LM
        READ(NHB)
      ENDDO
C
      READ(NHB)DY,CPGFV,EN,ENT,R,PT,TDDAMP
     1,        F4D,F4Q,EF4T,DETA,RDETA,AETA
      NTSPH=INT(3600./DT+0.50)
C
      DO L=1,23
        READ(NHB)
      ENDDO
C
      READ(NHB) PTBL,TTBL,R1,PT1,TSPH,WBD,SBD,TLM0D,TPH0D
C
      READ(NHB)
      READ(NHB)
      READ(NHB)
C
      READ(NHB)((DUM(I,J,1),I=1,IM),J=1,JM)
      DO N=1,NUMSTA
        VEGFRC(N)=DUM(IHINDX(N),JHINDX(N),1)
      ENDDO
C------------------------------------------------------------------------
C***
C***  GENERATE THE NAME OF THE CURRENT RESTRT FILE
C***
      CALL GETENV("tmmark",RESTHR)
      print*,'in PROF, RESTHR=',RESTHR
C
      print*,'ITAG=',ITAG
      IF(RESTHR.EQ.'    ')THEN
        WRITE(RSTFIL,50)ITAG
   50   FORMAT('restrt',I2.2)
      ELSE
        WRITE(RSTFIL,55)ITAG,RESTHR
   55   FORMAT('restrt',I2.2,'.',a4)
      ENDIF
      print*,'RSTFIL=',RSTFIL
C
      CLOSE(LRSTRT)
      OPEN(UNIT=LRSTRT,FILE=RSTFIL,FORM='UNFORMATTED',IOSTAT=IER)
C***
C***  READ QUANTITIES NEEDED FROM THE RESTRT FILE
C***
      READ(LRSTRT)RUN,IDAT,IHRST,NTSD
      print*,'NTSD=',NTSD
      READ(LRSTRT)
C
      DO L=1,LM
        READ(LRSTRT)((DUM(I,J,1),I=1,IM),J=1,JM)
        DO N=1,NUMSTA
          OMGALF(N,L)=DUM(IHINDX(N),JHINDX(N),1)
        ENDDO
      ENDDO
C
      READ(LRSTRT)
      READ(LRSTRT)PD,(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,2)
C
      DO N=1,NUMSTA
        RES(N)=DUM(IHINDX(N),JHINDX(N),1)
        FIS(N)=DUM(IHINDX(N),JHINDX(N),2)
      ENDDO
C
      READ(LRSTRT)
C
      DO L=1,LM
        READ(LRSTRT)((DUM(I,J,1),I=1,IM),J=1,JM)
        READ(LRSTRT)((DUM(I,J,2),I=1,IM),J=1,JM)
        READ(LRSTRT)((DUM(I,J,3),I=1,IM),J=1,JM)
        READ(LRSTRT)((DUM(I,J,4),I=1,IM),J=1,JM)
        READ(LRSTRT)((DUM(I,J,5),I=1,IM),J=1,JM)
        READ(LRSTRT)
        READ(LRSTRT)((DUM(I,J,6),I=1,IM),J=1,JM)
        READ(LRSTRT)((DUM(I,J,7),I=1,IM),J=1,JM)
        READ(LRSTRT)((DUM(I,J,8),I=1,IM),J=1,JM)
C
        DO N=1,NUMSTA
          T(N,L)=DUM(IHINDX(N),JHINDX(N),1)
          Q(N,L)=DUM(IHINDX(N),JHINDX(N),2)
          U(N,L)=DUM(IVINDX(N),JVINDX(N),3)
          V(N,L)=DUM(IVINDX(N),JVINDX(N),4)
          Q2(N,L)=DUM(IHINDX(N),JHINDX(N),5)
          CWM(N,L)=DUM(IHINDX(N),JHINDX(N),6)
          TRAIN(N,L)=DUM(IHINDX(N),JHINDX(N),7)
          TCUCN(N,L)=DUM(IHINDX(N),JHINDX(N),8)
        ENDDO
      ENDDO
C
      READ(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
     1,        (((DUM(I,J,N),I=1,IM),J=1,JM),N=1,6)
      DO N=1,NUMSTA
        Z0(N)=DUM(IHINDX(N),JHINDX(N),4)
        CZEN(N)=DUM(IHINDX(N),JHINDX(N),6)
      ENDDO
C
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,7)
      DO N=1,NUMSTA
        THS(N)=DUM(IHINDX(N),JHINDX(N),2)
        HBOT(N)=DUM(IHINDX(N),JHINDX(N),6)
        CFRACL(N)=DUM(IHINDX(N),JHINDX(N),7)
      ENDDO
C
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,7)
      DO N=1,NUMSTA
        CFRACM(N)=DUM(IHINDX(N),JHINDX(N),7)
      ENDDO
C
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,7)
      DO N=1,NUMSTA
        SNO(N)=DUM(IHINDX(N),JHINDX(N),1)
        PSLP(N)=DUM(IHINDX(N),JHINDX(N),5)
        CFRACH(N)=DUM(IHINDX(N),JHINDX(N),7)
      ENDDO
C
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,4)
      DO N=1,NUMSTA
        SOILTB(N)=DUM(IHINDX(N),JHINDX(N),1)
        SFCEXC(N)=DUM(IHINDX(N),JHINDX(N),2)
        SMSTAV(N)=DUM(IHINDX(N),JHINDX(N),3)
        SMSTOT(N)=DUM(IHINDX(N),JHINDX(N),4)
      ENDDO
C
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,3)
      DO N=1,NUMSTA
        CZMEAN(N)=DUM(IHINDX(N),JHINDX(N),3)
      ENDDO
C
      READ(LRSTRT)((DUM(I,J,1),I=1,IM),J=1,JM),UL,IDUM
     1,           ((DUM(I,J,2),I=1,IM),J=1,JM)
      DO N=1,NUMSTA
        U00(N)=DUM(IHINDX(N),JHINDX(N),1)
        SR(N)=DUM(IHINDX(N),JHINDX(N),2)
      ENDDO
C
      READ(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
     1,        (((DUM(I,J,N),I=1,IM),J=1,JM),N=1,4)
      DO N=1,NUMSTA
        ACPREC(N)=DUM(IHINDX(N),JHINDX(N),2)
        CUPREC(N)=DUM(IHINDX(N),JHINDX(N),4)
      ENDDO
C
      READ(LRSTRT)
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,4)
      DO N=1,NUMSTA
        ACSNOW(N)=DUM(IHINDX(N),JHINDX(N),1)
        ACSNOM(N)=DUM(IHINDX(N),JHINDX(N),2)
        SSROFF(N)=DUM(IHINDX(N),JHINDX(N),3)
        BGROFF(N)=DUM(IHINDX(N),JHINDX(N),4)
      ENDDO
C
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,4)
      DO N=1,NUMSTA
        SFCSHX(N)=DUM(IHINDX(N),JHINDX(N),1)
        SFCLHX(N)=DUM(IHINDX(N),JHINDX(N),2)
        SUBSHX(N)=DUM(IHINDX(N),JHINDX(N),3)
        SNOPCX(N)=DUM(IHINDX(N),JHINDX(N),4)
      ENDDO
C
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,6)
      DO N=1,NUMSTA
        ASWIN(N)=DUM(IHINDX(N),JHINDX(N),1)
        ASWOUT(N)=DUM(IHINDX(N),JHINDX(N),2)
        ASWTOA(N)=DUM(IHINDX(N),JHINDX(N),3)
        ALWIN(N)=DUM(IHINDX(N),JHINDX(N),4)
        ALWOUT(N)=DUM(IHINDX(N),JHINDX(N),5)
        ALWTOA(N)=DUM(IHINDX(N),JHINDX(N),6)
      ENDDO
C
      READ(LRSTRT)
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,6)
      DO N=1,NUMSTA
        TH10(N)=DUM(IHINDX(N),JHINDX(N),1)
        Q10(N)=DUM(IHINDX(N),JHINDX(N),2)
        U10(N)=DUM(IHINDX(N),JHINDX(N),3)
        V10(N)=DUM(IHINDX(N),JHINDX(N),4)
        TSHLTR(N)=DUM(IHINDX(N),JHINDX(N),5)
        QSHLTR(N)=DUM(IHINDX(N),JHINDX(N),6)
      ENDDO
C
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,NSOIL)
      DO NS=1,NSOIL
        DO N=1,NUMSTA
          SMC(N,NS)=DUM(IHINDX(N),JHINDX(N),NS)
        ENDDO
      ENDDO
C
      READ(LRSTRT)((DUM(I,J,1),I=1,IM),J=1,JM)
      DO N=1,NUMSTA
        CMC(N)=DUM(IHINDX(N),JHINDX(N),1)
      ENDDO
C
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,NSOIL)
      DO NS=1,NSOIL
        DO N=1,NUMSTA
          STC(N,NS)=DUM(IHINDX(N),JHINDX(N),NS)
        ENDDO
      ENDDO
C
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,NSOIL)
      DO NS=1,NSOIL
        DO N=1,NUMSTA
          SH2O(N,NS)=DUM(IHINDX(N),JHINDX(N),NS)
        ENDDO
      ENDDO
C
      READ(LRSTRT)
      READ(LRSTRT)
      READ(LRSTRT)
C------------------------------------------------------------------------
      READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,3)
     1,              ACUTIM,ARATIM,APHTIM
     2,              NHEAT,NPHS,NCNVC,NPREC,NRDSW,NRDLW,NSRFC
     3,              TPH0D,TLM0D,RESTRT
      DO N=1,NUMSTA
        POTFLX(N)=DUM(IHINDX(N),JHINDX(N),1)
        TLMIN(N)=DUM(IHINDX(N),JHINDX(N),2)
        TLMAX(N)=DUM(IHINDX(N),JHINDX(N),3)
      ENDDO
C------------------------------------------------------------------------
C***
C***  READ RADIATIVE TEMPERATURE TENDENCIES
C***
      DO L=1,LM
        READ(LRSTRT)((DUM(I,J,1),I=1,IM),J=1,JM)
        READ(LRSTRT)((DUM(I,J,2),I=1,IM),J=1,JM)
        DO N=1,NUMSTA
          RSWTT(N,L)=DUM(IHINDX(N),JHINDX(N),1)
          RLWTT(N,L)=DUM(IHINDX(N),JHINDX(N),2)
        ENDDO
      ENDDO
C
      CLOSE(LRSTRT)
C------------------------------------------------------------------------
C***
C***  THE FORECAST HOUR
C***  
      IFHR=NTSD/NTSPH
      print*,'IFHR=',IFHR
C------------------------------------------------------------------------
      print*,'NTSD=',NTSD
      IF(NTSD.GT.1)THEN
C***
C***  GENERATE THE NAME OF THE PRECEDING RESTRT FILE
C***
C       ITAG0=ITAG-1
        ITAG0=ITAG-3
        CALL GETENV("tmmark",RESTHR)
C
        IF(RESTHR.EQ.'    ')THEN
          WRITE(RSTFIL,80)ITAG0
   80     FORMAT('restrt',I2.2)
        ELSE
          WRITE(RSTFIL,85)ITAG0,RESTHR
   85     FORMAT('restrt',I2.2,'.',a4)
        ENDIF
C
        CLOSE(LRSTRT)
        OPEN(UNIT=LRSTRT,FILE=RSTFIL,FORM='UNFORMATTED',IOSTAT=IER)
C***
C***  READ THE PREVIOUS RESTRT FILE
C***
        READ(LRSTRT)
        READ(LRSTRT)
C
        DO L=1,LM
          READ(LRSTRT)
        ENDDO
C
        READ(LRSTRT)
        READ(LRSTRT)
        READ(LRSTRT)
C
        DO L=1,LM
          READ(LRSTRT)
          READ(LRSTRT)
          READ(LRSTRT)
          READ(LRSTRT)
          READ(LRSTRT)
          READ(LRSTRT)
          READ(LRSTRT)
          READ(LRSTRT)((DUM(I,J,1),I=1,IM),J=1,JM)
          READ(LRSTRT)((DUM(I,J,2),I=1,IM),J=1,JM)
C
          DO N=1,NUMSTA
            TRAIN0(N,L)=DUM(IHINDX(N),JHINDX(N),1)
            TCUCN0(N,L)=DUM(IHINDX(N),JHINDX(N),2)
          ENDDO
C
        ENDDO      
C
        READ(LRSTRT)
        READ(LRSTRT)
        READ(LRSTRT)
        READ(LRSTRT)
        READ(LRSTRT)
        READ(LRSTRT)
        READ(LRSTRT)
C
        READ(LRSTRT)RUN,IDAT0,IHRST0,NTSD0,LABEL
     1,          (((DUM(I,J,N),I=1,IM),J=1,JM),N=1,4)
        DO N=1,NUMSTA
          ACPREC0(N)=DUM(IHINDX(N),JHINDX(N),2)
          CUPREC0(N)=DUM(IHINDX(N),JHINDX(N),4)
        ENDDO
C
        READ(LRSTRT)
C
        READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,4)
        DO N=1,NUMSTA
          ACSNOW0(N)=DUM(IHINDX(N),JHINDX(N),1)
          ACSNOM0(N)=DUM(IHINDX(N),JHINDX(N),2)
          SSROFF0(N)=DUM(IHINDX(N),JHINDX(N),3)
          BGROFF0(N)=DUM(IHINDX(N),JHINDX(N),4)
        ENDDO
C
        READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,4)
        DO N=1,NUMSTA
          SFCSHX0(N)=DUM(IHINDX(N),JHINDX(N),1)
          SFCLHX0(N)=DUM(IHINDX(N),JHINDX(N),2)
          SUBSHX0(N)=DUM(IHINDX(N),JHINDX(N),3)
          SNOPCX0(N)=DUM(IHINDX(N),JHINDX(N),4)
        ENDDO
C
        READ(LRSTRT)(((DUM(I,J,N),I=1,IM),J=1,JM),N=1,6)
        DO N=1,NUMSTA
          ASWIN0(N)=DUM(IHINDX(N),JHINDX(N),1)
          ASWOUT0(N)=DUM(IHINDX(N),JHINDX(N),2)
          ASWTOA0(N)=DUM(IHINDX(N),JHINDX(N),3)
          ALWIN0(N)=DUM(IHINDX(N),JHINDX(N),4)
          ALWOUT0(N)=DUM(IHINDX(N),JHINDX(N),5)
          ALWTOA0(N)=DUM(IHINDX(N),JHINDX(N),6)
        ENDDO
C
        READ(LRSTRT)
        READ(LRSTRT)
        READ(LRSTRT)
        READ(LRSTRT)
        READ(LRSTRT)
C
        READ(LRSTRT)((DUM(I,J,1),I=1,IM),J=1,JM)
        DO N=1,NUMSTA
          POTFLX0(N)=DUM(IHINDX(N),JHINDX(N),1)
        ENDDO
C
      ENDIF
C
      CLOSE(LRSTRT)
C------------------------------------------------------------------------
C***
C***  ALL THE DATA IS NOW IN.
C***  CALCULATE CLOUD FRACTION AND CLOUD WATER/ICE ID NUMBER.
C***
C------------------------------------------------------------------------
      UTIM=1.
      US=1.
      CCLIMIT=1.E-3
      CLIMIT =1.E-20
C-----------------------------------------------------------------------
!$OMP parallel do 
      DO N=1,NUMSTA
        IW(N,1)=0
        CCR(N,1)=0.
        PDSL1(N)=PD(IHINDX(N),JHINDX(N))*RES(N)
      ENDDO
C
C------------------QW, QI AND QINT--------------------------------------
C
      DO 220 L=2,LM
C
!$OMP parallel do private(cwmkl,fiq,hh,iwkl,lml,pp,qc,qi,qint,qkl,qw,
!$OMP*                    rqkl,tkl,tmt0,tmt15,u00kl)
      DO 210 N=1,NUMSTA
      LML=LM-LMH(IHINDX(N),JHINDX(N))
      HH=HTM(N,L)*HBM2(N)
      TKL=T(N,L)
      QKL=Q(N,L)
      CWMKL=CWM(N,L)
      TMT0=(TKL-273.16)*HH
      TMT15=AMIN1(TMT0,-15.)*HH
      AI=0.008855
      BI=1.
C
      IF(TMT0.LT.-20.)THEN
        AI=0.007225
        BI=0.9674
      ENDIF
C
      PP=PDSL1(N)*AETA(L)+PT
      QW=HH*PQ0/PP*EXP(HH*A2*(TKL-A3)/(TKL-A4))
      QI=QW*(BI+AI*AMIN1(TMT0,0.))
      QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
      IF(TMT0.LE.-40.)QINT=QI
C
C-------------------ICE-WATER ID NUMBER IW------------------------------
C
      U00KL=U00(N)+UL(L+LML)*(0.95-U00(N))*UTIM
      IF(TMT0.LT.-15.)THEN
        FIQ=QKL-U00KL*QI
        IF(FIQ.GT.0..OR.CWMKL.GT.CLIMIT)THEN
          IW(N,L)=1
        ELSE
          IW(N,L)=0
        ENDIF
      ENDIF
C
      IF(TMT0.GE.0.)THEN
        IW(N,L)=0
      ENDIF
C
      IF(TMT0.LT.0..AND.TMT0.GE.-15.)THEN
        IW(N,L)=0
        IF(IW(N,L-1).EQ.1.AND.CWMKL.GT.CLIMIT)IW(N,L)=1
      ENDIF
C
      IWKL=IW(N,L)
C----------------THE SATUATION SPECIFIC HUMIDITY------------------------
      FIW=FLOAT(IWKL)
      QC=(1.-FIW)*QINT+FIW*QI
C----------------THE RELATIVE HUMIDITY----------------------------------
      IF(QC.LE.0.)THEN
         RQKL=0.
       ELSE
         RQKL=QKL/QC
      ENDIF
C----------------CLOUD COVER RATIO CCR----------------------------------
      IF(RQKL.GE.0.9999)THEN
        CCR(N,L)=AMIN1(US,RQKL)
      ELSE
        CCR(N,L)=RQKL*(1.-EXP((-1000.*CWMKL)/(US-RQKL)))
      ENDIF
C----------------------------------------------------------------------
  210                 CONTINUE
  220                 CONTINUE
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C***
C***  BEGIN THE PROFILE POSTING CODE.
C***
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C***
C***  USE ZERO IN ACCUMULATION ARRAYS AT APPROPRIATE TIMES
C***
      IF(RESTRT.OR.NTSD.LT.2)THEN
        DO N=1,NUMSTA
C
C*** ZERO ACCUMLATION ARRAYS.
C
          STATPR(N)=0.
          STACPR(N)=0.
          STAEVP(N)=0.
          STAPOT(N)=0.
          STASHX(N)=0.
          STASUB(N)=0.
          STAPCX(N)=0.
          STASWI(N)=0.
          STASWO(N)=0.
          STALWI(N)=0.
          STALWO(N)=0.
          STALWT(N)=0.
          STASWT(N)=0.
          STASNM(N)=0.
          STASNO(N)=0.
          STASRF(N)=0.
          STABRF(N)=0.
          DO L=1,LM
            DHCNVC(L,N)=0.
            DHRAIN(L,N)=0.
          ENDDO
        ENDDO
C
        GO TO 300
      ENDIF
C---------------------------------------------------------------------
C***
C***  WE MUST CHECK TO SEE IF WE ARE 1 HOUR AFTER ANY OF THE 
C***  ACCUMULATION BUCKETS HAVE BEEN EMPTIED.  IF WE ARE AT SUCH A 
C***  TIME THEN WE NEED TO SET TO ZERO THE VARIABLES USED TO HOLD
C***  THE PRECEDING HOUR'S VALUES.
C***
C---------------------------------------------------------------------
      TIME=(NTSD-1)*DT
      RESET0=TIME-(NTSD/NPREC)*NPREC*DT
      RESET1=(NPHS-1)*DT+3600.
C
      IF(MOD(NTSD,NPREC).GE.NPHS.AND.RESET0.LE.RESET1)THEN
        DO N=1,NUMSTA
          STATPR(N)=0.
          STACPR(N)=0.
          STASNM(N)=0.
          STASNO(N)=0.
          STASRF(N)=0.
          STABRF(N)=0.
        ENDDO
      ELSE
        DO N=1,NUMSTA
          STATPR(N)=ACPREC0(N)*1.E3
          STACPR(N)=CUPREC0(N)*1.E3
          STASNM(N)=ACSNOM0(N)*1.E3
          STASNO(N)=ACSNOW0(N)*1.E3
          STASRF(N)=SSROFF0(N)*1.E3
          STABRF(N)=BGROFF0(N)*1.E3
        ENDDO
      ENDIF          
C
      RESET0=TIME-(NTSD/NRDSW)*NRDSW*DT
      IF(MOD(NTSD,NRDSW).GE.NPHS.AND.RESET0.LE.RESET1)THEN
        DO N=1,NUMSTA
          STASWI(N)=0.
          STASWO(N)=0.
          STASWT(N)=0.
        ENDDO
      ELSE
        DO N=1,NUMSTA
          STASWI(N)=ASWIN0(N)
          STASWO(N)=ASWOUT0(N)
          STASWT(N)=ASWTOA0(N)
        ENDDO
      ENDIF
C
      RESET0=TIME-(NTSD/NRDLW)*NRDLW*DT
      IF(MOD(NTSD,NRDLW).GE.NPHS.AND.RESET0.LE.RESET1)THEN
        DO N=1,NUMSTA
          STALWI(N)=0.
          STALWO(N)=0.
          STALWT(N)=0.
        ENDDO
      ELSE
        DO N=1,NUMSTA
          STALWI(N)=ALWIN0(N)
          STALWO(N)=ALWOUT0(N)
          STALWT(N)=-ALWTOA0(N)
        ENDDO
      ENDIF
C
      RESET0=TIME-(NTSD/NSRFC)*NSRFC*DT
      IF(MOD(NTSD,NSRFC).GE.NPHS.AND.RESET0.LE.RESET1)THEN
        DO N=1,NUMSTA
          STAEVP(N)=0.
          STAPOT(N)=0.
          STASHX(N)=0.
          STASUB(N)=0.
          STAPCX(N)=0.
        ENDDO
      ELSE
        DO N=1,NUMSTA
          STAEVP(N)=SFCLHX0(N)
          STAPOT(N)=POTFLX0(N)
          STASHX(N)=SFCSHX0(N)
          STASUB(N)=SUBSHX0(N)
          STAPCX(N)=SNOPCX0(N)
        ENDDO
      ENDIF
C
      RESET0=TIME-(NTSD/NHEAT)*NHEAT*DT
      IF(MOD(NTSD,NHEAT).GE.NCNVC.AND.RESET0.LE.RESET1)THEN
        DO N=1,NUMSTA
          DO L=1,LM
            DHCNVC(L,N)=0.
            DHRAIN(L,N)=0.
          ENDDO
        ENDDO
      ELSE
        DO N=1,NUMSTA
          DO L=1,LM
            DHCNVC(L,N)=TCUCN0(N,L)
            DHRAIN(L,N)=TRAIN0(N,L)
          ENDDO
        ENDDO
      ENDIF
C------------------------------------------------------------------
  300 CONTINUE
C------------------------------------------------------------------
C
C***  FOR ROTATION OF WINDS FROM E-GRID TO GEODETIC ORIENTATION
C***  WE NEED THE TWO QUANTITIES BELOW.
C
      SINPH0=SIN(TPH0D*DTR)
      COSPH0=COS(TPH0D*DTR)
C
C***  INITIAL CALCULATIONS/PREPARATIONS.  WE LOAD SEVERAL
C***  ARRAYS WITH PROFILE VARIABLES.
C
!$OMP parallel do
      DO N=1,NUMSTA
        IF(CZMEAN(N).GT.0.)THEN
          FACTR(N)=CZEN(N)/CZMEAN(N)
        ELSE
          FACTR(N)=0.
        ENDIF
      ENDDO
C
C***  ADJUST SHORTAVE TENDENCIES TO ACCOUNT FOR CHANGE OF SOLAR POSITION
C***  BETWEEN CALLS TO RADIATION
C
!$OMP parallel do
      DO L=1,LM
        DO N=1,NUMSTA
          RSWTT(N,L)=RSWTT(N,L)*FACTR(N)
        ENDDO
      ENDDO
C
C***  COMPUTE RTOP
C
!$OMP parallel do
      DO L=1,LM
        DO N=1,NUMSTA
          APEL=PT+AETA(L)*PDSL1(N)
          RTOP(N,L)=R*T(N,L)*(1.+0.608*Q(N,L))/APEL
        ENDDO
      ENDDO
C
C***  PDS IS SURFACE PRESSURE.
C
!$OMP parallel do 
      DO J=1,JM
      DO I=1,IM
        PDS(I,J)=PD(I,J)+PT
      ENDDO
      ENDDO
C
C***  EGRID2 IS THE SURFACE TEMPERATURE.
C
!$OMP parallel do 
      DO N=1,NUMSTA
        EGRID2(N)= THS(N)*(PDS(IHINDX(N),JHINDX(N))*1.E-5)**CAPA
        IF(ACPREC(N).LT.0.)ACPREC(N)=0.
        IF(CUPREC(N).LT.0.)CUPREC(N)=0.
      ENDDO
C
C***  SET CYCLE, DATE, AND FORECAST TIME.
C
      IHR  =NTSD/NTSPH+0.5
      IYR  =IDAT(3)
      IMNTH=IDAT(1)
      IDAY =IDAT(2)
      IFCST=(NTSD-1)*DT
C
      WRITE(LIST,*)' POST PROFILE FOR ',
     1                       IYR,IMNTH,IDAY,IHR
C
C***  SET RTSPH,RTSCU,RTSRA TO 1. OVER THE NUMBER OF TIMES THE
C***  VARIOUS PHYSICS ROUTINES HAVE BEEN
C***  CALLED SINCE LAST OUTPUT OF PROFILER DATA.  NECESSARY FOR
C***  CORRECT AVERAGING OF VARIABLES.
C
      IF(APHTIM.GT.0.)THEN
        RTSPH=1./APHTIM
      ELSE
        RTSPH=1.
      ENDIF
C
      IF(ACUTIM.GT.0.)THEN
        RTSCU=1./ACUTIM
      ELSE
        RTSCU=1.
      ENDIF
C
      IF(ARATIM.GT.0.)THEN
        RTSRA=1./ARATIM
      ELSE
        RTSRA=1.
      ENDIF
C
C--------------------------------------------------------------------------
C--------------------------------------------------------------------------
C***
C***  OUTPUT PROFILE DATA.  THE FOLLOWING LOOP IS OVER ALL PROFILE SITES.
C***
C--------------------------------------------------------------------------
      OPEN(UNIT=LCLAS1,ACCESS='DIRECT',RECL=LRECPR,IOSTAT=IER)
C--------------------------------------------------------------------------
      DO 1000 N=1,NUMSTA
C
C***  ZERO OUTPUT ARRAY.
C
      DO K=1,NWORDM
        PRODAT(K)=0.
        FPACK(K) =0.
      ENDDO
C
C***  CONSTRUCT HEADER FOR CURRENT PROFILE SITE.  THE HEADER CONTAINS
C***  THE FOLLOWING INFORMATION:  PACKED CYCLE-DATE, FORECAST TIME,
C***  INTEGER STATION ID, STATION LATITUDE, STATION LONGITUDE, STATION
C***  ELEVATION, NUMBER OF VERTICAL LEVELS IN PROFILE, NUMBER OF MULTI-
C***  LEVEL PARAMETERS, NUMBER OF SINGLE LEVEL PARAMETERS, TOTAL LENGTH
C***  (IN WORDS) OF MULTI- AND SINGLE LEVEL DATA, PROFILE CLASS FLAG,
C***  AND A DUMMY WORD FOR FUTURE USE.
C
      IH=IHINDX(N)
      JH=JHINDX(N)
      LMHK     = LMH(IH,JH)
      NWORD2   = 2*LMHK
      NWORD3   = 3*LMHK
      NWORD4   = 4*LMHK
      NWORD5   = 5*LMHK
      NWORD6   = 6*LMHK
      NWORD7   = 7*LMHK
      NWORD8   = 8*LMHK
      NWORD9   = 9*LMHK
      NWORD10  = 10*LMHK
      NWORD11  = 11*LMHK
      NWORD12  = 12*LMHK
      NWORD13  = 13*LMHK
      ISTAT    = IDSTN(N)
      CISTAT   = CIDSTN(N)
C
C    PRINT*,'ISTAT ', ISTAT
C
      FPACK(1) = STNLAT(N)/DTR
      FPACK(2) = -STNLON(N)/DTR
c     print*,'FPACK(1),FPACK(2)=',FPACK(1),FPACK(2)
      IF(FPACK(2).LT.-180.)FPACK(2)=FPACK(2)+360.
      FPACK(3) = FIS(N)*GI
      FPACK(4) = FLOAT(LMHK)
      FPACK(5) = LCL1ML
      FPACK(6) = LCL1SL
      FPACK(7) = 9+FPACK(5)*FPACK(4)+FPACK(6)
      FPACK(8) = 999.
      FPACK(9) = 999.
C
C***  WIND ROTATION SINES AND COSINES
C
      DLM    = STNLON(N)+TLM0D*DTR
      XX     = COSPH0*COS(STNLAT(N))*COS(DLM)
     1        +SINPH0*SIN(STNLAT(N))
      YY     = -COS(STNLAT(N))*SIN(DLM)
      TLON   = ATAN(YY/XX)
      ALPHA  = ASIN(SINPH0*SIN(TLON)/COS(STNLAT(N)))
      SINALP = SIN(ALPHA)
      COSALP = COS(ALPHA)
C
C------------------------------------------------------------------
C***  EXTRACT PRESSURE AND TEMPERATURE PROFILES.
C***  EXTRACT/ROTATE U AND V WIND COMPONENT PROFILES.
C***  EXTRACT SPECIFIC HUMIDITY AND TEMPERATURE TENDENCY.
C***  EXTRACT CLOUD WATER, HEATING DUE TO CONVECTION, LARGE
C***  SCALE RAIN, SHORT WAVE RADIATION, LONG WAVE RADIATION,
C***  AND CLOUD FRACTION.
C------------------------------------------------------------------
C
      DO LV=1,LMHK
        LVL=LMHK-LV+1
        PRODAT(LVL)      = PDSL1(N)*AETA(LV)+PT
        PRODAT(LMHK+LVL) = T(N,LV)
C
C***  ROTATE WINDS
C
        UT     = U(N,LV)
        VT     = V(N,LV)
        PRODAT(NWORD2+LVL) = UT*COSALP+VT*SINALP
        PRODAT(NWORD3+LVL) = VT*COSALP-UT*SINALP
C
        PRODAT(NWORD4+LVL) = Q(N,LV)
C
        IF(RTOP(N,LV).GT.1.E-12)
     1   PRODAT(NWORD5+LVL) = OMGALF(N,LV)*CP/(RTOP(N,LV)*DT)
        IF(IW(N,LV).GT.0.5)THEN
          PRODAT(NWORD6+LVL) = -CWM(N,LV)
        ELSE
          PRODAT(NWORD6+LVL) = CWM(N,LV)
        ENDIF
C
        PRODAT(NWORD7+LVL) = TCUCN(N,LV)
        PRODAT(NWORD8+LVL) = TRAIN(N,LV)
        PRODAT(NWORD9+LVL) = RSWTT(N,LV)
        PRODAT(NWORD10+LVL)= RLWTT(N,LV)
        PRODAT(NWORD11+LVL)= CCR(N,LV)*100.
C
        IF(LV.EQ.1)THEN
          PRODAT(NWORD12+LVL)=Q2(N,LV)
        ELSE
          PRODAT(NWORD12+LVL)=(Q2(N,LV)+Q2(N,LV-1))*0.5
        ENDIF
      ENDDO
C
C***  MODIFY ACCUMLATIONS SO AS TO REPRESENT ACCUMULATED
C***  CHANGE SINCE LAST PROFILE OUTPUT TIME.
C
      DO LL=1,LMHK
        STADHC(LL) = PRODAT(NWORD7+LL) - DHCNVC(LL,N)
        STADHR(LL) = PRODAT(NWORD8+LL) - DHRAIN(LL,N)
C
        DHCNVC(LL,N) = PRODAT(NWORD7+LL)
        DHRAIN(LL,N) = PRODAT(NWORD8+LL)
C
        IF(MOD(NTSD,NHEAT).LT.NCNVC)THEN
          DHCNVC(LL,N) = 0.
          DHRAIN(LL,N) = 0.
        ENDIF
      ENDDO
C
C***  EXTRACT SINGLE LEVEL DATA.   EGRID2 IS SURFACE TEMPERATURE.
C
      PRODAT(NWORD13+1)  = PSLP  (N)
      PRODAT(NWORD13+2)  = PDS   (IH,JH)
      PRODAT(NWORD13+3)  = EGRID2(N)
      PRODAT(NWORD13+4)  = TLMIN (N)
      PRODAT(NWORD13+5)  = TLMAX (N)
      PRODAT(NWORD13+6)  = SMSTAV(N)*100.
      PRODAT(NWORD13+7)  = ACPREC(N)*1000.
      PRODAT(NWORD13+8)  = CUPREC(N)*1000.
      PRODAT(NWORD13+27) = Z0    (N)
C
      STAPRX=PRODAT(NWORD13+7)-STATPR(N)
      STACRX=PRODAT(NWORD13+8)-STACPR(N)
C
C***  ROTATE WINDS
C
      UT     = U10(N)
      VT     = V10(N)
      PRODAT(NWORD13+28) = UT*COSALP+VT*SINALP
      PRODAT(NWORD13+29) = VT*COSALP-UT*SINALP
C
      PRODAT(NWORD13+30) = TH10  (N)
      PRODAT(NWORD13+31) = Q10   (N)
      PRODAT(NWORD13+32) = TSHLTR(N)
      PRODAT(NWORD13+33) = QSHLTR(N)
      PRODAT(NWORD13+34) = SFCEXC(N)
      PRODAT(NWORD13+35) = VEGFRC(N)*100.
      PRODAT(NWORD13+36) = CMC   (N)*1000.
      PRODAT(NWORD13+37) = SMC   (N,1)
      PRODAT(NWORD13+38) = SMC   (N,2)
      PRODAT(NWORD13+39) = SMC   (N,3)
      PRODAT(NWORD13+40) = SMC   (N,4)
      PRODAT(NWORD13+41) = STC   (N,1)
      PRODAT(NWORD13+42) = STC   (N,2)
      PRODAT(NWORD13+43) = STC   (N,3)
      PRODAT(NWORD13+44) = STC   (N,4)
      PRODAT(NWORD13+45) = SM    (N) + SICE(N)
      PRODAT(NWORD13+46) = CFRACL(N)*100.
      PRODAT(NWORD13+47) = CFRACM(N)*100.
      PRODAT(NWORD13+48) = CFRACH(N)*100.
      PRODAT(NWORD13+49) = SR    (N)*100.
      PRODAT(NWORD13+50) = NINT(HBOT(N))
C
      PRODAT(NWORD13+9)   = SFCLHX(N)
      PRODAT(NWORD13+10)  = POTFLX(N)
      PRODAT(NWORD13+11)  = SFCSHX(N)
      PRODAT(NWORD13+12)  = SUBSHX(N)
      PRODAT(NWORD13+13)  = SNOPCX(N)
      PRODAT(NWORD13+14)  = ASWIN (N)
      PRODAT(NWORD13+15)  = ASWOUT(N)
      PRODAT(NWORD13+16)  = ALWIN (N)
      PRODAT(NWORD13+17)  = ALWOUT(N)
      PRODAT(NWORD13+18)  =-ALWTOA(N)
      PRODAT(NWORD13+19)  = ASWTOA(N)
      PRODAT(NWORD13+20)  = ACSNOW(N)*1000.
      PRODAT(NWORD13+21)  = SMSTOT(N)*1000.
      PRODAT(NWORD13+22)  = SNO   (N)*1000.
      PRODAT(NWORD13+23)  = ACSNOM(N)*1000.
      PRODAT(NWORD13+24)  = SSROFF(N)*1000.
      PRODAT(NWORD13+25)  = BGROFF(N)*1000.
      PRODAT(NWORD13+26)  = SOILTB(N)
C
C***  ACCUMULATED CHANGE SINCE LAST PROFILE OUTPUT TIME.
C
      PSFCEVP  = PRODAT(NWORD13+9 ) - STAEVP(N)
      PPOTEVP  = PRODAT(NWORD13+10) - STAPOT(N)
      PSFCSHX  = PRODAT(NWORD13+11) - STASHX(N)
      PSFCSUB  = PRODAT(NWORD13+12) - STASUB(N)
      PSNOPCX  = PRODAT(NWORD13+13) - STAPCX(N)
      PRSWIN   = PRODAT(NWORD13+14) - STASWI(N)
      PRSWOUT  = PRODAT(NWORD13+15) - STASWO(N)
      PRLWIN   = PRODAT(NWORD13+16) - STALWI(N)
      PRLWOUT  = PRODAT(NWORD13+17) - STALWO(N)
      PRLWTOA  = PRODAT(NWORD13+18) - STALWT(N)
      PRSWTOA  = PRODAT(NWORD13+19) - STASWT(N)
      PACSNOW  = PRODAT(NWORD13+20) - STASNO(N)
      PACSNOM  = PRODAT(NWORD13+23) - STASNM(N)
      PSSROFF  = PRODAT(NWORD13+24) - STASRF(N)
      PBGROFF  = PRODAT(NWORD13+25) - STABRF(N)
C***
C***  TRANSFER STATION PROFILE DATA TO "PACKED" OUTPUT ARRAY.
C***
      NN   = 0
      NLEN = FPACK(7)
C
      DO NL = 10,NLEN
        NN = NL-9
        FPACK(NL) = PRODAT(NN)
      ENDDO
C
C***  REPLACE ACCUMULATED QUANTITIES WITH ACCUMULATION
C***  SINCE LAST PROFILE OUTPUT TIME.
C
      DO LL = 1,LMHK
        FPACK(9+NWORD7+LL) = STADHC(LL)*RTSCU
        FPACK(9+NWORD8+LL) = STADHR(LL)*RTSRA
      ENDDO
C
      FPACK(9+NWORD13+7)  = STAPRX
      FPACK(9+NWORD13+8)  = STACRX
      FPACK(9+NWORD13+9)  = PSFCEVP * RTSPH
      FPACK(9+NWORD13+10) = PPOTEVP * RTSPH
      FPACK(9+NWORD13+11) = PSFCSHX * RTSPH
      FPACK(9+NWORD13+12) = PSFCSUB * RTSPH
      FPACK(9+NWORD13+13) = PSNOPCX * RTSPH
      FPACK(9+NWORD13+14) = PRSWIN  * RTSPH
      FPACK(9+NWORD13+15) = PRSWOUT * RTSPH
      FPACK(9+NWORD13+16) = PRLWIN  * RTSPH
      FPACK(9+NWORD13+17) = PRLWOUT * RTSPH
      FPACK(9+NWORD13+18) = PRLWTOA * RTSPH
      FPACK(9+NWORD13+19) = PRSWTOA * RTSPH
      FPACK(9+NWORD13+20) = PACSNOW
      FPACK(9+NWORD13+23) = PACSNOM
      FPACK(9+NWORD13+24) = PSSROFF
      FPACK(9+NWORD13+25) = PBGROFF
C
      IF(RESTRT)THEN
        DO LL = 1,LMHK
          FPACK(9+NWORD7+LL) = 0.
          FPACK(9+NWORD8+LL) = 0.
        ENDDO
C
        FPACK(9+NWORD13+7)  = 0.
        FPACK(9+NWORD13+8)  = 0.
        FPACK(9+NWORD13+9)  = 0.
        FPACK(9+NWORD13+10) = 0.
        FPACK(9+NWORD13+11) = 0.
        FPACK(9+NWORD13+12) = 0.
        FPACK(9+NWORD13+13) = 0.
        FPACK(9+NWORD13+14) = 0.
        FPACK(9+NWORD13+15) = 0.
        FPACK(9+NWORD13+16) = 0.
        FPACK(9+NWORD13+17) = 0.
        FPACK(9+NWORD13+18) = 0.
        FPACK(9+NWORD13+19) = 0.
        FPACK(9+NWORD13+20) = 0.
        FPACK(9+NWORD13+23) = 0.
        FPACK(9+NWORD13+24) = 0.
        FPACK(9+NWORD13+25) = 0.
      ENDIF
C---------------------------------------------------------------------
C***
C***  WRITE PROFILE DATA
C***
      
      NREC=IFHR*NUMSTA+N
c     print*,'FPACK(1),FPACK(2)=',FPACK(1),FPACK(2)
      print*,'NREC=',NREC,' is being written'
      WRITE(LCLAS1,REC=NREC)IHRST,IDAT,IFCST,ISTAT,CISTAT
     1,                    (FPACK(NL),NL=1,NLEN)
C---------------------------------------------------------------------
 1000 CONTINUE
      CLOSE(LCLAS1)
C
C***  END OF PROFILE SITE LOOP
C
C***  END PROFILE POSTING CODE.
C---------------------------------------------------------------------
      RETURN
      END
