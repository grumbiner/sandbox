      SUBROUTINE ZNLDIA(NZNL,RHOUR,IDATE,KDT,LATB2,LEVS,
     &                  WGB,COLRAB,DEL)
C
      DIMENSION IDATE(4)
      DIMENSION WGB(LATB2),COLRAB(LATB2),DEL(LEVS)
C
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
C
      DIMENSION ZNLM(NLB,LEVS+1,NRM)
      DIMENSION ZNLS(NLB,NST,NRS)
      DIMENSION WEIM(NLB),WEIS(NLB,NST)
C.......................................................................
      WEIM=0.
      WEIS=0.
      ZNLM=0.
      ZNLS=0.
C
      DO LAT=1,LATB2
        NLBH=NLB/2
        WLB= 3.141593E+0 /(2*NLBH)
        JB=COLRAB(LAT)/WLB+1
        JN=JB+1
        JS=NLB-JB+1
C
        WEIM(JN)=WEIM(JN)+ZWM(1,LAT)*WGB(LAT)
        WEIM(JS)=WEIM(JS)+ZWM(2,LAT)*WGB(LAT)
        DO N=1,NRM
          IF(ZHM(N).GT.0.) THEN
            W=WGB(LAT)*ZHM(N)
            DO K=1,LEVS
              ZNLM(JN,K,N)=ZNLM(JN,K,N)+ZDM(1,K,N,LAT)*W
              ZNLM(JS,K,N)=ZNLM(JS,K,N)+ZDM(2,K,N,LAT)*W
            ENDDO
          ENDIF
        ENDDO
C
        DO K=1,NST
          WEIS(JN,K)=WEIS(JN,K)+ZWS(1,K,LAT)*WGB(LAT)
          WEIS(JS,K)=WEIS(JS,K)+ZWS(2,K,LAT)*WGB(LAT)
        ENDDO
        DO N=1,NRS
          DO K=1,NST
            ZNLS(JN,K,N)=ZNLS(JN,K,N)+ZDS(1,K,N,LAT)*WGB(LAT)
            ZNLS(JS,K,N)=ZNLS(JS,K,N)+ZDS(2,K,N,LAT)*WGB(LAT)
          ENDDO
        ENDDO
      ENDDO
C
      DO J=2,NLB
        WEIM(1)=WEIM(1)+WEIM(J)
      ENDDO
      DO K=1,LEVS
        DO J=2,NLB
          DO N=1,NRM
            ZNLM(1,K,N)=ZNLM(1,K,N)+ZNLM(J,K,N)
          ENDDO
        ENDDO
      ENDDO
      DO J=1,NLB
        DO K=1,LEVS
          DO N=1,NRM
            ZNLM(J,LEVS+1,N)=ZNLM(J,LEVS+1,N)+ZNLM(J,K,N)*DEL(K)
          ENDDO
        ENDDO
      ENDDO
      DO K=1,LEVS+1
        DO J=1,NLB
          DO N=1,NRM
            IF(WEIM(J).NE.0.) ZNLM(J,K,N)=ZNLM(J,K,N)/WEIM(J)
          ENDDO
        ENDDO
      ENDDO
C
      DO J=2,NLB
        DO K=1,NST
          WEIS(1,K)=WEIS(1,K)+WEIS(J,K)
        ENDDO
      ENDDO
      DO K=1,NST
        DO J=2,NLB
          DO N=1,NRS
            ZNLS(1,K,N)=ZNLS(1,K,N)+ZNLS(J,K,N)
          ENDDO
        ENDDO
      ENDDO
      DO K=1,NST
        DO J=1,NLB
          DO N=1,NRS
            IF(WEIS(J,K).NE.0.) ZNLS(J,K,N)=ZNLS(J,K,N)/WEIS(J,K)
          ENDDO
        ENDDO
      ENDDO
C
      DO J=1,NLB
        IF(WEIS(1,1).NE.0.) ZNLS(J,1,NSSLMSK)=100.*WEIS(J,1)/WEIS(1,1)
      ENDDO
      DO K=2,NST
        DO J=1,NLB
          IF(WEIS(J,1).NE.0.) ZNLS(J,K,NSSLMSK)=100.*WEIS(J,K)/WEIS(J,1)
        ENDDO
      ENDDO
C
      CALL ZNLPRT(NZNL,RHOUR,IDATE,KDT,LEVS+1,ZNLM,ZNLS)
C
      RETURN
      END
