c**********************************************************************c
      SUBROUTINE CALVIS(QV,QC,QR,QI,QS,TT,PP,VIS)
c
c   This routine computes horizontal visibility at the
c   surface or lowest model layer, from qc, qr, qi, and qs.  
c   qv--water vapor mixing ratio (kg/kg)
c   qc--cloud water mixing ratio (kg/kg)
c   qr--rain water mixing ratio  (kg/kg)
c   qi--cloud ice mixing ratio   (kg/kg)
c   qs--snow mixing ratio        (kg/kg)
c   tt--temperature              (k)
c   pp--pressure                 (Pa)
c
c   If iice=0:
c      qprc=qr     qrain=qr and qclw=qc if T>0C
c      qcld=qc          =0          =0  if T<0C
c                  qsnow=qs and qclice=qc  if T<0C
c                       =0            =0   if T>0C
c   If iice=1:
c      qprc=qr+qs   qrain=qr and qclw=qc
c      qcld=qc+qi   qsnow=qs and qclice=qc
c
c   Independent of the above definitions, the scheme can use different
c   assumptions of the state of hydrometeors:
c        meth='d': qprc is all frozen if T<0, liquid if T>0
c        meth='b': Bocchieri scheme used to determine whether qprc
c           is rain or snow. A temperature assumption is used to
c           determine whether qcld is liquid or frozen.
c        meth='r': Uses the four mixing ratios qrain, qsnow, qclw,
c           and qclice
c
c   The routine uses the following
c   expressions for extinction coefficient, beta (in km**-1),
c   with C being the mass concentration (in g/m**3):
c
c      cloud water:  beta = 144.7 * C ** (0.8800)
c      rain water:   beta =  2.24 * C ** (0.7500)
c      cloud ice:    beta = 327.8 * C ** (1.0000)
c      snow:         beta = 10.36 * C ** (0.7776)
c
c   These expressions were obtained from the following sources:
c
c      for cloud water: from Kunkel (1984)
c      for rainwater: from M-P dist'n, with No=8e6 m**-4 and
c         rho_w=1000 kg/m**3
c      for cloud ice: assume randomly oriented plates which follow
c         mass-diameter relationship from Rutledge and Hobbs (1983)
c      for snow: from Stallabrass (1985), assuming beta = -ln(.02)/vis
c
c   The extinction coefficient for each water species present is
c   calculated, and then all applicable betas are summed to yield
c   a single beta. Then the following relationship is used to
c   determine visibility (in km), where epsilon is the threshhold
c   of contrast, usually taken to be .02:
c
c      vis = -ln(epsilon)/beta      [found in Kunkel (1984)]
c
C------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "params"
      INCLUDE "CTLBLK.comm"
C
      REAL QV(IM,JM),QC(IM,JM),QI(IM,JM),QR(IM,JM),QS(IM,JM)
     1,    TT(IM,JM),PP(IM,JM),VIS(IM,JM)
      CHARACTER METH*1
C------------------------------------------------------------------
C------------------------------------------------------------------
      CELKEL=273.15
      TICE=CELKEL-10.
      COEFLC=144.7
      COEFLP=2.24
      COEFFC=327.8
      COEFFP=10.36
      EXPONLC=0.8800
      EXPONLP=0.7500
      EXPONFC=1.0000
      EXPONFP=0.7776
      CONST1=-LOG(.02)
      RHOICE=917.
      RHOWAT=1000.
C
      DO J=JSTA,JEND
      DO I=1,IM
c       IF(IICE.EQ.0)THEN
c         QPRC=QR
c         QCLD=QC
c         IF(TT.LT.CELKEL)THEN
c           QRAIN=0.
c           QSNOW=QPRC
c           QCLW=0.
c           QCLICE=QCLD
c         ELSE
c           QRAIN=QPRC
c           QSNOW=0.
c           QCLW=QCLD
c           QCLICE=0.
c         ENDIF
c       ELSE
          QPRC=QR(I,J)+QS(I,J)
          QCLD=QC(I,J)+QI(I,J)
          QRAIN=QR(I,J)
          QSNOW=QS(I,J)
          QCLW=QC(I,J)
          QCLICE=QI(I,J)
c       ENDIF
c       TV=VIRTUAL(TT,QV)
        TV=TT(I,J)*(H1+D608*QV(I,J))
        RHOAIR=PP(I,J)/(RD*TV)
c       IF(METH.EQ.'D')THEN
c         IF(TT.LT.CELKEL)THEN
c           VOVERMD=(1.+QV)/RHOAIR+(QPRC+QCLD)/RHOICE
c           CONCLC = 0.
c           CONCLP = 0.
c           CONCFC = QCLD/VOVERMD*1000.
c           CONCFP = QPRC/VOVERMD*1000.
c         ELSE
c           VOVERMD=(1.+QV)/RHOAIR+(QPRC+QCLD)/RHOWAT
c           CONCLC = QCLD/VOVERMD*1000.
c           CONCLP = QPRC/VOVERMD*1000.
c           CONCFC = 0.
c           CONCFP = 0.
c         ENDIF
c       ELSEIF(METH.EQ.'B')THEN
c         IF(TT.LT.TICE)THEN
c           VOVERMD=(1.+QV)/RHOAIR+(QPRC+QCLD)/RHOICE
c           CONCLC = 0.
c           CONCLP = 0.
c           CONCFC = QCLD/VOVERMD*1000.
c           CONCFP = QPRC/VOVERMD*1000.
c         ELSEIF(PRSNOW.GE.50.)THEN
c           VOVERMD=(1.+QV)/RHOAIR+QPRC/RHOICE+QCLD/RHOWAT
c           CONCLC = QCLD/VOVERMD*1000.
c           CONCLP = 0.
c           CONCFC = 0.
c           CONCFP = QPRC/VOVERMD*1000.
c         ELSE
c           VOVERMD=(1.+QV)/RHOAIR+(QPRC+QCLD)/RHOWAT
c           CONCLC = QCLD/VOVERMD*1000.
c           CONCLP = QPRC/VOVERMD*1000.
c           CONCFC = 0.
c           CONCFP = 0.
c         ENDIF
c       ELSEIF(METH.EQ.'R')THEN
          VOVERMD=(1.+QV(I,J))/RHOAIR+(QCLW+QRAIN)/RHOWAT+
     1            (QCLICE+QSNOW)/RHOICE
          CONCLC=QCLW/VOVERMD*1000.
          CONCLP=QRAIN/VOVERMD*1000.
          CONCFC=QCLICE/VOVERMD*1000.
          CONCFP=QSNOW/VOVERMD*1000.
c       ENDIF
        BETAV=COEFFC*CONCFC**EXPONFC+COEFFP*CONCFP**EXPONFP
     1       +COEFLC*CONCLC**EXPONLC+COEFLP*CONCLP**EXPONLP
     2       +1.E-10
c CHANGED GSM 3-10-00 -->  no point in distinguishing values
c       above 20 km, so make that value the max (prev max was 80)
        VIS(I,J)=1.E3*MIN(20.,CONST1/BETAV)   ! max of 20km
      ENDDO
      ENDDO
C
      RETURN
      END
