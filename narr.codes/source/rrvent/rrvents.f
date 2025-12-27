C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      PROGRAM RRVENTS
 
      PARAMETER(MPP=50000,MXX=200000)
 
      CHARACTER*80 HDSTR,OBSTR,PVSTR,QVSTR,TVSTR,UVSTR,VVSTR
      CHARACTER*8  SUBSET,VAR,SID
      CHARACTER*8  PID(MPP),QID(MXX),TID(MXX),UID(MXX),VID(MXX)
 
      DIMENSION EVN(11)
      DIMENSION PCD(MPP),POO(MPP),PFC(MPP),PAN(MPP),PXX(MPP),PYY(MPP)
      DIMENSION PEL(MPP),PDH(MPP),PPP(MPP),PQM(MPP),PER(MPP),PRT(MPP)
      DIMENSION QCD(MXX),QOO(MXX),QFC(MXX),QAN(MXX),QXX(MXX),QYY(MXX)
      DIMENSION QEL(MXX),QDH(MXX),QPR(MXX),QQM(MXX),QER(MXX),QRT(MXX)
      DIMENSION TCD(MXX),TOO(MXX),TFC(MXX),TAN(MXX),TXX(MXX),TYY(MXX)
      DIMENSION TEL(MXX),TDH(MXX),TPR(MXX),TQM(MXX),TER(MXX),TRT(MXX)
      DIMENSION UCD(MXX),UOO(MXX),UFC(MXX),UAN(MXX),UXX(MXX),UYY(MXX)
      DIMENSION UEL(MXX),UDH(MXX),UPR(MXX),UQM(MXX),UER(MXX),URT(MXX)
      DIMENSION VCD(MXX),VOO(MXX),VFC(MXX),VAN(MXX),VXX(MXX),VYY(MXX)
      DIMENSION VEL(MXX),VDH(MXX),VPR(MXX),VQM(MXX),VER(MXX),VRT(MXX)
 
      EQUIVALENCE (HDR(1),SID)
 
      REAL*8    HDR(10)
      REAL*8    OBS(10,255),PEV(10,255),QEV(10,255),QMS(10,255)
      REAL*8    TEV(10,255),UEV(10,255),VEV(10,255)
 
      DATA HDSTR /'SID XOB YOB DHR ELV TYP    '/
      DATA OBSTR /'POB QOB TOB UOB VOB CAT ZOB'/
      DATA PVSTR /'POB PQM PRC PPC PFC PAN PER'/
      DATA QVSTR /'QOB QQM QRC QPC QFC QAN QER'/
      DATA TVSTR /'TOB TQM TRC TPC TFC TAN TER'/
      DATA UVSTR /'UOB WQM WRC WPC UFC UAN WER'/
      DATA VVSTR /'VOB NUL NUL NUL VFC VAN NUL'/
 
      DATA ANLCODE /10/
 
      DATA LUNEV /20/
      DATA LUNIN /21/
      DATA LUOUT /50/
      DATA BMISS /10E10/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  CLEAR THE SUMMARY COUNTERS
 
      NPP = 0 ; NPOB = 0 ; NP3DX = 0 ; NPR2X = 0
      NQQ = 0 ; NQOB = 0 ; NQ3DX = 0 ; NQR2X = 0
      NTT = 0 ; NTOB = 0 ; NT3DX = 0 ; NTR2X = 0
      NUU = 0 ; NUOB = 0 ; NU3DX = 0 ; NUR2X = 0
      NVV = 0 ; NVOB = 0 ; NV3DX = 0 ; NVR2X = 0
 
C  READ AND STORE THE ASCII TEXT EVENTS
C  ------------------------------------
 
10    READ(LUNEV,11,END=20,err=10) VAR,ICD,EVN,SID
11    FORMAT(A3,I3.3,11E13.5,2X,A8)
      III = LJUST(SID)
 
      IF(VAR.EQ.'PP.') THEN
         IF(NPP+1.GT.MPP) CALL BORT('TOO MANY P EVENTS')
         NPP = NPP+1
         PCD(NPP) = ICD
         POO(NPP) = EVN( 1)
         PFC(NPP) = EVN( 2)
         PAN(NPP) = EVN( 3)
         PXX(NPP) = EVN( 4)
         PYY(NPP) = EVN( 5)
         PEL(NPP) = EVN( 6)
         PDH(NPP) = EVN( 7)
         PPP(NPP) = EVN( 8)
         PQM(NPP) = EVN( 9)
         PER(NPP) = EVN(10)
         PRT(NPP) = EVN(11)
         PID(NPP) = SID
      ELSEIF(VAR.EQ.'TT.') THEN
         IF(NTT+1.GT.MXX) CALL BORT('TOO MANY T EVENTS')
         NTT = NTT+1
         TCD(NTT) = ICD
         TOO(NTT) = EVN( 1)-273.15
         TFC(NTT) = EVN( 2)-273.15
         TAN(NTT) = EVN( 3)-273.15
         TXX(NTT) = EVN( 4)
         TYY(NTT) = EVN( 5)
         TPR(NTT) = EVN( 6)
         TEL(NTT) = EVN( 7)
         TDH(NTT) = EVN( 8)
         TQM(NTT) = EVN( 9)
         TER(NTT) = EVN(10)
         TRT(NTT) = EVN(11)
         TID(NTT) = SID
      ELSEIF(VAR.EQ.'QQ.') THEN
         IF(NQQ+1.GT.MXX) CALL BORT('TOO MANY Q EVENTS')
         NQQ = NQQ+1
         QCD(NQQ) = ICD
         QOO(NQQ) = EVN( 1)*1.E6; qoo(nqq) = nint(qoo(nqq))
         QFC(NQQ) = EVN( 2)*1.E6; qfc(nqq) = nint(qfc(nqq))
         QAN(NQQ) = EVN( 3)*1.E6; qan(nqq) = nint(qan(nqq))
         QXX(NQQ) = EVN( 4)
         QYY(NQQ) = EVN( 5)
         QPR(NQQ) = EVN( 6)
         QEL(NQQ) = EVN( 7)
         QDH(NQQ) = EVN( 8)
         QQM(NQQ) = EVN( 9)
         QER(NQQ) = EVN(10)
         QRT(NQQ) = EVN(11)
         QID(NQQ) = SID
         if(qrt(nqq).eq.119.) nqq = nqq-1
      ELSEIF(VAR.EQ.'UU.') THEN
         IF(NUU+1.GT.MXX) CALL BORT('TOO MANY U EVENTS')
         NUU = NUU+1
         UCD(NUU) = ICD
         UOO(NUU) = EVN( 1)
         UFC(NUU) = EVN( 2)
         UAN(NUU) = EVN( 3)
         UXX(NUU) = EVN( 4)
         UYY(NUU) = EVN( 5)
         UPR(NUU) = EVN( 6)
         UEL(NUU) = EVN( 7)
         UDH(NUU) = EVN( 8)
         UQM(NUU) = EVN( 9)
         UER(NUU) = EVN(10)
         URT(NUU) = EVN(11)
         UID(NUU) = SID
      ELSEIF(VAR.EQ.'VV.') THEN
         IF(NVV+1.GT.MXX) CALL BORT('TOO MANY V EVENTS')
         NVV = NVV+1
         VCD(NVV) = ICD
         VOO(NVV) = EVN( 1)
         VFC(NVV) = EVN( 2)
         VAN(NVV) = EVN( 3)
         VXX(NVV) = EVN( 4)
         VYY(NVV) = EVN( 5)
         VPR(NVV) = EVN( 6)
         VEL(NVV) = EVN( 7)
         VDH(NVV) = EVN( 8)
         VQM(NVV) = EVN( 9)
         VER(NVV) = EVN(10)
         VRT(NVV) = EVN(11)
         VID(NVV) = SID
c     ELSE
c        CALL BORT('UNKNOWN EVENT TYPE '//VAR)
      ENDIF
      GOTO 10
 
C  OPEN INPUT AND OUTPUT PREPBUFR FILES
C  ------------------------------------
 
20    CALL OPENBF(LUNIN,'IN ',LUNIN)
      CALL OPENBF(LUOUT,'OUT',LUNIN)
 
C  NOW GO THROUGH A PREPBUFR FILE AND ENCODE THESE EVENTS
C  ------------------------------------------------------
 
      DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)
      DO WHILE(IREADSB(LUNIN).EQ.0)
 
      CALL UFBINT(LUNIN,HDR,10,1,IRET,HDSTR)
 
      DO I=1,8
      IF(ICHAR(SID(I:I)).EQ.00) SID(I:I) = ' '
      ENDDO
      III = LJUST(SID)
 
      XOB = HDR(2)
      YOB = HDR(3)
      DHR = HDR(4)
      ELV = HDR(5)
      RTP = HDR(6)
 
C  INTERROGATE VARIABLES FOR EACH OB AT EACH LEVEL - STORE EVENTS
C  --------------------------------------------------------------
 
      CALL UFBINT(LUNIN,OBS,10,255,NLEV,OBSTR)
      CALL UFBINT(LUNIN,QMS,10,255,NLEV,'PQM QQM TQM WQM')
 
      PEV = BMISS
      QEV = BMISS
      TEV = BMISS
      UEV = BMISS
      VEV = BMISS
 
      DO L=1,NLEV
      POB = OBS(1,L)
      QOB = OBS(2,L)
      TOB = OBS(3,L)
      UOB = OBS(4,L)
      VOB = OBS(5,L)
      CAT = OBS(6,L)
      ELV = OBS(7,L)
 
      QMP = QMS(1,L)
      QMQ = QMS(2,L)
      QMT = QMS(3,L)
      QMW = QMS(4,L)
 
C  CHECK FOR A SURFACE PRESSURE OB EVENT
C  -------------------------------------
 
      IF(CAT.EQ.0) THEN
         NPOB = NPOB+1
         DO N=1,NPP
         IF(SID.EQ.PID(N) .AND. PCD(N).GE.0) THEN
            IF(XOB.EQ.PXX(N) .AND. YOB.EQ.PYY(N) .AND.
     .         DHR.EQ.PDH(N) .AND. RTP.EQ.PRT(N)) THEN
               IF(PCD(N).NE.0) THEN  ! STORE 3DVAR QC REJECTION CODES
                  PEV(1,L) = POB     ! OBSERVED VALUE
                  PEV(2,L) = 10      ! 3DVAR QC REJECT CODE
                  PEV(3,L) = PCD(N)  ! 3DVAR QC REASON CODE
                  PEV(4,L) = ANLCODE ! 3DVAR PROGRAM CODE
               ENDIF
               DP =  POO(N)/POB
               PEV(5,L) = PFC(N)/DP  ! FIRST GUESS
               PEV(6,L) = PAN(N)/DP  ! ANALYSIS
               PEV(7,L) = PER(N)     ! OB ERROR
               PCD(N) = -1
               GOTO 21
            ENDIF
         ENDIF
         ENDDO
         ! NO EVENT - EITHER REJECTED BY R2 QC OR 3DVAR APRIORI FILTER
         IF(QMP.LT.4) THEN     ! STORE FILTER EVENT CODES
            PEV(1,L) = POB     ! OBSERVED VALUE
            PEV(2,L) = 9       ! 3DVAR FILTER CODE
            PEV(3,L) = 9       ! 3DVAR REASON CODE
            PEV(4,L) = ANLCODE ! 3DVAR PROGRAM CODE
            NP3DX = NP3DX+1
         ELSE
            NPR2X = NPR2X+1
         ENDIF
      ENDIF
 
C  CHECK FOR A SPECIFIC HUMIDITY OB EVENT
C  --------------------------------------
 
21    IF(QOB.LT.BMISS) THEN
         NQOB = NQOB+1
         DO N=1,NQQ
         IF(SID.EQ.QID(N) .AND. QCD(N).GE.0) THEN
            IF(XOB.EQ.QXX(N) .AND. YOB.EQ.QYY(N) .AND.
     .         RTP.EQ.QRT(N) .AND. DHR.EQ.QDH(N) .AND.
     .         ABS(QOB-QOO(N)).LT..001) THEN
               IF(QCD(N).NE.0) THEN  ! STORE 3DVAR QC REJECTION CODES
                  QEV(1,L) = QOB     ! OBSERVED VALUE
                  QEV(2,L) = 10      ! 3DVAR QC REJECT CODE
                  QEV(3,L) = QCD(N)  ! 3DVAR QC REASON CODE
                  QEV(4,L) = ANLCODE ! 3DVAR PROGRAM CODE
               ENDIF
               QEV(5,L) = QFC(N) ! FIRST GUESS
               QEV(6,L) = QAN(N) ! ANALYSIS
               QEV(7,L) = QER(N) ! OB ERROR
               QCD(N) = -1
               GOTO 22
            ENDIF
         ENDIF
         ENDDO
         ! NO EVENT - EITHER REJECTED BY R2 QC OR 3DVAR APRIORI FILTER
         IF(QMQ.LT.4) THEN     ! STORE FILTER EVENT CODES
            QEV(1,L) = QOB     ! OBSERVED VALUE
            QEV(2,L) = 9       ! 3DVAR FILTER MARK
            QEV(3,L) = 9       ! 3DVAR REASON CODE
            QEV(4,L) = ANLCODE ! 3DVAR PROGRAM CODE
            NQ3DX = NQ3DX+1
         ELSE
            NQR2X = NQR2X+1
         ENDIF
      ENDIF
 
C  CHECK FOR A TEMPERATURE OB EVENT
C  --------------------------------
 
22    IF(TOB.LT.BMISS) THEN
         NTOB = NTOB+1
         DO N=1,NTT
         IF(SID.EQ.TID(N).AND.TCD(N).GE.0) THEN
            IF(XOB.EQ.TXX(N) .AND. YOB.EQ.TYY(N) .AND.
     .         RTP.EQ.TRT(N) .AND. DHR.EQ.TDH(N) .AND.
     .         POB.EQ.TPR(N)) THEN
               IF(TCD(N).NE.0) THEN   ! STORE 3DVAR QC REJECTION CODES
                  TEV(1,L) = TOB      ! OBSERVED VALUE
                  TEV(2,L) = 10       ! 3DVAR QC REJECT CODE
                  TEV(3,L) = TCD(N)   ! 3DVAR QC REASON CODE
                  TEV(4,L) = ANLCODE  ! 3DVAR PROGRAM CODE
               ENDIF
               TEV(5,L) = TFC(N)  ! FIRST GUESS
               TEV(6,L) = TAN(N)  ! ANALYSIS
               TEV(7,L) = TER(N)  ! OB ERROR
               TCD(N) = -1
               GOTO 23
            ENDIF
         ENDIF
         ENDDO
         ! NO EVENT - EITHER REJECTED BY R2 QC OR 3DVAR APRIORI FILTER
         IF(QMT.LT.4) THEN     ! STORE FILTER EVENT CODES
            TEV(1,L) = TOB     ! OBSERVED VALUE
            TEV(2,L) = 9       ! 3DVAR FILTER MARK
            TEV(3,L) = 9       ! 3DVAR REASON CODE
            TEV(4,L) = ANLCODE ! 3DVAR PROGRAM CODE
            NT3DX = NT3DX+1
         ELSE
            NTR2X = NTR2X+1
         ENDIF
      ENDIF
 
C  CHECK FOR A U COMPONENT OB
C  --------------------------
 
23    IF(UOB.LT.BMISS) THEN
         NUOB = NUOB+1
         DO N=1,NUU
         IF(SID.EQ.UID(N).AND.UCD(N).GE.0) THEN
            IF(XOB.EQ.UXX(N) .AND. YOB.EQ.UYY(N) .AND.
     .         RTP.EQ.URT(N) .AND. DHR.EQ.UDH(N) .AND.
     .         POB.EQ.UPR(N) .AND. UOB.EQ.UOO(N)) THEN
               IF(UCD(N).NE.0) THEN   ! STORE 3DVAR QC REJECTION CODES
                  UEV(1,L) = UOB      ! OBSERVED VALUE
                  UEV(2,L) = 10       ! 3DVAR QC REJECT CODE
                  UEV(3,L) = UCD(N)   ! 3DVAR QC REASON CODE
                  UEV(4,L) = ANLCODE  ! 3DVAR PROGRAM CODE
               ENDIF
               UEV(5,L) = UFC(N)  ! FIRST GUESS
               UEV(6,L) = UAN(N)  ! ANALYSIS
               UEV(7,L) = UER(N)  ! OB ERROR
               UCD(N) = -1
               GOTO 24
            ENDIF
         ENDIF
         ENDDO
         ! NO EVENT - EITHER REJECTED BY R2 QC OR 3DVAR APRIORI FILTER
         IF(QMW.LT.4) THEN     ! STORE FILTER EVENT CODES
            UEV(1,L) = UOB     ! OBSERVED VALUE
            UEV(2,L) = 9       ! 3DVAR FILTER MARK
            UEV(3,L) = 9       ! 3DVAR REASON CODE
            UEV(4,L) = ANLCODE ! 3DVAR PROGRAM CODE
            NU3DX = NU3DX+1
         ELSE
            NUR2X = NUR2X+1
         ENDIF
      ENDIF
 
C  CHECK FOR A V COMPONENT OB
C  --------------------------
 
24    IF(VOB.LT.BMISS) THEN
         NVOB = NVOB+1
         DO N=1,NVV
         IF(SID.EQ.VID(N).AND.VCD(N).GE.0) THEN
            IF(XOB.EQ.VXX(N) .AND. YOB.EQ.VYY(N) .AND.
     .         RTP.EQ.VRT(N) .AND. DHR.EQ.VDH(N) .AND.
     .         POB.EQ.VPR(N) .AND. VOB.EQ.VOO(N)) THEN
               IF(VCD(N).NE.0) THEN   ! STORE 3DVAR QC REJECTION CODES
                  VEV(1,L) = VOB      ! OBSERVED SPECIFIC HUMIDITY
                  VEV(2,L) = 10       ! 3DVAR QC REJECT CODE
                  VEV(3,L) = VCD(N)   ! 3DVAR QC REASON CODE
                  VEV(4,L) = ANLCODE  ! 3DVAR PROGRAM CODE
               ENDIF
               VEV(5,L) = VFC(N)  ! FIRST GUESS
               VEV(6,L) = VAN(N)  ! ANALYSIS
               VEV(7,L) = VER(N)  ! OB ERROR
               VCD(N) = -1
               GOTO 25
            ENDIF
         ENDIF
         ENDDO
         ! NO EVENT - EITHER REJECTED BY R2 QC OR 3DVAR APRIORI FILTER
         IF(QMW.LT.4) THEN     ! STORE FILTER EVENT CODES
            VEV(1,L) = VOB     ! OBSERVED VALUE
            VEV(2,L) = 9       ! 3DVAR FILTER MARK
            VEV(3,L) = 9       ! 3DVAR REASON CODE
            VEV(4,L) = ANLCODE ! 3DVAR PROGRAM CODE
            NV3DX = NV3DX+1
         ELSE
            NVR2X = NVR2X+1
         ENDIF
      ENDIF
25    CONTINUE

C  MAKE SURE UEV AND VEV ARE COORDINATED PROPERLY
C  ----------------------------------------------
 
      IF(UEV(1,L).LT.BMISS .AND. VEV(1,L).GE.BMISS) THEN
         VEV(1,L) = VOB
      ELSEIF(UEV(1,L).GE.BMISS .AND. VEV(1,L).LT.BMISS) THEN
         UEV(1,L) = UOB
         UEV(2,L) = VEV(2,L)
         UEV(3,L) = VEV(3,L)
         UEV(4,L) = VEV(4,L)
      ENDIF
 
      ENDDO ! END OF REPORT LEVEL LOOP
 
C  WRITE THIS REPORT WITH EVENTS INTO THE OUTPUT FILE
C  --------------------------------------------------
 
      CALL OPENMB(LUOUT,SUBSET,IDATE)
      CALL UFBCPY(LUNIN,LUOUT)
      CALL UFBINT(LUOUT,PEV,10,NLEV,IRET,PVSTR)
      CALL UFBINT(LUOUT,QEV,10,NLEV,IRET,QVSTR)
      CALL UFBINT(LUOUT,TEV,10,NLEV,IRET,TVSTR)
      CALL UFBINT(LUOUT,UEV,10,NLEV,IRET,UVSTR)
      CALL UFBINT(LUOUT,VEV,10,NLEV,IRET,VVSTR)
      CALL WRITSB(LUOUT)
 
C  END OF READ/WRITE LOOP
C  ----------------------
 
      ENDDO ! END OF READSB LOOP
      ENDDO ! END OF READMG LOOP
 
      CALL CLOSBF(LUOUT)
 
C  WRITE A SUMMARY OF EVENTS FOR EACH VARIABLE
C  -------------------------------------------
 
      IPSUM = NPOB-(NPP+NP3DX+NPR2X)
      IQSUM = NQOB-(NQQ+NQ3DX+NQR2X)
      ITSUM = NTOB-(NTT+NT3DX+NTR2X)
      IUSUM = NUOB-(NUU+NU3DX+NUR2X)
      IVSUM = NVOB-(NVV+NV3DX+NVR2X)

      print'(1x, 5a10)','totobs','totevn','3dfltr','r2fltr','cksum'
      PRINT'("p",5I10)',NPOB,NPP,NP3DX,NPR2X,IPSUM 
      PRINT'("q",5I10)',NQOB,NQQ,NQ3DX,NQR2X,IQSUM
      PRINT'("t",5I10)',NTOB,NTT,NT3DX,NTR2X,ITSUM
      PRINT'("u",5I10)',NUOB,NUU,NU3DX,NUR2X,IUSUM 
      PRINT'("v",5I10)',NVOB,NVV,NV3DX,NVR2X,IVSUM

      if(ipsum.ne.0.or.iqsum.ne.0.or.itsum.ne.0.or.
     .   iusum.ne.0.or.ivsum.ne.0)
     .print*,'>>>>>>>>>>>rrvent checksum error<<<<<<<<<<'

      if(nuu.ne.nvv) 
     .print*,'>>>>>>>>>>>rrvent uv event error<<<<<<<<<<'

      STOP
      END
