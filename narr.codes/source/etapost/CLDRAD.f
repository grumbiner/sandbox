      SUBROUTINE CLDRAD(IMOUT,JMOUT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CLDRAD       POST SNDING/CLOUD/RADTN FIELDS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-08-30       
C     
C ABSTRACT:  THIS ROUTINE COMPUTES/POSTS SOUNDING, CLOUD 
C   RELATED, AND RADIATION FIELDS.  UNDER THE HEADING OF 
C   SOUNDING FIELDS FALL THE THREE ETA MODEL LIFTED INDICES,
C   CAPE, CIN, AND TOTAL COLUMN PRECIPITABLE WATER.
C
C   THE THREE ETA MODEL LIFTED INDICES DIFFER ONLY IN THE
C   DEFINITION OF THE PARCEL TO LIFT.  ONE LIFTS PARCELS FROM
C   THE LOWEST ABOVE GROUND ETA LAYER.  ANOTHER LIFTS MEAN 
C   PARCELS FROM ANY OF NBND BOUNDARY LAYERS (SEE SUBROUTINE
C   BNDLYR2).  THE FINAL TYPE OF LIFTED INDEX IS A BEST LIFTED
C   INDEX BASED ON THE NBND BOUNDARY LAYER LIFTED INDICES.
C
C   TWO TYPES OF CAPE/CIN ARE AVAILABLE.  ONE IS BASED ON PARCELS
C   IN THE LOWEST ETA LAYER ABOVE GROUND.  THE OTHER IS BASED 
C   ON A LAYER MEAN PARCEL IN THE N-TH BOUNDARY LAYER ABOVE 
C   THE GROUND.  SEE SUBROUTINE CALCAPE FOR DETAILS.
C
C   THE CLOUD FRACTION AND LIQUID CLOUD WATER FIELDS ARE DIRECTLY
C   FROM THE MODEL WITH MINIMAL POST PROCESSING.  THE LIQUID 
C   CLOUD WATER, 3-D CLOUD FRACTION, AND TEMPERATURE TENDENCIES
C   DUE TO PRECIPITATION ARE NOT POSTED IN THIS ROUTINE.  SEE
C   SUBROUTINE ETAFLD2 FOR THESE FIELDS.  LIFTING CONDENSATION
C   LEVEL HEIGHT AND PRESSURE ARE COMPUTED AND POSTED IN
C   SUBROUTINE MISCLN.  
C
C   THE RADIATION FIELDS POSTED BY THIS ROUTINE ARE THOSE COMPUTED
C   DIRECTLY IN THE MODEL.
C     
C PROGRAM HISTORY LOG:
C   93-08-30  RUSS TREADON
C   94-08-04  MICHAEL BALDWIN - ADDED OUTPUT OF INSTANTANEOUS SFC
C                               FLUXES OF NET SW AND LW DOWN RADIATION
C   97-04-25  MICHAEL BALDWIN - FIX PDS FOR PRECIPITABLE WATER
C   97-04-29  GEOFF MANIKIN - MOVED CLOUD TOP TEMPS CALCULATION
C                               TO THIS SUBROUTINE.  CHANGED METHOD
C                               OF DETERMINING WHERE CLOUD BASE AND
C                               TOP ARE FOUND AND ADDED HEIGHT OPTION
C                               FOR TOP AND BASE.
C   98-04-29  GEOFF MANIKIN - CHANGED VALUE FOR CLOUD BASE/TOP PRESSURES
C                               AND HEIGHTS FROM SPVAL TO -500
C   98-06-15  T BLACK       - CONVERSION FROM 1-D TO 2-D
C   98-07-17  MIKE BALDWIN  - REMOVED LABL84
C   00-01-04  JIM TUCCILLO  - MPI VERSION
C   00-02-22  GEOFF MANIKIN - CHANGED VALUE FOR CLOUD BASE/TOP PRESSURES
C                               AND HEIGHTS FROM SPVAL TO -500 (WAS NOT IN
C                               PREVIOUS IBM VERSION)
C
C
C     
C USAGE:    CALL CLDRAD(IMOUT,JMOUT)
C   INPUT ARGUMENT LIST:
C     IMOUT    - FIRST DIMENSION OF OUTPUT GRID.
C     JMOUT    - SECOND DIMENSION OF OUTPUT GRID.
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - RQSTFLD
C                  EXTRA
C                  MAPOT
C                  PHYS
C                  LOOPS
C                  MASKS
C                  VRBLS
C                  CNVCLD
C                  CTLBLK
C                  ACMCLD
C                  ACMRDS
C                  ACMRDL
C                  OPTIONS
C                  CLDWTR
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
C
C     INCLUDE GRID DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
C     
      INCLUDE "parmeta"
      INCLUDE "parmout"
      INCLUDE "params"
      INCLUDE "parm.tbl"
C     
C     SET CELSIUS TO KELVIN CONVERSION.
      PARAMETER (C2K=273.15)
C     
C     DECLARE VARIABLES.
C     
      LOGICAL RUN,FIRST,RESTRT,SIGMA,OLDRD,STDRD
      LOGICAL NEED(IM,JM)
      INTEGER L1D(IM,JM)
      REAL EGRID1(IM,JM),EGRID2(IM,JM)
      REAL GRID1(IMOUT,JMOUT),GRID2(IMOUT,JMOUT),CLDTP(IM,JM),
     &        CLDTZ(IM,JM),CLDBP(IM,JM),CLDBZ(IM,JM),CLDTT(IM,JM)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "RQSTFLD.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "PHYS1.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "CTLBLK.comm"
      INCLUDE "ACMCLD.comm"
      INCLUDE "ACMRDS.comm"
      INCLUDE "ACMRDL.comm"
      INCLUDE "OPTIONS.comm"
      INCLUDE "CLDWTR.comm"
C     
C
C*************************************************************************
C     START CLDRAD HERE.
C     
C***  BLOCK 1.  SOUNDING DERIVED FIELDS.
C     
C     ETA SURFACE TO 500MB LIFTED INDEX.  TO BE CONSISTENT WITH THE

C     LFM AND NGM POSTING WE ADD 273.15 TO THE LIFTED INDEX
C     NOTE:  25 JUNE 1993, RUSS TREADON.
C               ON THE LFM FORECAST GRID (026) WE POST
C               THE FIRST BOUNDARY LAYER LIFTED INDEX.
C               SEE SUBROUTINE MISCLN.
C
C     THE BEST (SIX LAYER) AND BOUNDARY LAYER LIFTED INDICES ARE
C     COMPUTED AND POSTED IN SUBROUTINE MISCLN.
      print*,'Start of CLDRAD'
C
      IF ( (IGET(030).GT.0).AND.(KGTYPE.NE.026) ) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J) = SPVAL
         ENDDO
         ENDDO
C
         CALL OTLIFT2(T500,EGRID1)
C
         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J) = EGRID1(I,J) + C2K
         ENDDO
         ENDDO
C
         CALL E2OUT(030,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25)=0
         ID(10)  =50
         ID(11)  =100
         CALL OUTPUT(IOUTYP,IGET(030),LVLS(1,IGET(030)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     SOUNDING DERIVED AREA INTEGRATED ENERGIES - CAPE AND CIN.
      IF ((IGET(032).GT.0).OR.(IGET(107).GT.0)) THEN
         IF ( (LVLS(1,IGET(032)).GT.0) .OR. 
     X        (LVLS(1,IGET(107)).GT.0) ) THEN
            ITYPE = 1
            CALL CALCAPE(ITYPE,P1D,T1D,Q1D,L1D,EGRID1,EGRID2)
C
C           CONVECTIVE AVAILABLE POTENTIAL ENERGY.
            IF (IGET(032).GT.0) THEN
               CALL E2OUT(032,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
               CALL BOUND(GRID1,D00,H99999,IMOUT,JMOUT)
               ID(1:25)=0
               CALL OUTPUT(IOUTYP,IGET(032),LVLS(1,IGET(032)),
     X              GRID1,IMOUT,JMOUT)
            ENDIF
C
C           CONVECTIVE INHIBITION.
            IF (IGET(107).GT.0) THEN
               CALL E2OUT(107,000,EGRID2,EGRID1,GRID1,GRID2,IMOUT,JMOUT)
               DO J=JSTA,JEND
               DO I=1,IMOUT
                 GRID1(I,J) = -1.*GRID1(I,J)
               ENDDO
               ENDDO
               CALL BOUND(GRID1,D00,H99999,IMOUT,JMOUT)
               DO J=JSTA,JEND
               DO I=1,IMOUT
                 GRID1(I,J) = -1.*GRID1(I,J)
               ENDDO
               ENDDO
               ID(1:25)=0
               CALL OUTPUT(IOUTYP,IGET(107),LVLS(1,IGET(107)),
     X              GRID1,IMOUT,JMOUT)
            ENDIF
         ENDIF
      ENDIF
C
C     TOTAL COLUMN PRECIPITABLE WATER.
      IF (IGET(080).GT.0) THEN
         CALL CALPW(EGRID1)
         CALL E2OUT(080,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25)=0
         CALL BOUND(GRID1,D00,H99999,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(080),LVLS(1,IGET(080)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C   
C
C
C***  BLOCK 2.  2-D CLOUD FIELDS.
C
C     LOW CLOUD FRACTION.
      IF (IGET(037).GT.0) THEN
         CALL E2OUT(037,000,CFRACL,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25)=0
         CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(037),LVLS(1,IGET(037)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     MIDDLE CLOUD FRACTION.
      IF (IGET(038).GT.0) THEN
         CALL E2OUT(038,000,CFRACM,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25)=0
         CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(038),LVLS(1,IGET(038)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     HIGH CLOUD FRACTION.
      IF (IGET(039).GT.0) THEN
         CALL E2OUT(039,000,CFRACH,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25)=0
         CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(039),LVLS(1,IGET(039)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     TOTAL CLOUD FRACTION (INSTANTANEOUS).
      IF (IGET(161).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=AMAX1(CFRACL(I,J),
     1                 AMAX1(CFRACM(I,J),CFRACH(I,J)))
         ENDDO
         ENDDO
         CALL E2OUT(161,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25)=0
         CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(161),LVLS(1,IGET(161)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     TIME AVERAGED TOTAL CLOUD FRACTION.
         IF (IGET(144).GT.0) THEN
            DO 40 J=JSTA,JEND
            DO 40 I=1,IM
               ISUM = NCFRST(I,J)+NCFRCV(I,J)
               IF (ISUM.GT.0) THEN
                  EGRID1(I,J)=(ACFRST(I,J)+ACFRCV(I,J))/ISUM
               ELSE
                  EGRID1(I,J) = D00
               ENDIF
 40         CONTINUE
C
            CALL E2OUT(144,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)= 0
            IFHR       = NTSD/TSPH+0.5
            ITCLOD     = INT(TCLOD)
            IFINCR     = MOD(IFHR,ITCLOD)
            ID(19)  = IFHR
            ID(20)  = 3
            IF (IFINCR.EQ.0) THEN
               ID(18)  = IFHR-ITCLOD
            ELSE
               ID(18)  = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(144),LVLS(1,IGET(144)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C     TIME AVERAGED STRATIFORM CLOUD FRACTION.
         IF (IGET(139).GT.0) THEN
            DO 50 J=JSTA,JEND
            DO 50 I=1,IM
               IF (NCFRST(I,J).GT.0) THEN
                  EGRID1(I,J) = ACFRST(I,J)/NCFRST(I,J)
               ELSE
                  EGRID1(I,J) = D00
               ENDIF
 50         CONTINUE
C
            CALL E2OUT(139,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            IFHR       = NTSD/TSPH+0.5
            ITCLOD     = INT(TCLOD)
            IFINCR     = MOD(IFHR,ITCLOD)
            ID(19)  = IFHR
            ID(20)  = 3
            IF (IFINCR.EQ.0) THEN
               ID(18)  = IFHR-ITCLOD
            ELSE
               ID(18)  = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(139),LVLS(1,IGET(139)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C    
C     TIME AVERAGED CONVECTIVE CLOUD FRACTION.
         IF (IGET(143).GT.0) THEN
            DO 60 J=JSTA,JEND
            DO 60 I=1,IM
               IF (NCFRCV(I,J).GT.0) THEN
                  EGRID1(I,J) = ACFRCV(I,J)/NCFRCV(I,J)
               ELSE
                  EGRID1(I,J) = D00
               ENDIF
 60         CONTINUE
C
            CALL E2OUT(143,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            IFHR       = NTSD/TSPH+0.5
            ITCLOD     = INT(TCLOD)
            IFINCR     = MOD(IFHR,ITCLOD)
            ID(19)  = IFHR
            ID(20)  = 3
            IF (IFINCR.EQ.0) THEN
               ID(18)  = IFHR-ITCLOD
            ELSE
               ID(18)  = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
            CALL OUTPUT(IOUTYP,IGET(143),LVLS(1,IGET(143)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C    
C     CLOUD BASE FIELDS 
      IF ((IGET(148).GT.0) .OR. (IGET(178).GT.0)) THEN
          CLIMIT =1.0E-06
          DO J=JSTA,JEND
          DO I=1,IM
            NEED(I,J)=.TRUE.
            CLDBP(I,J) = -50000.
            CLDBZ(I,J) = -5000.
          ENDDO
          ENDDO

!$omp  parallel do
!$omp& private(cbot,lev,llmh)
          DO 70 J=JSTA,JEND
          DO 70 I=1,IM
            LLMH=LMH(I,J)
            CBOT = 500
            DO L=LLMH,1,-1
C GSM
C START AT THE FIRST LAYER ABOVE GROUND, AND FIND THE
C   FIRST LAYER WITH A VALUE OF CLOUD WATER GREATER THAN
C   THE SIGNIFICANT LIMIT (VALUE DESIGNATED BY Q. ZHAO).
C   THIS LAYER WILL BE THE CLOUD BOTTOM UNLESS THE BOTTOM
C   OF THE CONVECTIVE CLOUD (HBOT) IS FOUND BELOW IN WHICH
C   CASE HBOT BECOMES THE CLOUD BASE LAYER.
              IF (CWM(I,J,L).GT.CLIMIT.AND.NEED(I,J)) THEN
                CBOT=L
                IF (HBOT(I,J).GT.CBOT) THEN
                   CBOT = HBOT(I,J)
                ENDIF
                NEED(I,J)=.FALSE.
               ENDIF
            ENDDO
C
              IF (CBOT.EQ.500.) THEN
                 CLDBP(I,J) = -50000.
                 CLDBZ(I,J) = -5000.
              ELSE IF (CBOT.EQ.LM) THEN
                 CLDBP(I,J) = AETA1(INT(CBOT))*PDSL(I,J)+PT1
                 CLDBZ(I,J) = ZINT(I,J,LM) 
              ELSE
                 LEV = CBOT
                 CLDBP(I,J) = AETA1(INT(LEV))*PDSL(I,J)+PT1
                 CLDBZ(I,J) = HTM(I,J,LEV+1)*T(I,J,LEV+1)
     1                       *(Q(I,J,LEV+1)*D608+H1)*ROG*
     2                        (LOG(PINT(I,J,LEV+1))-LOG(CLDBP(I,J)))
     3                       +ZINT(I,J,LEV+1)
              ENDIF
 70      CONTINUE
C   CLOUD BOTTOM PRESSURE
         IF (IGET(148).GT.0) THEN
          CALL E2OUT(148,000,CLDBP,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
          ID(1:25)=0
          CALL OUTPUT(IOUTYP,IGET(148),LVLS(1,IGET(148)),
     X        GRID1,IMOUT,JMOUT)
         ENDIF

C    CLOUD BOTTOM HEIGHT    
         IF (IGET(178).GT.0) THEN
          CALL E2OUT(148,000,CLDBZ,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
          ID(1:25)=0
          CALL OUTPUT(IOUTYP,IGET(178),LVLS(1,IGET(178)),
     X        GRID1,IMOUT,JMOUT)
         ENDIF
      ENDIF
C
C     CLOUD TOP FIELDS 
      IF ((IGET(149).GT.0) .OR. (IGET(179).GT.0) .OR.
     X    (IGET(168).GT.0)) THEN
          CLIMIT =1.0E-06
          DO J=JSTA,JEND
          DO I=1,IM
            NEED(I,J)=.TRUE.
          ENDDO
          ENDDO
C GSM
C   START AT THE TOP OF THE ATMOSPHERE.  FIND THE
C   FIRST LAYER WITH A VALUE OF CLOUD WATER GREATER THAN
C   THE SIGNIFICANT LIMIT (VALUE DESIGNATED BY Q. ZHAO).
C   THIS LAYER WILL BE THE CLOUD TOP UNLESS THE TOP 
C   OF THE CONVECTIVE CLOUD (HTOP) IS FOUND ABOVE IN WHICH
C   CASE HTOP BECOMES THE CLOUD TOP LAYER.

!$omp  parallel do
!$omp& private(ctop,lev,lmhk)
          DO 80 J=JSTA,JEND
          DO 80 I=1,IM
            CTOP = 0.
            LMHK=LMH(I,J)
            DO L=1,LMHK
!              print *, i,j,l,CWM(I,J,L),CLIMIT,"-----"
              IF (CWM(I,J,L).GT.CLIMIT.AND.NEED(I,J)) THEN
                CTOP=L
                IF (HTOP(I,J).LT.CTOP) THEN
                   CTOP = HTOP(I,J)
                ENDIF
                NEED(I,J)=.FALSE.
              ENDIF
            ENDDO
C
            IF (CTOP.EQ.0.)THEN
              CLDTP(I,J) = -50000.
              CLDTZ(I,J) = -5000.
              LMHK=LMH(I,J)
              CLDTT(I,J) = T(I,J,LMHK)
            ELSE IF (CTOP.EQ.LM) THEN
              CLDTP(I,J) = AETA1(INT(LM))*PDSL(I,J)+PT1
              CLDTZ(I,J) = ZINT(I,J,LM)
              CLDTT(I,J) = T(I,J,LM)
            ELSE
              LEV = CTOP
              CLDTP(I,J) = AETA1(INT(LEV))*PDSL(I,J)+PT1
              CLDTZ(I,J) = HTM(I,J,LEV+1)*T(I,J,LEV+1)
     1                    *(Q(I,J,LEV+1)*D608+H1)*ROG*
     2                     (LOG(PINT(I,J,LEV+1))-LOG(CLDTP(I,J)))
     3                    +ZINT(I,J,LEV+1) 
              CLDTT(I,J) = T(I,J,LEV)
            ENDIF
!            print *, i,j,CLDTP(I,J), CTOP, NEED(I,J)
 80      CONTINUE
C   CLOUD TOP PRESSURE
         IF (IGET(149).GT.0) THEN
           CALL E2OUT(149,000,CLDTP,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
           ID(1:25)=0
           CALL OUTPUT(IOUTYP,IGET(149),LVLS(1,IGET(149)),
     X        GRID1,IMOUT,JMOUT)
          ENDIF
 
C   CLOUD TOP HEIGHT 
          IF (IGET(179).GT.0) THEN
           CALL E2OUT(179,000,CLDTZ,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
           ID(1:25)=0
           CALL OUTPUT(IOUTYP,IGET(179),LVLS(1,IGET(179)),
     X        GRID1,IMOUT,JMOUT)
          ENDIF

C   CLOUD TOP TEMPS
          IF (IGET(168).GT.0) THEN
           CALL E2OUT(168,000,CLDTT,EGRID2,
     X         GRID1,GRID2,IMOUT,JMOUT)
           ID(1:25)=0
           CALL OUTPUT(IOUTYP,IGET(168),LVLS(1,IGET(168)),
     X         GRID1,IMOUT,JMOUT)
          ENDIF
      ENDIF

C***  BLOCK 3.  RADIATION FIELDS.
C     
C
C     TIME AVERAGED SURFACE SHORT WAVE INCOMING RADIATION.
         IF (IGET(126).GT.0) THEN
           IF(ARDSW.GT.0.)THEN
             RRNUM=1./ARDSW
           ELSE
             RRNUM=0.
           ENDIF
           DO J=JSTA,JEND
           DO I=1,IM
              EGRID1(I,J) = ASWIN(I,J)*RRNUM
           ENDDO
           ENDDO
            CALL E2OUT(126,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            IFHR       = NTSD/TSPH+0.5
            ITRDSW     = INT(TRDSW)
            IFINCR     = MOD(IFHR,ITRDSW)
            ID(19)  = IFHR
            ID(20)  = 3
            IF (IFINCR.EQ.0) THEN
               ID(18)  = IFHR-ITRDSW
            ELSE
               ID(18)  = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL OUTPUT(IOUTYP,IGET(126),LVLS(1,IGET(126)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C     TIME AVERAGED SURFACE LONG WAVE INCOMING RADIATION.
         IF (IGET(127).GT.0) THEN
           IF(ARDLW.GT.0.)THEN
             RRNUM=1./ARDLW
           ELSE
             RRNUM=0.
           ENDIF
           DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = ALWIN(I,J)*RRNUM
           ENDDO
           ENDDO
            CALL E2OUT(127,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            IFHR       = NTSD/TSPH+0.5
            ITRDLW     = INT(TRDLW)
            IFINCR     = MOD(IFHR,ITRDLW)
            ID(19)  = IFHR
            ID(20)  = 3
            IF (IFINCR.EQ.0) THEN
               ID(18)  = IFHR-ITRDLW
            ELSE
               ID(18)  = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL OUTPUT(IOUTYP,IGET(127),LVLS(1,IGET(127)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C     TIME AVERAGED SURFACE SHORT WAVE OUTGOING RADIATION.
         IF (IGET(128).GT.0) THEN
           IF(ARDSW.GT.0.)THEN
             RRNUM=1./ARDSW
           ELSE
             RRNUM=0.
           ENDIF
           DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = -1.0*ASWOUT(I,J)*RRNUM
           ENDDO
           ENDDO
            CALL E2OUT(128,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            IFHR       = NTSD/TSPH+0.5
            ITRDSW     = INT(TRDSW)
            IFINCR     = MOD(IFHR,ITRDSW)
            ID(19)  = IFHR
            ID(20)  = 3
            IF (IFINCR.EQ.0) THEN
               ID(18)  = IFHR-ITRDSW
            ELSE
               ID(18)  = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL OUTPUT(IOUTYP,IGET(128),LVLS(1,IGET(128)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C     TIME AVERAGED SURFACE LONG WAVE OUTGOING RADIATION.
         IF (IGET(129).GT.0) THEN
           IF(ARDLW.GT.0.)THEN
             RRNUM=1./ARDLW
           ELSE
             RRNUM=0.
           ENDIF
           DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = -1.0*ALWOUT(I,J)*RRNUM
           ENDDO
           ENDDO
            CALL E2OUT(129,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            IFHR       = NTSD/TSPH+0.5
            ITRDLW     = INT(TRDLW)
            IFINCR     = MOD(IFHR,ITRDLW)
            ID(19)  = IFHR
            ID(20)  = 3
            IF (IFINCR.EQ.0) THEN
               ID(18)  = IFHR-ITRDLW
            ELSE
               ID(18)  = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL OUTPUT(IOUTYP,IGET(129),LVLS(1,IGET(129)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C     TIME AVERAGED TOP OF THE ATMOSPHERE SHORT WAVE RADIATION.
         IF (IGET(130).GT.0) THEN
           IF(ARDSW.GT.0.)THEN
             RRNUM=1./ARDSW
           ELSE
             RRNUM=0.
           ENDIF
           DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = ASWTOA(I,J)*RRNUM
           ENDDO
           ENDDO
            CALL E2OUT(130,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            IFHR       = NTSD/TSPH+0.5
            ITRDSW     = INT(TRDSW)
            IFINCR     = MOD(IFHR,ITRDSW)
            ID(19)  = IFHR
            ID(20)  = 3
            IF (IFINCR.EQ.0) THEN
               ID(18)  = IFHR-ITRDSW
            ELSE
               ID(18)  = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL OUTPUT(IOUTYP,IGET(130),LVLS(1,IGET(130)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C
C     TIME AVERAGED TOP OF THE ATMOSPHERE LONG WAVE RADIATION.
         IF (IGET(131).GT.0) THEN
           IF(ARDLW.GT.0.)THEN
             RRNUM=1./ARDLW
           ELSE
             RRNUM=0.
           ENDIF
           DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = ALWTOA(I,J)*RRNUM
           ENDDO
           ENDDO
            CALL E2OUT(131,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
            ID(1:25)=0
            IFHR       = NTSD/TSPH+0.5
            ITRDLW     = INT(TRDLW)
            IFINCR     = MOD(IFHR,ITRDLW)
            ID(19)  = IFHR
            ID(20)  = 3
            IF (IFINCR.EQ.0) THEN
               ID(18)  = IFHR-ITRDLW
            ELSE
               ID(18)  = IFHR-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            CALL OUTPUT(IOUTYP,IGET(131),LVLS(1,IGET(131)),
     X           GRID1,IMOUT,JMOUT)
         ENDIF
C     
C     CURRENT INCOMING SW RADIATION AT THE SURFACE.
      IF (IGET(156).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           IF(CZMEAN(I,J).GT.1.E-6) THEN
             FACTRS=CZEN(I,J)/CZMEAN(I,J)
           ELSE
             FACTRS=0.0
           ENDIF
           EGRID1(I,J)=HBM2(I,J)*RSWIN(I,J)*FACTRS
         ENDDO
         ENDDO
C
         CALL E2OUT(156,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25)=0
         CALL OUTPUT(IOUTYP,IGET(156),LVLS(1,IGET(156)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     CURRENT INCOMING LW RADIATION AT THE SURFACE.
      IF (IGET(157).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           IF(SIGT4(I,J).GT.0.0) THEN
             LLMH=LMH(I,J)
             TLMH=T(I,J,LLMH)
             FACTRL=5.67E-8*TLMH*TLMH*TLMH*TLMH/SIGT4(I,J)
           ELSE
             FACTRL=0.0
           ENDIF
           EGRID1(I,J)=HBM2(I,J)*RLWIN(I,J)*FACTRL
         ENDDO
         ENDDO
C
         CALL E2OUT(157,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25)=0
         CALL OUTPUT(IOUTYP,IGET(157),LVLS(1,IGET(157)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     CURRENT OUTGOING SW RADIATION AT THE SURFACE.
      IF (IGET(141).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           IF(CZMEAN(I,J).GT.1.E-6) THEN
             FACTRS=CZEN(I,J)/CZMEAN(I,J)
           ELSE
             FACTRS=0.0
           ENDIF
           EGRID1(I,J)=HBM2(I,J)*RSWOUT(I,J)*FACTRS
         ENDDO
         ENDDO
C
         CALL E2OUT(141,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25)=0
         CALL OUTPUT(IOUTYP,IGET(141),LVLS(1,IGET(141)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     CURRENT OUTGOING LW RADIATION AT THE SURFACE.
      IF (IGET(142).GT.0) THEN
         CALL E2OUT(142,000,RADOT,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25)=0
         CALL OUTPUT(IOUTYP,IGET(142),LVLS(1,IGET(142)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     END OF ROUTINE.
      print*,'End of CLDRAD'
C
      RETURN
      END
