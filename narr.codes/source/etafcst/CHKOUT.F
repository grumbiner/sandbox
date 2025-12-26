                       SUBROUTINE CHKOUT
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CHKOUT      POSTS PROFILES AND OUTPUT POST DATA
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-02-26       
C     
C ABSTRACT:  THIS ROUTINE POSTS PROFILE DATA AND WRITES
C   COMMON BLOCKS TO TEMPORARY FILE FOR USE BY THE POST
C   PROCESSOR.  OPTIONALLY, IF RUN UNDER PSHELL THIS
C   ROUTINE WILL SUBMIT POST JOBS AS THE MODEL RUNS.
C   THIS ROUTINE REPLACES ETA MODEL SUBROUTINE OUTMAP.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-02-26  RUSS TREADON
C   93-08-30  RUSS TREADON - ADDED DOCBLOC AND DIAGNOSTIC PROFILES.
C   95-03-31  T BLACK - CONVERTED FROM 1-D TO 2-D IN HORIZONTAL.   
C   95-07-31  MIKE BALDWIN - REMOVED SOUNDING DIAGNOSTICS AND BUFR.
C   96-03-13  F MESINGER - IMPROVED REDUCTION TO SEA LEVEL
C                          (TO ACHIEVE EXACT CONSISTENCY WITH THE 
C                           MODEL'S HYDROSTATIC EQUATION NEXT TO
C                           MOUNTAIN SIDES)
C   96-04-12  MIKE BALDWIN - MODIFIED SOUNDING OUTPUT
C   96-10-31  T BLACK - MODIFICATIONS FOR GENERATIONS OF NEST'S BCs
C   98-11-17  T BLACK - MODIFIED FOR DISTRIBUTED MEMORY
C   99-05-03  T BLACK - SLP REDUCTION, BCEX, AND PROFILES REMOVED;
C                       EACH PE WRITES ITS OWN MINI-RESTRT FILE
C   00-08-01  JIM TUCCILLO - QUILT SERVER CAPABILITY ADDED
C   00-10-11  T BLACK - MODIFICATIONS FOR RESTART CAPABILITY 
C                
C     
C USAGE:    CALL CHKOUT
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       
C     LIBRARY: NONE
C
C   COMMON BLOCKS: OUTFIL
C                  CTLBLK
C                  LOOPS
C                  MASKS
C                  MAPOT
C                  VRBLS
C                  PVRBLS
C                  DYNAMD
C                  PHYS2
C                  BOCO
C                  CNVCLD
C                  CLDWTR
C                  CUINIT
C                  ACMCLD
C                  ACMCLH
C                  ACMPRE
C                  ACMRDL
C                  ACMRDS
C                  ACMSFC
C                  SOIL
C                  PRFHLD
C                  TEMPV
C                  INDX
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM SP
C$$$  
C     
C     INCLUDE/DECLARE PARAMETERS.
C     
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
#include "sp.h"
C--------------------------------------------------------------------     
                            P A R A M E T E R
     & (IMJM=IM*JM-JM/2,IMT=2*IM-1,JMT=JM/2+1,LB=2*IM+JM-3)
C--------------------------------------------------------------------     
                            P A R A M E T E R
     & (LM1=LM-1,LP1=LM+1,JAM=6+2*(JM-10)
     &, NRLX1=250,NRLX2=100)
C--------------------------------------------------------------------     
                            P A R A M E T E R
     & (CAPA=0.285896)
C--------------------------------------------------------------------     
C     
C     DECLARE VARIABLES.
C
C--------------------------------------------------------------------     
                            L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,STDRD,MESO,ONHOUR,EXBC,NEST
C--------------------------------------------------------------------     
      CHARACTER*2  FHR                             
      CHARACTER*8  OUTJOB
      CHARACTER*13 ASSIGN
      CHARACTER*4  ASTMRK,TMYY
      CHARACTER*15 SUBMIT
      CHARACTER*32 LABEL
      INTEGER LABINT(4)
      EQUIVALENCE(LABEL, LABINT)
      CHARACTER*80 LINE
      CHARACTER*1  LINE1(80)
      CHARACTER*4 RESTHR
      EQUIVALENCE  (LINE,LINE1)
C--------------------------------------------------------------------     
                            R E A L
     & PSLP  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PDS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FACTR (IDIM1:IDIM2,JDIM1:JDIM2)
     &,SWTTC (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,TTND  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
C
                            I N T E G E R
     & IKNTS(0:INPES*JNPES-1),IDISP(0:INPES*JNPES-1)
C
                            R E A L
     &,ALLOCATABLE,DIMENSION(:,:,:) :: TEMPSOIL
C
C--------------------------------------------------------------------     
      CHARACTER FINFIL*50,DONE*10
C--------------------------------------------------------------------     
C     
C     INCLUDE COMMON BLOCKS.
C
C--------------------------------------------------------------------     
      INCLUDE "OUTFIL.comm"
      INCLUDE "CTLBLK.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "DYNAMD.comm"
      INCLUDE "PHYS2.comm"
      INCLUDE "BOCO.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "ACMCLD.comm"
      INCLUDE "ACMCLH.comm"
      INCLUDE "ACMPRE.comm"
      INCLUDE "ACMRDL.comm"
      INCLUDE "ACMRDS.comm"
      INCLUDE "ACMSFC.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "PRFHLD.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "INDX.comm"
      INCLUDE "CONTIN.comm"
      INCLUDE "TEMPV.comm"
      INCLUDE "BUFFER.comm"
      INCLUDE "QFLX.comm"
      INCLUDE "PPTASM.comm"
!--------------------------------------------------------------------
      COMMON /CUINIT/ CURAD
      LOGICAL CURAD
C--------------------------------------------------------------------     
C     
C     DECLARE EQUIVALENCES.
C
C--------------------------------------------------------------------     
                            E Q U I V A L E N C E
     & (TTND (1,1,1),SWTTC(1,1,1))
C--------------------------------------------------------------------     
                            I N T E G E R
     & JSTAT(MPI_STATUS_SIZE)
C--------------------------------------------------------------------     
      REAL(8) SUMT(LM),
     &        SUMT_0(LM),
     &        SUMT2(LM),
     &        SUMT2_0(LM)
      REAL(8) STDEV,RMS,TMEAN
      REAL    TMAX(LM), TMAX_0(LM), TMIN(LM), TMIN_0(LM)
      REAL(8) STRWAIT, ENDWAIT, rtc
      INTEGER IHS
      DATA IHS/MPI_REQUEST_NULL/
      INTEGER STATUS(MPI_STATUS_SIZE)
      INTEGER ISERVE
C
      DATA ISERVE / 1 /
C
C--------------------------------------------------------------------     
C***
C***  THE FOLLOWING ARE USED FOR TIMIMG PURPOSES ONLY
C***
      real*8 timef
      real nhb_tim,mpp_tim,init_tim
      common/timing/surfce_tim,nhb_tim,res_tim,exch_tim
      common/timchk/slp_tim,gath_tim,wrt_tim,prof_tim
     1,             bcex_tim,stat_tim
C***********************************************************************
C     START CHKOUT HERE.
C***********************************************************************
C***
C***  ON FIRST ENTRY INITIALIZE THE OUTPUT FILE TAG TO ZERO
C***  AND DO PRELIMINARY PROFILE DATA ASSIGNMENTS 
C***  
      IF(NTSD.EQ.1)THEN
        ITAG=0
C
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          LMHK=LMH(I,J)
          TLL1=T(I,J,LMHK)
          TLMIN(I,J)=TLL1
          TLMAX(I,J)=TLL1
        ENDDO
        ENDDO
      ENDIF
C***********************************************************************
C***
C***  UPDATE MAX AND MIN LOWEST LAYER TEMPS 
C***
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        LMHK=LMH(I,J)
        TLL1=T(I,J,LMHK)
        IF(TLL1.LT.TLMIN(I,J))TLMIN(I,J)=TLL1
        IF(TLL1.GT.TLMAX(I,J))TLMAX(I,J)=TLL1
      ENDDO
      ENDDO
C***********************************************************************
C***
C***  FIGURE OUT JUST WHERE IN THE FORECAST WE ARE.
C***
      NTSPH=INT(3600./DT+0.50)
      TIMES=(NTSD-1)*DT
      ONHOUR=.FALSE.
      IF((MOD(TIMES,3600.).EQ.0.).OR.
     1   (MOD(TIMES,3600.).GT.3600.-DT))ONHOUR=.TRUE.
C------------------------------------------------------------------
C
C     IF THE CURRENT FORECAST TIME IS A FULL HOUR OR EQUALS
C     A FULL BLOWN POST TIME, THEN WRITE THE FIELDS.
C     IF NOT, EXIT THIS ROUTINE.
C
      IF((NTSD.EQ.NSHDE).OR.ONHOUR)GO TO 100
      IF(NSTART.GT.0.AND.NSTART+1.EQ.NSHDE.AND.
     1   NTSD-1.EQ.NSHDE)GO TO 100
!
!--- Begin: Initialize convective cloud fields for radiation before
!           returning to EBU  (Ferrier 23 Jan 02)
!
      IF (CURAD) THEN
        IF (MYPE .EQ. 0) THEN
          WRITE(0,"(a)") 'CHKOUT: Initialize CUPPT,HTOP,HBOT'
          WRITE(6,"(a)") 'CHKOUT: Initialize CUPPT,HTOP,HBOT'
        ENDIF
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          CUPPT(I,J)=0.
          HTOP(I,J)=100.
          HBOT(I,J)=0.
        ENDDO
        ENDDO
        CURAD=.FALSE.
      ENDIF
!
!--- End:
!
      RETURN
C
C     IT IS TIME TO WRITE TO THE PROFILE FILE AND/OR WRITE
C     TEMPORARY FILES FOR A FULL BLOWN POST.
C     
  100 CONTINUE
C---------------------------------------------------------------------
C
C     SET FORECAST HOUR.
C
      IHR=NTSD/TSPH+0.5
C--------------------------------------------------------------------
C***  IF THIS IS NOT A FULL BLOWN OUTPUT TIME,
C***  SKIP THE RESTART FILE AND POST JOB WRITES AND GO TO SECTION
C***  WHERE ACCUMULATION ARRAYS ARE ZEROED OUT IF NECESSARY.
C--------------------------------------------------------------------
C
      IF(NTSD.NE.NSHDE.AND.NSTART+1.NE.NSHDE)GO TO 1310
C     
C--------------------------------------------------------------------
C***
C***  COMPUTE TEMPERATURE STATISTICS
C***
C--------------------------------------------------------------------
      btim0=timef()
      DO 1100 L=1,LM
C
      TMAX(L)=-1.E6
      TMIN(L)=1.E6
      SUMT(L)=0.
      SUMT2(L)=0.
C
      JJ=0
      DO J=MY_JS_GLB,MY_JE_GLB
        JJ=JJ+1
        IF(MOD(J+1,2).NE.0.and.MY_IE_GLB.EQ.IM)THEN
          IMAX=MY_IE_LOC-1
        ELSE
          IMAX=MY_IE_LOC
        ENDIF
        DO I=MYIS,IMAX
          SUMT(L)=SUMT(L)+T(I,JJ,L) 
          SUMT2(L)=SUMT2(L)+T(I,JJ,L)**2
          TMAX(L)=AMAX1(TMAX(L),T(I,JJ,L))
          TMIN(L)=AMIN1(TMIN(L),T(I,JJ,L))
        ENDDO
      ENDDO
1100  CONTINUE
C
C***  GLOBAL STATS
C
       CALL MPI_REDUCE(SUMT,SUMT_0,LM,MPI_REAL8,MPI_SUM,0,
     1        MPI_COMM_COMP,IRTN)
       CALL MPI_REDUCE(SUMT2,SUMT2_0,LM,MPI_REAL8,MPI_SUM,0,
     1        MPI_COMM_COMP,IRTN)
       CALL MPI_REDUCE(TMAX,TMAX_0,LM,MPI_REAL,MPI_MAX,0,
     1        MPI_COMM_COMP,IRTN)
       CALL MPI_REDUCE(TMIN,TMIN_0,LM,MPI_REAL,MPI_MIN,0,
     1        MPI_COMM_COMP,IRTN)
C
C
      IF(MYPE.EQ.0)THEN
        DO L=1,LM
          TMEAN=SUMT_0(L)/DBLE(IMJM)
          STDEV=DSQRT((DBLE(IMJM)*SUMT2_0(L)-SUMT_0(L)**2)/
     1                 DBLE(DBLE(IMJM)*(DBLE(IMJM-1))))
          RMS  =DSQRT(SUMT2_0(L)/DBLE(IMJM))
          WRITE(6,1094)L,TMAX_0(L),TMIN_0(L)
          WRITE(6,1095)TMEAN,STDEV,RMS
 1094     FORMAT(' LAYER=',I2,' TMAX=',E13.6,' TMIN=',E13.6)
 1095     FORMAT(9X,' TMEAN=',E13.6,' STDEV=',E13.6,
     1              ' RMS=',E13.6)
        ENDDO
      ENDIF
C
      stat_tim=stat_tim+timef()-btim0
C
C----------------------------------------------------------------------
C***  WE REACH THE CODE BELOW ONLY IF IT IS A FULL BLOWN POSTING TIME.
C***  WRITE DATA REQUIRED TO RESTART THE MODEL/INITIALIZE THE POST.
C----------------------------------------------------------------------
      CALL MPI_BARRIER(MPI_COMM_COMP,ISTAT)
C
C     PDS IS SURFACE PRESSURE.
C     TSHLTR HOLDS THE 2M THETA, CONVERT TO TEMPERATURE.
C     TERM1 IS 2m*G/(Rd*T)
C
!$omp parallel do
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        LLMH=LMH(I,J)
        PDS(I,J)=PD(I,J)+PT
        TERM1=-0.068283/T(I,J,LLMH)
        PSHLTR(I,J)=PDS(I,J)*EXP(TERM1)
        TSHLTR(I,J)=TSHLTR(I,J)*(PSHLTR(I,J)*1.E-5)**CAPA
C
        IF(CZMEAN(I,J).GT.0.)THEN
          FACTR(I,J)=CZEN(I,J)/CZMEAN(I,J)
        ELSE
          FACTR(I,J)=0.
        ENDIF
C
      ENDDO
      ENDDO
C
C   MAKE SURE POST DOES NOT BLOW UP WHEN COMPUTING RH 
C   ON THE GLOBAL N/S BOUNDARIES
C
      IF(MYPE.LT.INPES)THEN
        DO J=1,2
        DO I=MYIS,MYIE
          TSHLTR(I,J)=TSHLTR(I,3)
          QSHLTR(I,J)=QSHLTR(I,3)
        ENDDO
        ENDDO
      ENDIF
      IF(MYPE.GE.NPES-INPES)THEN
        DO J=MYJE-1,MYJE
        DO I=MYIS,MYIE
          TSHLTR(I,J)=TSHLTR(I,MYJE-2)
          QSHLTR(I,J)=QSHLTR(I,MYJE-2)
        ENDDO
        ENDDO
      ENDIF
C
C     SWTTC IS THE CURRENT SW TEMP TENDENCIES.
C
!$omp parallel do
      DO L=1,LM
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          SWTTC(I,J,L)=RSWTT(I,J,L)*FACTR(I,J)
        ENDDO
        ENDDO
      ENDDO
C
C***  TTND IS THE CURRENT RAD TEMP TENDENCIES.
C
      DO L=1,LM
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          TTND(I,J,L)=RLWTT(I,J,L)+SWTTC(I,J,L)
        ENDDO
        ENDDO
      ENDDO
C***
C***  CREATE NAME FOR RESTART FILE.
C***
c     IF(MYPE.EQ.0)THEN
C
        ITAG=NTSD/TSPH+0.5
        CALL GETENV("tmmark",RESTHR)
        IF(RESTHR.EQ.'    ')THEN
c         WRITE(RSTFIL,1150)ITAG,MYPE
c1150     FORMAT('restrt',I2.2
c    1,           '.',I3.3)
          WRITE(RSTFIL,1150)ITAG
 1150     FORMAT('restrt',I2.2
     1,           '.quilt')
        ELSE
c         WRITE(RSTFIL,1155)ITAG,MYPE,RESTHR
c1155     FORMAT('restrt',I2.2
c    1,           '.',I3.3,'.',a4)
          WRITE(RSTFIL,1155)ITAG,RESTHR
 1155     FORMAT('restrt',I2.2
     1,           '.quilt.',a4)
        ENDIF
C***
C***  OPEN UNIT TO RESTART FILE.
C***
        LRSTRT=8
c
        wrt_tim=0.
        btimw=timef()
        btim0=timef()
c
        CLOSE(LRSTRT)
c       OPEN(UNIT=LRSTRT,FILE=RSTFIL,FORM='UNFORMATTED',IOSTAT=IER) 
c      IF(IER.NE.0)WRITE(LIST,*)' LRSTRT OPEN UNIT ERROR IER=',IER
C
C       BE SURE THAT THE BUFFER IF AVAILABLE
C
        STRWAIT = rtc()
        CALL MPI_WAIT(IHS,STATUS,IERR)
        ENDWAIT = rtc() - STRWAIT
C
        IF(MYPE.EQ.0)THEN
          IF(ENDWAIT.GE.1.)THEN
            PRINT*,' Appears to be wait time in CHKOUT, time = '
     1,            ENDWAIT
          ENDIF
        ENDIF
C
C       PLACEHOLDER FOR RECORD LENGTH
        CALL COAL(DUMMY,-1)
C***
C***  WRITE DATE AND TIMESTEP INFORMATION TO RESTART FILE.
C***
        LABEL='OMEGA-ALPHA*DT/CP'
c       WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
        print*,'rstfil=',rstfil
        print*,'idat,ihrst,ntsd=',idat,ihrst,ntsd
        CALL COAL(RUN,1)
        CALL COAL(IDAT,3)
        CALL COAL(IHRST,1)
        CALL COAL(NTSD,1)
        CALL COAL(LABEL,8)
c     ENDIF
C----------------------------------------------------------------------
C***
C***  BEGIN WRITING THE RESTRT FILE
C***
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((PD(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((RES(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(PD(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(RES(1:MYIE,1:MYJE),MYIE*MYJE)
C----------------------------------------------------------------------
C
      DO L=1,LM
c       WRITE(LRSTRT)((OMGALF(I,J,L),I=1,MYIE),J=1,MYJE)
      CALL COAL(OMGALF(1:MYIE,1:MYJE,L),MYIE*MYJE)
      ENDDO
c rec46
C
      LABEL = 'BND,PD,RES,T,Q,U,V,Q2,TTND,CWM,TRAIN,TCUCN'
c     WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
c    1,              FIRST,IOUT,NSHDE
        CALL COAL(RUN,1)
        CALL COAL(IDAT,3)
        CALL COAL(IHRST,1)
        CALL COAL(NTSD,1)
        CALL COAL(LABEL,8)
        CALL COAL(FIRST,1)
        CALL COAL(IOUT,1)
        CALL COAL(NSHDE,1)
c rec47
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((PD(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((RES(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((FIS(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(PD(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(RES(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(FIS(1:MYIE,1:MYJE),MYIE*MYJE)
CCCCC
CCCCC
CCCCC   BOUNDARY CONDITION WRITE CHANGED TO BLANK RECORD
CCCCC
CCCCC
c     WRITE(LRSTRT)
      CALL COAL(PDB,LB*2)
      CALL COAL(TB,LB*LM*2)
      CALL COAL(QB,LB*LM*2)
      CALL COAL(UB,LB*LM*2)
      CALL COAL(VB,LB*LM*2)
      CALL COAL(Q2B,LB*LM*2)
      CALL COAL(CWMB,LB*LM*2)
c rec48
C----------------------------------------------------------------------
C
      DO L = 1,LM
c       WRITE(LRSTRT)((T(I,J,L),I=1,MYIE),J=1,MYJE)
      CALL COAL(T(1:MYIE,1:MYJE,L),MYIE*MYJE)
C
c       WRITE(LRSTRT)((Q(I,J,L),I=1,MYIE),J=1,MYJE)
      CALL COAL(Q(1:MYIE,1:MYJE,L),MYIE*MYJE)
C
c       WRITE(LRSTRT)((U(I,J,L),I=1,MYIE),J=1,MYJE)
      CALL COAL(U(1:MYIE,1:MYJE,L),MYIE*MYJE)
C
c       WRITE(LRSTRT)((V(I,J,L),I=1,MYIE),J=1,MYJE)
      CALL COAL(V(1:MYIE,1:MYJE,L),MYIE*MYJE)
C
c       WRITE(LRSTRT)((Q2(I,J,L),I=1,MYIE),J=1,MYJE)
      CALL COAL(Q2(1:MYIE,1:MYJE,L),MYIE*MYJE)
C
c       WRITE(LRSTRT)((TTND(I,J,L),I=1,MYIE),J=1,MYJE)
      CALL COAL(TTND(1:MYIE,1:MYJE,L),MYIE*MYJE)
C
c       WRITE(LRSTRT)((CWM(I,J,L),I=1,MYIE),J=1,MYJE)
      CALL COAL(CWM(1:MYIE,1:MYJE,L),MYIE*MYJE)
C
c       WRITE(LRSTRT)((TRAIN(I,J,L),I=1,MYIE),J=1,MYJE)
      CALL COAL(TRAIN(1:MYIE,1:MYJE,L),MYIE*MYJE)
C
c       WRITE(LRSTRT)((TCUCN(I,J,L),I=1,MYIE),J=1,MYJE)
      CALL COAL(TCUCN(1:MYIE,1:MYJE,L),MYIE*MYJE)
      ENDDO
c rec453
C----------------------------------------------------------------------
C
      LABEL = 'MISC VARIABLES'
c     WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
c    1,            ((RSWIN(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((RSWOUT(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((TG(I,J),I=1,MYIE),J=1,MYJE)
c    4,            ((Z0(I,J),I=1,MYIE),J=1,MYJE)
c    5,            ((AKMS(I,J),I=1,MYIE),J=1,MYJE)
c    6,            ((CZEN(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(RUN,1)
      CALL COAL(IDAT,3)
      CALL COAL(IHRST,1)
      CALL COAL(NTSD,1)
      CALL COAL(LABEL,8)
      CALL COAL(RSWIN(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(RSWOUT(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(TG(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(Z0(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(AKMS(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(CZEN(1:MYIE,1:MYJE),MYIE*MYJE)

c rec454
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((AKHS(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((THS(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((QS(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((TWBS(I,J),I=1,MYIE),J=1,MYJE)
c    4,            ((QWBS(I,J),I=1,MYIE),J=1,MYJE)
c    5,            ((HBOT(I,J),I=1,MYIE),J=1,MYJE)
c    6,            ((CFRACL(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(AKHS(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(THS(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(QS(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(TWBS(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(QWBS(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(HBOT(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(CFRACL(1:MYIE,1:MYJE),MYIE*MYJE)
c rec455
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((THZ0(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((QZ0(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((UZ0(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((VZ0(I,J),I=1,MYIE),J=1,MYJE)
c    4,            ((USTAR(I,J),I=1,MYIE),J=1,MYJE)
c    5,            ((HTOP(I,J),I=1,MYIE),J=1,MYJE)
c    6,            ((CFRACM(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(THZ0(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(QZ0(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(UZ0(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(VZ0(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(USTAR(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(HTOP(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(CFRACM(1:MYIE,1:MYJE),MYIE*MYJE)
c rec456
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((SNO(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((SI(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((CLDEFI(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((RF(I,J),I=1,MYIE),J=1,MYJE)
c    4,            ((PSLP(I,J),I=1,MYIE),J=1,MYJE)
c    5,            ((CUPPT(I,J),I=1,MYIE),J=1,MYJE)
c    6,            ((CFRACH(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(SNO(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SI(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(CLDEFI(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(RF(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(PSLP(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(CUPPT(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(CFRACH(1:MYIE,1:MYJE),MYIE*MYJE)
c rec457
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((SOILTB(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((SFCEXC(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((SMSTAV(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((SMSTOT(I,J),I=1,MYIE),J=1,MYJE)
c    4,            ((GRNFLX(I,J),I=1,MYIE),J=1,MYJE)
c    5,            ((PCTSNO(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(SOILTB(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SFCEXC(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SMSTAV(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SMSTOT(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(GRNFLX(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(PCTSNO(1:MYIE,1:MYJE),MYIE*MYJE)
c rec458
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((RLWIN(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((RADOT(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((CZMEAN(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((SIGT4(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(RLWIN(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(RADOT(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(CZMEAN(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SIGT4(1:MYIE,1:MYJE),MYIE*MYJE)
c rec459
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((U00(I,J),I=1,MYIE),J=1,MYJE)
c    1,              UL
c    2,            ((LC(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((SR(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(U00(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(UL,2*LM)
      CALL COAL(LC(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SR(1:MYIE,1:MYJE),MYIE*MYJE)
c rec460
C----------------------------------------------------------------------
C
      LABEL = 'ACCUMULATED VARIABLES'
c     WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
c    1,          ((PREC(I,J),I=1,MYIE),J=1,MYJE)
c    2,          ((ACPREC(I,J),I=1,MYIE),J=1,MYJE)
c    3,          ((ACCLIQ(I,J),I=1,MYIE),J=1,MYJE)
c    4,          ((CUPREC(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(RUN,1)
      CALL COAL(IDAT,3)
      CALL COAL(IHRST,1)
      CALL COAL(NTSD,1)
      CALL COAL(LABEL,8)
      CALL COAL(PREC(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(ACPREC(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(ACCLIQ(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(CUPREC(1:MYIE,1:MYJE),MYIE*MYJE)
c rec461
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((ACFRCV(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((NCFRCV(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((ACFRST(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((NCFRST(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(ACFRCV(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(NCFRCV(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(ACFRST(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(NCFRST(1:MYIE,1:MYJE),MYIE*MYJE)
c rec462
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((ACSNOW(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((ACSNOM(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((SSROFF(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((BGROFF(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(ACSNOW(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(ACSNOM(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SSROFF(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(BGROFF(1:MYIE,1:MYJE),MYIE*MYJE)
c rec463
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((SFCSHX(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((SFCLHX(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((SUBSHX(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((SNOPCX(I,J),I=1,MYIE),J=1,MYJE)
c    4,            ((SFCUVX(I,J),I=1,MYIE),J=1,MYJE)
c    5,            ((SFCEVP(I,J),I=1,MYIE),J=1,MYJE)
c    6,            ((POTEVP(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(SFCSHX(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SFCLHX(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SUBSHX(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SNOPCX(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SFCUVX(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(SFCEVP(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(POTEVP(1:MYIE,1:MYJE),MYIE*MYJE)
c rec464
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((ASWIN(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((ASWOUT(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((ASWTOA(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((ALWIN(I,J),I=1,MYIE),J=1,MYJE)
c    4,            ((ALWOUT(I,J),I=1,MYIE),J=1,MYJE)
c    5,            ((ALWTOA(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(ASWIN(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(ASWOUT(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(ASWTOA(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(ALWIN(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(ALWOUT(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(ALWTOA(1:MYIE,1:MYJE),MYIE*MYJE)
C
c     WRITE(LRSTRT)ARDSW,ARDLW,ASRFC,AVRAIN,AVCNVC
      CALL COAL(ARDSW,1)
      CALL COAL(ARDLW,1)
      CALL COAL(ASRFC,1)
      CALL COAL(AVRAIN,1)
      CALL COAL(AVCNVC,1)
c rec465
C
c     WRITE(LRSTRT)((TH10(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((Q10(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((U10(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((V10(I,J),I=1,MYIE),J=1,MYJE)
c    4,            ((TSHLTR(I,J),I=1,MYIE),J=1,MYJE)
c    5,            ((QSHLTR(I,J),I=1,MYIE),J=1,MYJE)
c    6,            ((PSHLTR(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((TH30(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((Q30(I,J),I=1,MYIE),J=1,MYJE)
c    3,            ((U30(I,J),I=1,MYIE),J=1,MYJE)
c    4,            ((V30(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(TH10(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(Q10(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(U10(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(V10(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(TSHLTR(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(QSHLTR(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(PSHLTR(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(TH30(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(Q30(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(U30(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(V30(1:MYIE,1:MYJE),MYIE*MYJE)
c rec466
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)(((SMC(I,J,N),I=1,MYIE),J=1,MYJE),N=1,NSOIL)
      CALL COAL(SMC(1:MYIE,1:MYJE,1:NSOIL),MYIE*MYJE*NSOIL)
c rec467
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((CMC(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(CMC(1:MYIE,1:MYJE),MYIE*MYJE)
c rec468
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)(((STC(I,J,N),I=1,MYIE),J=1,MYJE),N=1,NSOIL)
      CALL COAL(STC(1:MYIE,1:MYJE,1:NSOIL),MYIE*MYJE*NSOIL)
c rec469
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)(((SH2O(I,J,N),I=1,MYIE),J=1,MYJE),N=1,NSOIL)
      CALL COAL(SH2O(1:MYIE,1:MYJE,1:NSOIL),MYIE*MYJE*NSOIL)
c rec???
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((ALBEDO(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(ALBEDO(1:MYIE,1:MYJE),MYIE*MYJE)
c rec???
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((FQU(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    2,           ((FQV(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    3,           ((DQFLX(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    4,           ((FCU(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    5,           ((FCV(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    6,           ((DCFLX(I,J),I=MYIS,MYIE),J=MYJS,MYJE)

c    7,           ((FQU7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    8,           ((FQV7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    9,           ((DQFLX7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    0,           ((FCU7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    1,           ((FCV7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    2,           ((DCFLX7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)

c    3,           ((DQADV(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    4,           ((FQNEV1(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
c    5,           ((FQSEV1(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
      CALL COAL(FQU(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(FQV(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(DQFLX(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(FCU(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(FCV(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(DCFLX(1:MYIE,1:MYJE),MYIE*MYJE)

      CALL COAL(FQU7(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(FQV7(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(DQFLX7(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(FCU7(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(FCV7(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(DCFLX7(1:MYIE,1:MYJE),MYIE*MYJE)

      CALL COAL(DQADV(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(FQNEV1(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(FQSEV1(1:MYIE,1:MYJE),MYIE*MYJE)
c rec???
C----------------------------------------------------------------------
C
c     WRITE(LRSTRT)((VAPINC(I,J),I=1,MYIE),J=1,MYJE)
c    2,           ((CLDINC(I,J),I=1,MYIE),J=1,MYJE)
c    3,           ((VAPINC7(I,J),I=1,MYIE),J=1,MYJE)
c    4,           ((CLDINC7(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(VAPINC(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(CLDINC(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(VAPINC7(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(CLDINC7(1:MYIE,1:MYJE),MYIE*MYJE)
c rec???
C----------------------------------------------------------------------
c     WRITE(LRSTRT)((POTFLX(I,J),I=1,MYIE),J=1,MYJE)
c    1,            ((TLMIN(I,J),I=1,MYIE),J=1,MYJE)
c    2,            ((TLMAX(I,J),I=1,MYIE),J=1,MYJE)
c    3,              ACUTIM,ARATIM,APHTIM
c    4,              NHEAT,NPHS,NCNVC,NPREC,NRDSW,NRDLW,NSRFC
c    5,              TPH0D,TLM0D,RESTRT
      CALL COAL(POTFLX(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(TLMIN(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(TLMAX(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(ACUTIM,1)
      CALL COAL(ARATIM,1)
      CALL COAL(APHTIM,1)
      CALL COAL(NHEAT,1)
      CALL COAL(NPHS,1)
      CALL COAL(NCNVC,1)
      CALL COAL(NPREC,1)
      CALL COAL(NRDSW,1)
      CALL COAL(NRDLW,1)
      CALL COAL(NSRFC,1)
      CALL COAL(TPH0D,1)
      CALL COAL(TLM0D,1)
      CALL COAL(RESTRT,1)
c rec470
C----------------------------------------------------------------------
      DO L=1,LM
c       WRITE(LRSTRT)((RSWTT(I,J,L),I=1,MYIE),J=1,MYJE)
c       WRITE(LRSTRT)((RLWTT(I,J,L),I=1,MYIE),J=1,MYJE)
        CALL COAL(RSWTT(1:MYIE,1:MYJE,L),MYIE*MYJE)
        CALL COAL(RLWTT(1:MYIE,1:MYJE,L),MYIE*MYJE)
      ENDDO
C
      DO L=1,LM
c       WRITE(LRSTRT)((T0(I,J,L),I=1,MYIE),J=1,MYJE)
c       WRITE(LRSTRT)((Q0(I,J,L),I=1,MYIE),J=1,MYJE)
        CALL COAL(T0(1:MYIE,1:MYJE,L),MYIE*MYJE)
        CALL COAL(Q0(1:MYIE,1:MYJE,L),MYIE*MYJE)
      ENDDO
C
c     WRITE(LRSTRT)((P0(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(P0(1:MYIE,1:MYJE),MYIE*MYJE)
C
c     WRITE(LRSTRT)((CNVBOT(I,J),I=1,MYIE),J=1,MYJE)
c     WRITE(LRSTRT)((CNVTOP(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(CNVBOT(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(CNVTOP(1:MYIE,1:MYJE),MYIE*MYJE)
C
c     WRITE(LRSTRT)((RSWTOA(I,J),I=1,MYIE),J=1,MYJE)
c     WRITE(LRSTRT)((RLWTOA(I,J),I=1,MYIE),J=1,MYJE)
      CALL COAL(RSWTOA(1:MYIE,1:MYJE),MYIE*MYJE)
      CALL COAL(RLWTOA(1:MYIE,1:MYJE),MYIE*MYJE)
c rec560
C----------------------------------------------------------------------
C     AT THIS POINT WE HAVE ACCUMULATED ALL OF THE DATA INTO BUF.
C     WE WANT TO KNOW THE MAXIMUM AMOUNT ACROSS ALL MPI TASKS
C     THIS IS USEFUL IN CASE WE DECIDE TO WRITE A FILE
C     INSTEAD OF SENDING THE DATA TO THE I/O SERVERS
C
      CALL MPI_ALLREDUCE(IP,IPMAX,1,MPI_INTEGER,MPI_MAX,
     *   MPI_COMM_COMP,IERR)
C    
C     IPMAX IS THE MAXIMUM NUMBER OF 4 BYTE REALS ACROSS
C     THE MPI TASKS
C     LETS COMPUTE A RECLEN THAT IS A MULTIPLE OF 2**18 BYTES
C     WE WILL USE THIS WHEN OPENING THE DIRECT ACCESS FILE
C
      IBLOCK = ((IPMAX*4)/(2**18) ) + 1
      IRECLEN = IBLOCK * ( 2**18 )
C
C     WE WILL PLACE THE RECLEN IN THE BEGINNING OF THE FILE
C     THIS IS HANDY
C
      CALL REPLACE(IRECLEN,1,1) 
C
C     IF WE HAVE ANY I/O SERVERS WE WILL SEND THE DATA TO THEM
C     FOR PROCESSING
C
      IF ( IQUILT_GROUP .GT. 0 ) THEN
C
      IF ( MYPE .EQ. 0 ) THEN
         CALL MPI_SEND
     *   (ITAG,1,MPI_INTEGER,0,0,MPI_COMM_INTER_ARRAY(ISERVE),IERR)
      ENDIF
C
      DO I = 0, INUMQ(ISERVE) -1
         CALL PARA_RANGE(0, jnpes-1, INUMQ(ISERVE), I, ISTART, IEND)
cwas     call para_range(0, NPES-1,inumq(iserve), i, istart, iend)
         MYPE_ROW = MYPE / INPES
C
         IF(MYPE_ROW .GE. ISTART .AND. MYPE_ROW .LE. IEND )THEN
             CALL MPI_ISEND
     *  (BUF,IP,mpi_real,I,ITAG,MPI_COMM_INTER_ARRAY(ISERVE),IHS,IERR)
        ENDIF
C
      ENDDO
C
C     IN CASE WE HAVE MULTIPLE GROUPS OF I/O SERVERS, INCREMENT TO THE
C     NEXT SERVER FOR THE NEXT OUTPUT TIME
C
      ISERVE = ISERVE + 1
      IF ( ISERVE .GT. IQUILT_GROUP ) ISERVE = 1
C
C     APPARENTLY, WE HAVE CHOSEN NOT TO SUPPLY ANY I/O SERVERS
C     WE WILL WRITE A DIRECT ACCESS FILE INSTEAD
C
      ELSE
C
        OPEN(UNIT=LRSTRT,FILE=RSTFIL,FORM='UNFORMATTED',IOSTAT=IER,
     *    ACCESS='DIRECT',RECL=IRECLEN)
        IF(IER.NE.0)WRITE(LIST,*)' LRSTRT OPEN UNIT ERROR IER=',IER
C
        WRITE(LRSTRT,REC=MYPE+1) (BUF(I),I=1,IP)
        CLOSE(LRSTRT)
C
      ENDIF
c
      dif_tim=timef()-btim0
      wrt_tim=wrt_tim+dif_tim
      call mpi_reduce(wrt_tim,wrt_tim_0,1,MPI_REAL,MPI_MAX,0,
     1                MPI_COMM_COMP,ierr)
      if(mype.eq.0)then
        write(6,*)' SHIPPED OR WROTE DATA, TIME = ',
     *    wrt_tim_0*1.e-03
      endif
      CALL MPI_BARRIER(MPI_COMM_COMP,ISTAT)
C***
C***  SEND SIGNAL THAT ALL TASKS HAVE FINISHED WRITING
C***
      IF(IQUILT_GROUP.EQ.0)THEN
      IF(MYPE.EQ.0)THEN
        DONE='DONE'
        WRITE(FINFIL,1190)ITAG,RESTHR
 1190   FORMAT('fcstdone',I2.2,'.',A4)
        LFINFIL=91
        CLOSE(LFINFIL)
        OPEN(UNIT=LFINFIL,FILE=FINFIL,FORM='UNFORMATTED',IOSTAT=IER)
        WRITE(LFINFIL)DONE
        CLOSE(LFINFIL)
        IF(IER.NE.0)WRITE(LIST,*)' SIGNAL SENT TO FINFIL:  DONE'
      ENDIF
      ENDIF
C----------------------------------------------------------------------
C
C***  RESET ACCUMULATION COUNTERS TO ZERO.
C
      APHTIM=0.
      ACUTIM=0.
      ARATIM=0.
C----------------------------------------------------------------------
C***
C***  EDIT POST JOB SCRIPT TO ACCESS DATA JUST WRITTEN.
C***
C----------------------------------------------------------------------
      IF(MYPE.EQ.0)THEN
        CALL GETENV("tmmark",ASTMRK)
        IF(ASTMRK.EQ.'    ')THEN
          WRITE(ASSIGN,1200)ITAG
 1200     FORMAT('.assign',I2.2)
        ELSE
	  WRITE(ASSIGN,1230)ITAG,ASTMRK
 1230     FORMAT('.assign',I2.2,A4)
        ENDIF
C
        WRITE(OUTJOB,1240) ITAG
 1240   FORMAT('outjob',I2.2)
        WRITE(LIST,*)'CHKOUT:  GENERATE POST SCRIPT FILE ',OUTJOB
C
        LUNIN = 21
        LUNOT = 5
        REWIND(LUNIN)
        OPEN(LUNOT,FILE=OUTJOB)
 1250   READ(LUNIN,1260,END=1290) LINE
 1260   FORMAT(A80)
C
C***  TMMARK STATEMENT.
C
        IF(INDEX(LINE,'tmmark=').NE.0)THEN
	IF(LINE1(11).EQ.'0')THEN
	    CALL GETENV("TM00",TMYY)
          ELSEIF(LINE1(11).EQ.'3')THEN
	    CALL GETENV("TM03",TMYY)
          ELSEIF(LINE1(11).EQ.'6')THEN
	    CALL GETENV("TM06",TMYY)
          ELSEIF(LINE1(11).EQ.'9')THEN
	    CALL GETENV("TM09",TMYY)
          ELSEIF(LINE1(11).EQ.'2')THEN
	    CALL GETENV("TM12",TMYY)
          ENDIF
          WRITE(LUNOT,1260) LINE
          GO TO 1250
        ENDIF
C     
C***  TMYY STATEMENT
C     
        IF(INDEX(LINE,'TMYY=').NE.0)THEN
          LINE = 'TMYY='//TMYY
          WRITE(LUNOT,1260) LINE
          GO TO 1250
        ENDIF
C     
C***  ASSIGN STATEMENT.
C     
        IF(INDEX(LINE,'ASSIGN=').NE.0)THEN
           LINE = 'ASSIGN='//ASSIGN
           WRITE(LUNOT,1260) LINE
           GO TO 1250
        ENDIF
C     
C***  MODEL RESTART FILE.
C     
        IF(INDEX(LINE,'RESTRT=').NE.0)THEN
          LINE = 'RESTRT='//RSTFIL
          WRITE(LUNOT,1260) LINE
          GO TO 1250
        ENDIF
C     
C***  LINES CONTAINING qq NEED TO BE MODIFIED FOR DISPOSAL
C***  FROM AND TO PROPER PLACES
C
        IF(INDEX(LINE,'qq').NE.0)THEN
          IHR1 = IHR / 10
          IHR2 = IHR - 10 * IHR1
          IPUTIT = INDEX(LINE,'qq')
          WRITE(LINE1(IPUTIT),1270)IHR1
          WRITE(LINE1(IPUTIT+1),1270)IHR2
 1270     FORMAT(I1)
          ISTRT = IPUTIT + 2
C
          DO I = ISTRT,80
            IF(LINE1(I).EQ.'q')THEN
              WRITE(LINE1(I),1270)IHR1
              WRITE(LINE1(I+1),1270)IHR2
              GO TO 1280
            ENDIF
          ENDDO
 1280     CONTINUE
C
          WRITE(LUNOT,1260)LINE
          GO TO 1250
        ENDIF
C     
C***  TURN ON LINE TO QUEUE JOB AFTER ALL OUTPUT JOBS.
C     
        IF(NTSD.GE.NTSTM)THEN
          IF(INDEX(LINE,'$LAST').NE.0)THEN
            LXX  = INDEX(LINE,'#')
            LINE = LINE(LXX+1:80)
            WRITE(LUNOT,1260) LINE
            GO TO 1250
          ENDIF
        ENDIF
C     
C***  IF NOTHING TRIGGERED ABOVE, WRITE LINE TO OUTPUT FILE.
C     
        WRITE(LUNOT,1260) LINE
        GO TO 1250
C     
 1290   CONTINUE
        CLOSE(LUNOT)
C
      ENDIF
C     
C***  IF SUBPOST (NAMELIST INPUT VARIABLE) IS TRUE THEN 
C***  THE FOLLOWING CALL TO ISHELL WILL SUBMIT A POST JOB TO 
C***  PROCESS THE DATA JUST WRITTEN TO THE TEMPORARY FILES.
C
C***  IF SUBPOST IS FALSE ISHELL WILL NOT BE CALLED AND
C***  THE POST JOB SHOULD BE SUBMITTED LATER IN THE SCRIPT
C
ccccc IF(SUBPOST)THEN
ccccc   SUBMIT = 'qsub ' // OUTJOB
ccccc   WRITE(LIST,*)'CHKOUT:  SUBMIT POST JOB ',SUBMIT
ccccc   CALL ISHELL(SUBMIT)
ccccc ENDIF
C--------------------------------------------------------------------
C***
C***  POST-POSTING UPDATING AND INITIALIZING.
C***
C--------------------------------------------------------------------
C***  IF (NTSD.EQ.NSHDE), THEN THIS WAS ALSO A FORECAST
C***  OUTPUT TIME.  WE NEED TO INCREMENT NSHDE FOR THE
C***  NEXT FORECAST OUTPUT TIME.
C
      IF(NTSD.EQ.NSHDE.OR.NSTART+1.EQ.NSHDE)THEN
        IOUT = IOUT+1
        IF (.NOT.RESTRT)   GO TO 1300
        IF (NTSD.EQ.NSHDE.OR.NSTART+1.EQ.NSHDE) GO TO 1300
        IOUT  = IOUT-1
 1300   NSHDE = ISHDE(IOUT)
      ENDIF
C
C***  ZERO ACCUMULATOR ARRAYS.
C***  AVERAGE CLOUD AMOUNT ARRAY
C
 1310 CONTINUE
      IF(MOD(NTSD,NCLOD).LT.NPHS)THEN
        IF(MYPE.EQ.0)WRITE(LIST,*)'CHKOUT: ZERO AVG CLD AMT ARRAY'
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          ACFRCV(I,J) = 0.
          NCFRCV(I,J) = 0
          ACFRST(I,J) = 0.
          NCFRST(I,J) = 0
        ENDDO
        ENDDO
      ENDIF
C
C***  TOTAL AND CONVECTIVE PRECIPITATION ARRAYS.
C***  TOTAL SNOW AND SNOW MELT ARRAYS.
C***  STORM SURFACE AND BASE GROUND RUN OFF ARRAYS.
C***  PRECIPITATION TYPE ARRAY
C
      IF(MOD(NTSD,NPREC).LT.NCNVC)THEN
        IF(MYPE.EQ.0)WRITE(LIST,*)
     1                 'CHKOUT: ZERO ACCUM PRECIP ARRAYS'
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          ACPREC(I,J) = 0.
          CUPREC(I,J) = 0.
          ACSNOW(I,J) = 0.
          ACSNOM(I,J) = 0.
          SSROFF(I,J) = 0.
          BGROFF(I,J) = 0.
          SFCEVP(I,J) = 0.
          POTEVP(I,J) = 0.
        ENDDO
        ENDDO
CduleS
        DO J=JDIM1,JDIM2
        DO I=IDIM1,IDIM2
          VAPINC(I,J) = 0.
          CLDINC(I,J) = 0.
          VAPINC7(I,J) = 0.
          CLDINC7(I,J) = 0.
C
          FQNEV(I,J)=0.
          FQSEV(I,J)=0.
          FCNEV(I,J)=0.
          FCSEV(I,J)=0.
          FCNEV7(I,J)=0.
          FCSEV7(I,J)=0.
          FQNEV7(I,J)=0.
          FQSEV7(I,J)=0.
          FQU(I,J)=0.
          FQV(I,J)=0.
          FCU(I,J)=0.
          FCV(I,J)=0.
          FQU7(I,J)=0.
          FQV7(I,J)=0.
          FCU7(I,J)=0.
          FCV7(I,J)=0.
          DQADV(I,J)=0.
          DQFLX(I,J)=0.
          DCFLX(I,J)=0.
          DQFLX7(I,J)=0.
          DCFLX7(I,J)=0.
        ENDDO
        ENDDO
CduleE
      ENDIF
C
C***  GRID-SCALE AND CONVECTIVE (LATENT) HEATING ARRAYS.
C
      IF(MOD(NTSD,NHEAT).LT.NCNVC)THEN
        IF(MYPE.EQ.0)WRITE(LIST,*)
     1                 'CHKOUT: ZERO ACCUM LATENT HEATING ARRAYS'
        AVRAIN = 0.
        AVCNVC = 0.
        DO L=1,LM
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            TRAIN(I,J,L) = 0.
            TCUCN(I,J,L) = 0.
          ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!--- Begin: Initialize convective cloud fields for radiation before
!           returning to EBU  (Ferrier 23 Jan 02)
!
      IF (CURAD) THEN
        IF (MYPE .EQ. 0) THEN
          WRITE(0,"(a)") 'CHKOUT: Initialize CUPPT,HTOP,HBOT'
          WRITE(6,"(a)") 'CHKOUT: Initialize CUPPT,HTOP,HBOT'
        ENDIF
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          CUPPT(I,J)=0.
          HTOP(I,J)=100.
          HBOT(I,J)=0.
        ENDDO
        ENDDO
        CURAD=.FALSE.
      ENDIF
C
C***  RESET CONVECTIVE CLOUD TOP AND BOTTOM ARRAYS
C     (diagnostic only; these fields are not cycled & not read when TSTART=0)
C
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        CNVTOP(I,J)=100.
        CNVBOT(I,J)=0.
      ENDDO
      ENDDO
!
!--- End:
!
C
C***  LONG WAVE RADIATION ARRAYS.
C     
      IF(MOD(NTSD,NRDLW).LT.NPHS)THEN
        IF(MYPE.EQ.0)WRITE(LIST,*)
     1                 'CHKOUT: ZERO ACCUM LW RADTN ARRAYS'
        ARDLW = 0.
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          ALWIN(I,J) = 0.
          ALWOUT(I,J) = 0.
          ALWTOA(I,J) = 0.
        ENDDO
        ENDDO
      ENDIF
C     
C***  SHORT WAVE RADIATION ARRAYS.
C     
      IF(MOD(NTSD,NRDSW).LT.NPHS)THEN
        IF(MYPE.EQ.0)WRITE(LIST,*)
     1                 'CHKOUT:  ZERO ACCUM SW RADTN ARRAYS'
        ARDSW = 0.
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          ASWIN(I,J) = 0.
          ASWOUT(I,J) = 0.
          ASWTOA(I,J) = 0.
        ENDDO
        ENDDO
      ENDIF
C     
C***  SURFACE SENSIBLE AND LATENT HEAT FLUX ARRAYS.
C     
      IF(MOD(NTSD,NSRFC).LT.NPHS)THEN
        IF(MYPE.EQ.0)WRITE(LIST,*)
     1                 'CHKOUT:  ZERO ACCUM SFC FLUX ARRAYS'
        ASRFC = 0.
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          SFCSHX(I,J) = 0.
          SFCLHX(I,J) = 0.
          SUBSHX(I,J) = 0.
          SNOPCX(I,J) = 0.
          SFCUVX(I,J) = 0.
          POTFLX(I,J) = 0.
        ENDDO
        ENDDO
      ENDIF
C
C***  RESET THE MAX/MIN TEMPERATURE ARRAYS
C
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        TLMIN(I,J)=999.
        TLMAX(I,J)=-999.
      ENDDO
      ENDDO
C
C     END OF ROUTINE.
C
      RETURN
      END
      SUBROUTINE COAL(A,LEN)
      INCLUDE "BUFFER.comm"
      INCLUDE 'mpif.h'
      REAL A(*)
      IF ( LEN .LT. 0 ) THEN
         IP = 0
      END IF
      IF ( IP + LEN .GT. IBUFMAX ) THEN
         PRINT *, ' IBUFMAX in BUFFER.comm is too small, stopping'
         PRINT *, ' CHANGE IBUFMAX in parmbuf and recompile'       
         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
      ENDIF
      DO I = 1, ABS(LEN)
         IP = IP + 1
         BUF(IP) = A(I)
      ENDDO
      END
      SUBROUTINE REPLACE(A,LEN,IW)
      INCLUDE "BUFFER.comm"
      REAL A(*)
      IPP = IW
      DO I = 1, LEN
         BUF(IPP) = A(I)
         IPP = IPP + 1
      END DO
      END

      
