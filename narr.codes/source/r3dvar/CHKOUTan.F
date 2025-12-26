                       SUBROUTINE CHKOUTan
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
!   99-09-08  d parrish - extract part of CHKOUT that writes mini-restrt files
!                          for use in 3dvar analysis
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
C                  ACMCLD
C                  ACMCLH
C                  ACMPRE
C                  ACMRDL
C                  ACMRDS
C                  ACMSFC
C                  SOIL
C                  PRFHLD
C                  INDX
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM SP
C$$$  
C     
C     INCLUDE/DECLARE PARAMETERS.
C     
      INCLUDE "PARMETA.comm"
      INCLUDE "PARMTBL.comm"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
      include "my_comm.h"
!#include "sp.h"
C--------------------------------------------------------------------     
!                           P A R A M E T E R
!    & (IMJM=IM*JM-JM/2,IMT=2*IM-1,JMT=JM/2+1,LB=2*IM+JM-3)
C--------------------------------------------------------------------     
!                           P A R A M E T E R
!    & (LM1=LM-1,LP1=LM+1,JAM=6+2*(JM-10)
!    &, NRLX1=250,NRLX2=100)
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
      CHARACTER*80 LINE
      CHARACTER*1  LINE1(80)
      CHARACTER*4 RESTHR
      character(2) restihr
      EQUIVALENCE  (LINE,LINE1)
C--------------------------------------------------------------------     
                            R E A L
     & PSLP  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PDS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FACTR (IDIM1:IDIM2,JDIM1:JDIM2)
!    &,SWTTC (IDIM1:IDIM2,JDIM1:JDIM2,LM)
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
      INCLUDE "QFLX.comm"
      INCLUDE "PPTASM.comm"
C--------------------------------------------------------------------     
C     
C     DECLARE EQUIVALENCES.
C
C--------------------------------------------------------------------     
!                           E Q U I V A L E N C E
!    & (TTND (1,1,1),SWTTC(1,1,1))
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

!!!!!!!!!! eliminated everything before writing of restart file

C***
C***  CREATE NAME FOR RESTART FILE.
C***
      ITAG=NTSD/TSPH+0.5
      if(itag.eq.0) then

!-------------- this is write of analysis restart file

       call getenv("restrtahr",restihr)
       call getenv("tmmarka",resthr)
       write(rstfil,1150)restihr,mype,resthr
 1150     FORMAT('restrt',a2,
     1            '.',I3.3,'.',a4)
      ELSE

!--------------- guess file was only available in monolithic form, and
!--------------- so is being written out as mini-files for re-reading
!--------------- at end of analysis

       call getenv("tmmarkb",resthr)
          WRITE(RSTFIL,1155)ITAG,MYPE,RESTHR
 1155     FORMAT('restrt',I2.2
     1,           '.',I3.3,'.',a4)
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
        OPEN(UNIT=LRSTRT,FILE=RSTFIL,FORM='UNFORMATTED',IOSTAT=IER) 
        IF(IER.NE.0)WRITE(LIST,*)' LRSTRT OPEN UNIT ERROR IER=',IER
C***
C***  WRITE DATE AND TIMESTEP INFORMATION TO RESTART FILE.
C***
        LABEL='OMEGA-ALPHA*DT/CP'
        WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
c     ENDIF
C----------------------------------------------------------------------
C***
C***  BEGIN WRITING THE RESTRT FILE
C***
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((PD(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((RES(I,J),I=1,MYIE),J=1,MYJE)
C----------------------------------------------------------------------
C
      DO L=1,LM
        WRITE(LRSTRT)((OMGALF(I,J,L),I=1,MYIE),J=1,MYJE)
      ENDDO
c rec46
C
      LABEL = 'BND,PD,RES,T,Q,U,V,Q2,TTND,CWM,TRAIN,TCUCN'
      WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
     1,              FIRST,IOUT,NSHDE
c rec47
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((PD(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((RES(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((FIS(I,J),I=1,MYIE),J=1,MYJE)
CCCCC
CCCCC
CCCCC   BOUNDARY CONDITION WRITE CHANGED TO BLANK RECORD
CCCCC
CCCCC
      WRITE(LRSTRT)
c rec48
C----------------------------------------------------------------------
C
      DO L = 1,LM
        WRITE(LRSTRT)((T(I,J,L),I=1,MYIE),J=1,MYJE)
C
        WRITE(LRSTRT)((Q(I,J,L),I=1,MYIE),J=1,MYJE)
C
        WRITE(LRSTRT)((U(I,J,L),I=1,MYIE),J=1,MYJE)
C
        WRITE(LRSTRT)((V(I,J,L),I=1,MYIE),J=1,MYJE)
C
        WRITE(LRSTRT)((Q2(I,J,L),I=1,MYIE),J=1,MYJE)
C
        WRITE(LRSTRT)((TTND(I,J,L),I=1,MYIE),J=1,MYJE)
C
        WRITE(LRSTRT)((CWM(I,J,L),I=1,MYIE),J=1,MYJE)
C
        WRITE(LRSTRT)((TRAIN(I,J,L),I=1,MYIE),J=1,MYJE)
C
        WRITE(LRSTRT)((TCUCN(I,J,L),I=1,MYIE),J=1,MYJE)
      ENDDO
c rec453
C----------------------------------------------------------------------
C
      LABEL = 'MISC VARIABLES'
      WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
     1,            ((RSWIN(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((RSWOUT(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((TG(I,J),I=1,MYIE),J=1,MYJE)
     4,            ((Z0(I,J),I=1,MYIE),J=1,MYJE)
     5,            ((AKMS(I,J),I=1,MYIE),J=1,MYJE)
     6,            ((CZEN(I,J),I=1,MYIE),J=1,MYJE)
c rec454
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((AKHS(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((THS(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((QS(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((TWBS(I,J),I=1,MYIE),J=1,MYJE)
     4,            ((QWBS(I,J),I=1,MYIE),J=1,MYJE)
     5,            ((HBOT(I,J),I=1,MYIE),J=1,MYJE)
     6,            ((CFRACL(I,J),I=1,MYIE),J=1,MYJE)
c rec455
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((THZ0(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((QZ0(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((UZ0(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((VZ0(I,J),I=1,MYIE),J=1,MYJE)
     4,            ((USTAR(I,J),I=1,MYIE),J=1,MYJE)
     5,            ((HTOP(I,J),I=1,MYIE),J=1,MYJE)
     6,            ((CFRACM(I,J),I=1,MYIE),J=1,MYJE)
c rec456
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((SNO(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((SI(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((CLDEFI(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((RF(I,J),I=1,MYIE),J=1,MYJE)
     4,            ((PSLP(I,J),I=1,MYIE),J=1,MYJE)
     5,            ((CUPPT(I,J),I=1,MYIE),J=1,MYJE)
     6,            ((CFRACH(I,J),I=1,MYIE),J=1,MYJE)
c rec457
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((SOILTB(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((SFCEXC(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((SMSTAV(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((SMSTOT(I,J),I=1,MYIE),J=1,MYJE)
     4,            ((GRNFLX(I,J),I=1,MYIE),J=1,MYJE)
     5,            ((PCTSNO(I,J),I=1,MYIE),J=1,MYJE)
c rec458
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((RLWIN(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((RADOT(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((CZMEAN(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((SIGT4(I,J),I=1,MYIE),J=1,MYJE)
c rec459
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((U00(I,J),I=1,MYIE),J=1,MYJE)
     1,              UL
     2,            ((LC(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((SR(I,J),I=1,MYIE),J=1,MYJE)
c rec460
C----------------------------------------------------------------------
C
      LABEL = 'ACCUMULATED VARIABLES'
      WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
     1,          ((PREC(I,J),I=1,MYIE),J=1,MYJE)
     2,          ((ACPREC(I,J),I=1,MYIE),J=1,MYJE)
     3,          ((ACCLIQ(I,J),I=1,MYIE),J=1,MYJE)
     4,          ((CUPREC(I,J),I=1,MYIE),J=1,MYJE)
c rec461
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((ACFRCV(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((NCFRCV(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((ACFRST(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((NCFRST(I,J),I=1,MYIE),J=1,MYJE)
c rec462
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((ACSNOW(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((ACSNOM(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((SSROFF(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((BGROFF(I,J),I=1,MYIE),J=1,MYJE)
c rec463
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((SFCSHX(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((SFCLHX(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((SUBSHX(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((SNOPCX(I,J),I=1,MYIE),J=1,MYJE)
     4,            ((SFCUVX(I,J),I=1,MYIE),J=1,MYJE)
     5,            ((SFCEVP(I,J),I=1,MYIE),J=1,MYJE)
     6,            ((POTEVP(I,J),I=1,MYIE),J=1,MYJE)
c rec464
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((ASWIN(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((ASWOUT(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((ASWTOA(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((ALWIN(I,J),I=1,MYIE),J=1,MYJE)
     4,            ((ALWOUT(I,J),I=1,MYIE),J=1,MYJE)
     5,            ((ALWTOA(I,J),I=1,MYIE),J=1,MYJE)
C
      WRITE(LRSTRT)ARDSW,ARDLW,ASRFC,AVRAIN,AVCNVC
c rec465
C
      WRITE(LRSTRT)((TH10(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((Q10(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((U10(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((V10(I,J),I=1,MYIE),J=1,MYJE)
     4,            ((TSHLTR(I,J),I=1,MYIE),J=1,MYJE)
     5,            ((QSHLTR(I,J),I=1,MYIE),J=1,MYJE)
     6,            ((PSHLTR(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((TH30(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((Q30(I,J),I=1,MYIE),J=1,MYJE)
     3,            ((U30(I,J),I=1,MYIE),J=1,MYJE)
     4,            ((V30(I,J),I=1,MYIE),J=1,MYJE)
c rec466
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)(((SMC(I,J,N),I=1,MYIE),J=1,MYJE),N=1,NSOIL)
c rec467
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)((CMC(I,J),I=1,MYIE),J=1,MYJE)
c rec468
C----------------------------------------------------------------------
C
      WRITE(LRSTRT)(((STC(I,J,N),I=1,MYIE),J=1,MYJE),N=1,NSOIL)
C
      WRITE(LRSTRT)(((SH2O(I,J,N),I=1,MYIE),J=1,MYJE),N=1,NSOIL)
      WRITE(LRSTRT)((ALBEDO(I,J),I=1,MYIE),J=1,MYJE)
c rec469
C----------------------------------------------------------------------
      WRITE(LRSTRT)((FQU(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((FQV(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((DQFLX(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,           ((FCU(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     5,           ((FCV(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     6,           ((DCFLX(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     7,           ((FQU7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     8,           ((FQV7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     9,           ((DQFLX7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     &,           ((FCU7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((FCV7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((DCFLX7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((DQADV(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,           ((FQNEV1(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     5,           ((FQSEV1(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C----------------------------------------------------------------------
      WRITE(LRSTRT)((VAPINC(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((CLDINC(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((VAPINC7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,           ((CLDINC7(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C----------------------------------------------------------------------
      WRITE(LRSTRT)((POTFLX(I,J),I=1,MYIE),J=1,MYJE)
     1,            ((TLMIN(I,J),I=1,MYIE),J=1,MYJE)
     2,            ((TLMAX(I,J),I=1,MYIE),J=1,MYJE)
     3,              ACUTIM,ARATIM,APHTIM
     4,              NHEAT,NPHS,NCNVC,NPREC,NRDSW,NRDLW,NSRFC
     5,              TPH0D,TLM0D,RESTRT
c rec470
C----------------------------------------------------------------------
      DO L=1,LM
        WRITE(LRSTRT)((RSWTT(I,J,L),I=1,MYIE),J=1,MYJE)
        WRITE(LRSTRT)((RLWTT(I,J,L),I=1,MYIE),J=1,MYJE)
      ENDDO
c rec560
C----------------------------------------------------------------------
C***
C***  CLOSE THE RESTART FILE.
C***
      CLOSE(LRSTRT)
c
      dif_tim=timef()-btim0
      wrt_tim=wrt_tim+dif_tim
      call mpi_reduce(wrt_tim,wrt_tim_0,1,MPI_REAL,MPI_MAX,0,
     1                my_comm,ierr)
      if(mype.eq.0)then
        write(6,*)' WROTE RESTRT FILE, time = ', wrt_tim_0*1.e-03
      endif
      CALL MPI_BARRIER(my_comm,ISTAT)
C***
C***  SEND SIGNAL THAT ALL TASKS HAVE FINISHED WRITING
C***
      IF(MYPE.EQ.0)THEN
        DONE='DONE'
        WRITE(FINFIL,1190)ITAG,RESTHR
 1190   FORMAT('3dvrdone',I2.2,'.',A4)
        LFINFIL=91
        CLOSE(LFINFIL)
        OPEN(UNIT=LFINFIL,FILE=FINFIL,FORM='UNFORMATTED',IOSTAT=IER)
        WRITE(LFINFIL)DONE
        CLOSE(LFINFIL)
        IF(IER.NE.0)WRITE(LIST,*)' SIGNAL SENT TO FINFIL:  DONE'
      ENDIF
C----------------------------------------------------------------------

!!!!!!!!!!!!!!!! eliminated everything from here to end of CHKOUT 

      RETURN
      END
