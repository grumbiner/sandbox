                  PROGRAM POST0
C
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .
C MAIN PROGRAM:  POST0       BCEX FOR NESTS, & PROFILES
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-04-21
C
C ABSTRACT:  THIS PROGRAM PRODUCES THE RAW PROFILE FILES
C            AND EXTRACTS THE BOUNDARIES FOR THE NESTED
C            DOMAINS.  THESE JOBS WERE PREVIOUSLY DONE
C            INSIDE THE ETA MODEL FORECAST CODE IN
C            SUBROUTINE CHKOUT.
C
C PROGRAM HISTORY LOG:
C   99-04-21  T BLACK - EXTRACTED THE RELEVANT PARTS OF CHKOUT
C
C USAGE:  MAIN PROGRAM
C
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   INPUT FILES:  NONE 
C
C   OUTPUT FILES:  NONE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE: 
C             PROF - GENERATE PROFILE SOUNDINGS
C             BCEX - BC EXTRACTION FOR NESTED ETA RUNS
C
C   EXIT STATES:
C     COND =   0 - NORMAL EXIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C
C$$$
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
C-----------------------------------------------------------------------
                              P A R A M E T E R
     & (PTOP=2500.)
C-----------------------------------------------------------------------
                              R E A L
     & TBCEND(9),IMI(9),JMI(9),LMI(9),TPH0DI(9),TLM0DI(9)
     &,PD(IM,JM),USTAR(IM,JM)
     &,T(IM,JM,LM),Q(IM,JM,LM),U(IM,JM,LM),V(IM,JM,LM)
     &,Q2(IM,JM,LM),CWM(IM,JM,LM)
     &,PD0(IM,JM),USTAR0(IM,JM)
     &,T0(IM,JM,LM),Q0(IM,JM,LM),U0(IM,JM,LM),V0(IM,JM,LM)
     &,Q20(IM,JM,LM),CWM0(IM,JM,LM)
     &,HTM(IM,JM,LM),VTM(IM,JM,LM)
C-----------------------------------------------------------------------
                              I N T E G E R
     & IDAT(3),IDAT0(3)
C-----------------------------------------------------------------------
                              L O G I C A L
     & EXBC,NEST,RUN
C-----------------------------------------------------------------------
                              C H A R A C T E R
     & RSTFIL*50,RESTHR*4
C-----------------------------------------------------------------------
      DATA NHB/12/,LRSTRT/22/,LCLAS1/76/
     &,    IUDETO/30/,IUDETI/30/,IUNWGT/40/,IUBCF/60/
C-----------------------------------------------------------------------
      real*8 timef
C-----------------------------------------------------------------------
C***********************************************************************
C-----------------------------------------------------------------------
      btim=timef()
C***
C***  READ IN THE FIRST FORECAST HOUR THAT THIS JOB WILL WORK ON
C***  PLUS THE NUMBER OF OUTPUT TIMES TO PROCESS AND THE TIME INCREMENT
C***  IN HOURS BETWEEN PROCESS TIMES
C***
      READ(5,*)IHOUR,KOUNT,NINC
      print*,'IHOUR,KOUNT,NINC=',IHOUR,KOUNT,NINC
C-----------------------------------------------------------------------
C***
C***  LOOP OVER ALL THE OUTPUT TIMES
C***
      DO 100 NK=1,KOUNT
C-----------------------------------------------------------------------
C***
C***  GENERATION OF PROFILES
C***
      CALL PROF(NHB,LRSTRT,IHOUR,LCLAS1)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C***
C***  DO BOUNDARY EXTRACTION FOR NESTS ONLY IF 
C***  THIS JOB'S FORECAST HOUR IS GREATER THAN 0
C***  SINCE WE MUST REACH BACK ONE HOUR TO CREATE TENDENCIES
C***
      IF(IHOUR.GT.0)THEN
C-----------------------------------------------------------------------
C***
C***  BOUNDARY CONDITION EXTRACTION FOR ETA NESTS
C***
C-----------------------------------------------------------------------
        CALL BCEX(NHB,LRSTRT,IHOUR,PTOP
     1,           IUNWGT,IUDETO,IUDETI,IUBCF)
C-----------------------------------------------------------------------
      ENDIF
C-----------------------------------------------------------------------
      IHOUR=IHOUR+NINC
  100 CONTINUE
C-----------------------------------------------------------------------
      tot_tim=timef()-btim
      write(6,*)' tot_tim=',tot_tim*1.e-3
      STOP0
      END
