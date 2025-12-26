      SUBROUTINE WRTRST
C$$$  PROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C    PROGRAM:    WRTRST      WRITE OUT A 2-D RESTRT FILE
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 96-06-24
C     
C ABSTRACT:  WRTRST SIMPLY TAKES AN EXISTING INITIAL RESTRT 2-D
C            RESTRT FILE AND WRITES IT OUT.  IT IS VERY
C            IMPORTANT TO REMEMBER THAT THIS IS FOR AN INITIAL
C            RESTRT FILE ONLY SINCE SOME OF THE WRITES ARE
C            DUMMIES THAT ARE NOT NEEDED BY INIT BUT THAT
C            WOULD BE NEEDED BY THE POST_PROCESSOR
C     
C PROGRAM HISTORY LOG:
C   96-06-24  BLACK   - ORIGINATOR
C     
C USAGE:    RUN WRTRST AFTER ANY ROUTINES THAT HAVE MODIFIED
C           A 2-D RESTRT FILE 
C
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   INPUT FILES:
C     NONE
C
C   OUTPUT FILES:
C     RESOUT - THE RESTRT FILE IN 2-D FORM
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK
C                  LOOPS
C                  MASKS
C                  DYNAM
C                  PHYS2
C                  MAPOT1
C                  SCRCH
C                  VRBLS
C                  PVRBLS
C                  BOCO
C                  GRIDS
C                  ACMCLH
C                  ACMCLD
C                  ACMPRE
C                  ACMRDL
C                  ACMRDS
C                  ACMSFC
C                  CLDWTR
C                  CNVCLD
C                  SOIL
C    
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM SP
C$$$  
C
C-----------------------------------------------------------------------
C     INCLUDE/SET PARAMETERS.
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
C-----------------------------------------------------------------------
                              P A R A M E T E R
     & (IMJM=IM*JM-JM/2,JMP1=JM+1,JAM=6+2*(JM-10),IMJMX=IM*JM
     &, LM1=LM-1,LP1=LM+1,LB=2*IM+JM-3)
C
                              P A R A M E T E R
     & (NSOIL=2)
C-----------------------------------------------------------------------
C     
C                            DECLARE VARIABLES
C     
C-----------------------------------------------------------------------
                              L O G I C A L
     & RUN,RUNB,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
                              I N T E G E R
     & IDATB(3)
C-----------------------------------------------------------------------
                              C H A R A C T E R *32
     & LABEL1,LABEL2,LABEL3,LABEL4
C-----------------------------------------------------------------------
                              R E A L
     & RDUM(IM,JM)
                              I N T E G E R
     & IDUM(IM,JM)
C-----------------------------------------------------------------------
C     
C     INCLUDE COMMON BLOCKS.
C
      INCLUDE "CTLBLK.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "DYNAM.comm"
      INCLUDE "PHYS2.comm"
      INCLUDE "MAPOT1.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "CONTIN.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "BOCO.comm"
      INCLUDE "ACMCLH.comm"
      INCLUDE "ACMCLD.comm"
      INCLUDE "ACMPRE.comm"
      INCLUDE "ACMRDL.comm"
      INCLUDE "ACMRDS.comm"
      INCLUDE "ACMSFC.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "SOLAR.comm"
      INCLUDE "SOIL.comm"
C----------------------------------------------------------------------     
                              D A T A
     & IDUM/IMJMX*0/,RDUM/IMJMX*0./,DUMMY1/0./,DUMMY2/0./,NRSOUT/80/
C***********************************************************************
C***
C***  WRITE OUT 2-D RESTRT FILE
C***
      LABEL1='OMEGA-ALPHA*DT/CP'
      WRITE(NRSOUT) RUN,IDAT,IHRST,NTSD,LABEL1
      WRITE(NRSOUT) PD,((RES(I,J),I=1,IM),J=1,JM)
      DO L = 1,LM
        WRITE(NRSOUT)((OMGALF(I,J,L),I=1,IM),J=1,JM)
      ENDDO
C
      LABEL2='BND,VRBLS,Q2,CWM,TRAIN,TCUCN'
      WRITE(NRSOUT) RUN,IDAT,IHRST,NTSD,LABEL2,
     &            FIRST,IOUT,NSHDE
      WRITE(NRSOUT) PD,((RES(I,J),I=1,IM),J=1,JM),FIS,DUMMY1,DUMMY2
      WRITE(NRSOUT) PDB,TB,QB,UB,VB
C
      DO L = 1,LM
        WRITE(NRSOUT)((T(I,J,L),I=1,IM),J=1,JM)
        WRITE(NRSOUT)((Q(I,J,L),I=1,IM),J=1,JM)
        WRITE(NRSOUT)((U(I,J,L),I=1,IM),J=1,JM)
        WRITE(NRSOUT)((V(I,J,L),I=1,IM),J=1,JM)
        WRITE(NRSOUT)((Q2(I,J,L),I=1,IM),J=1,JM)
C
C     DUMMY WRITE OF THE TOTAL RADIATIVE TEMPERATURE TENDENCIES
C     WHICH ARE NOT USED EXPLICITLY IN THE INTEGRATION
C
        WRITE(NRSOUT) RDUM
C
        WRITE(NRSOUT)((CWM(I,J,L),I=1,IM),J=1,JM)
        WRITE(NRSOUT)((TRAIN(I,J,L),I=1,IM),J=1,JM)
        WRITE(NRSOUT)((TCUCN(I,J,L),I=1,IM),J=1,JM)
      ENDDO
C
      LABEL3='MISC VARIABLES'
      WRITE(NRSOUT) RUN,IDAT,IHRST,NTSD,LABEL3,
     &            RSWIN,RSWOUT,TG,Z0,((AKMS(I,J),I=1,IM),J=1,JM),CZEN
      WRITE(NRSOUT) AKHS,THS,QS,TWBS,QWBS,HBOT,CFRACL
      WRITE(NRSOUT) THZ0,QZ0
     &,            ((UZ0(I,J),I=1,IM),J=1,JM)
     &,            ((VZ0(I,J),I=1,IM),J=1,JM)
     &,              USTAR,HTOP,CFRACM
      WRITE(NRSOUT) SNO,SI,CLDEFI,RF,RDUM,CUPPT,CFRACH
      WRITE(NRSOUT) SOILTB,SFCEXC,SMSTAV,SMSTOT,GRNFLX,PCTSNO
      WRITE(NRSOUT) RLWIN,RADOT,CZMEAN,SIGT4
      WRITE(NRSOUT) U00,UL,LC,SR
C
      LABEL4='ACCUMULATED VARIABLES'
      WRITE(NRSOUT) RUN,IDAT,IHRST,NTSD,LABEL4,
     &            PREC,ACPREC,ACCLIQ,CUPREC
      WRITE(NRSOUT) ACFRCV,NCFRCV,ACFRST,NCFRST
      WRITE(NRSOUT) ACSNOW,ACSNOM,SSROFF,BGROFF,IDUM
      WRITE(NRSOUT) SFCSHX,SFCLHX,SUBSHX,SNOPCX,
     &            SFCUVX,SFCEVP,POTEVP
      WRITE(NRSOUT) ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA
      WRITE(NRSOUT) ARDSW,ARDLW,ASRFC,AVRAIN,AVCNVC
      WRITE(NRSOUT) TH10,Q10,U10,V10,TSHLTR,QSHLTR,RDUM
      WRITE(NRSOUT) SMC
      WRITE(NRSOUT) CMC
      WRITE(NRSOUT) STC
      WRITE(NRSOUT) SH2O
      WRITE(NRSOUT) ALBEDO
C
      STOP   
      END
