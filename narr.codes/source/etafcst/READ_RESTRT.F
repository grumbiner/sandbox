C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                        SUBROUTINE READ_RESTRT
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .

C SUBPROGRAM:    READ_RESTRT READ AND DISTRIBUTE RESTRT FILE
C   PRGRMMR: BLACK           ORG: W/NP2      DATE: 98-10-22
C
C ABSTRACT:
C     READ_RESTRT READS IN QUANTITIES FROM THE NFC FILE OR THE
C     RESTRT FILE AND DISTRIBUTES THEM TO THE OTHER NODES/PEs 
C
C PROGRAM HISTORY LOG:
C   97-??-??  MEYS       - ORIGINATOR
C   97-08-??  BLACK      - REWROTE FOR BENCHMARK
C   98-??-??  TUCCILLO   - MODIFIED FOR SINGLE OR DOUBLE PRECISION
C   98-10-23  BLACK      - MODIFIED FOR NEWEST RESTART FILE
C
C USAGE: CALL READ_RESTRT FROM SUBROUTINE INIT
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
C     UNIQUE: DSTRB
C             IDSTRB
C
C     LIBRARY: NONE
C
C   COMMON BLOCKS: CTLBLK
C                  LOOPS
C                  MASKS
C                  DYNAM
C                  PHYS2
C                  MAPOT1
C                  VRBLS
C                  CONTIN
C                  PVRBLS
C                  BOCO
C                  ACMCLH
C                  ACMCLD
C                  ACMPRE
C                  ACMRDL
C                  ACMRDS
C                  ACMSFC
C                  PRFHLD
C                  CLDWTR
C                  CNVCLD
C                  SOIL
C                  INDX
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM SP
C$$$
C-----------------------------------------------------------------------
C     INCLUDE/SET PARAMETERS.
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "parmsoil"
C-----------------------------------------------------------------------
                              P A R A M E T E R
     & (D00=0.0,D50=.50,H1=1.0,G=9.8
     &, CM1=2937.4,CM2=4.9283,CM3=23.5518,EPS=0.622
C
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
C    &, Q2INI=.01E0,EPSQ2=1.E-4,EPSQ=2.E-12,EPSWET=1.E-4
C    &, Q2INI=1.0E0,EPSQ2=1.E-4,EPSQ=2.E-12,EPSWET=1.E-4
C    &, Q2INI=.50E0,EPSQ2=1.E-4,EPSQ=2.E-12,EPSWET=1.E-4
C    &, Q2INI=.01E0,EPSQ2=1.E-4,EPSQ=2.E-12,EPSWET=0.0E0
     &, Q2INI=.50,EPSQ2=2.E-2,EPSQ=2.E-12,EPSWET=0.0
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
     &, Z0LAND=.10,Z0SEA=.001,FCM=.00001
     &, DTR=0.1745329E-1,H360=360.0
     &, H1905=190.5,H105=105.0)
C
C-----------------------------------------------------------------------
                              P A R A M E T E R
     & (IMJM=IM*JM-JM/2,JMP1=JM+1,JAM=6+2*(JM-10),LB=2*IM+JM-3
     &, LM1=LM-1,LP1=LM+1,IMT=2*IM-1)
C
C-----------------------------------------------------------------------
C     
C                            DECLARE VARIABLES
C     
C-----------------------------------------------------------------------
                              L O G I C A L
     & RUN,RUNB,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
                              C H A R A C T E R *32
     & LABEL
                              C H A R A C T E R *40
     & CONTRL,FILALL,FILMST,FILTMP,FILTKE,FILUNV
     &,FILCLD,FILRAD,FILSFC
C-----------------------------------------------------------------------
                              R E A L
     & PSLP(IDIM1:IDIM2,JDIM1:JDIM2)
                              R E A L
     & TEMPSOIL(IM,JM,NSOIL)
C-----------------------------------------------------------------------
                              I N T E G E R
     & IDATB(3)
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
      INCLUDE "PRFHLD.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "TEMPV.comm"
      INCLUDE "INDX.comm"
      INCLUDE "QFLX.comm"
      INCLUDE "PPTASM.comm"
C
C-----------------------------------------------------------------------
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
C
#ifdef DP_REAL
      LOGICAL*8 RUNX,FIRSTX
      INTEGER*8 IDATX(3),IHRSTX,NTSDX
      INTEGER*8 IOUTX,NSHDEX
      INTEGER*8 ITEMPX(IM,JM),ITEMP2X(IM,JM)
#endif
C
C***********************************************************************
C***********************************************************************
C
C***  TSTART IS THE INITIAL TIME IN HOURS
C
      TSTART=NSTART*DT/3600.
C
C     READ INITIAL CONDITIONS OR RESTART FILE.
C     FIRST, THE .NOT. RESTART FILE CASE.
C
      IF(.NOT.RESTRT)THEN
        IF(MYPE.EQ.0)THEN
#ifdef DP_REAL
          READ(NFCST)RUNX,IDATX,IHRSTX,NTSDX
          RUN=RUNX
          IDAT=IDATX
          IHRST=IHRSTX
          NTSD=NTSDX
#else
          READ(NFCST)RUN,IDAT,IHRST,NTSD
#endif
          IF(NTSD.EQ.1)NTSD=0
        ELSE
          READ(NFCST)DUMMY
        ENDIF
C
        CALL MPI_BCAST(RUN,1,MPI_LOGICAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(IDAT(1),3,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(IHRST,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(NTSD,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
C
        CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE PD
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1
c         WRITE(0,*)'READ PD'
        ENDIF
C
        CALL DSTRB(TEMP1,PD,1,1,1)
C
C2345678901234567890123456789012345678901234567890123456789012345678901
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE RES
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1
c         WRITE(0,*)'READ RES'
        ENDIF
C
        CALL DSTRB(TEMP1,RES,1,1,1)
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE FIS
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1
c         WRITE(0,*)'READ FIS'
        ENDIF
C
        CALL DSTRB(TEMP1,FIS,1,1,1)
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE U
C***
        DO L=1,LM
          IF(MYPE.EQ.0)THEN
            READ(NFCST)TEMP1
c           WRITE(0,*)'READ U'
          ENDIF
          CALL DSTRB(TEMP1,U,1,LM,L)
        ENDDO
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE V
C***
        DO L=1,LM
          IF(MYPE.EQ.0)THEN
            READ(NFCST)TEMP1
c           WRITE(0,*)'READ V'
          ENDIF
          CALL DSTRB(TEMP1,V,1,LM,L)
        ENDDO
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE T
C***
        DO L=1,LM
          IF(MYPE.EQ.0)THEN
            READ(NFCST)TEMP1
c           WRITE(0,*)'READ T'
          ENDIF
          CALL DSTRB(TEMP1,T,1,LM,L)
        ENDDO
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE Q
C***
        DO L=1,LM
          IF(MYPE.EQ.0)THEN
            READ(NFCST)TEMP1
c           WRITE(0,*)'READ Q'
          ENDIF
          CALL DSTRB(TEMP1,Q,1,LM,L)
        ENDDO
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE SI
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1
c         WRITE(0,*)'READ SI'
        ENDIF
C
        CALL DSTRB(TEMP1,SI,1,1,1)
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE SNO
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1
c         WRITE(0,*)'READ SNO'
        ENDIF
C
        CALL DSTRB(TEMP1,SNO,1,1,1)
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE SMC
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMPSOIL
c         WRITE(0,*)'READ SMC'
        ENDIF
C
        CALL DSTRB(TEMPSOIL,SMC,NSOIL,NSOIL,NSOIL)
C----------------------------------------------------------------------
C***
C***
C***  DISTRIBUTE CMC
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1
c         WRITE(0,*)'READ CMC'
        ENDIF
C
        CALL DSTRB(TEMP1,CMC,1,1,1)
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE STC
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMPSOIL
c         WRITE(0,*)'READ STC'
        ENDIF
C
        CALL DSTRB(TEMPSOIL,STC,NSOIL,NSOIL,NSOIL)
C----------------------------------------------------------------------
C***
C***  DISTRIBUTE SH2O
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMPSOIL
c         WRITE(0,*)'READ SH2O'
        ENDIF
C
        CALL DSTRB(TEMPSOIL,SH2O,NSOIL,NSOIL,NSOIL)
C-------------------------------------------------------------------
C***
C***
C***  DISTRIBUTE ALBEDO
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1
c         WRITE(0,*)'READ ALBEDO'
        ENDIF
C
        CALL DSTRB(TEMP1,ALBEDO,1,1,1)
C----------------------------------------------------------------------
C
        IYR  =IDAT(3)-1900
        IMNTH=IDAT(1)
        IDAY =IDAT(2)
        IF(MYPE.EQ.0)WRITE(LIST,*)'INIT: READ INITIAL CONDITION FILE'
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C     
C     SECOND, THE RESTART FILE CASE.
C
C-------------------------------------------------------------------
C-------------------------------------------------------------------
      ELSE
        IF(MYPE.EQ.0)WRITE(LIST,*)'INIT:  READ RESTART FILE'
        IF(MYPE.EQ.0)THEN
#ifdef DP_REAL
          READ(NFCST)RUNX,IDATX,IHRSTX,NTSDX,LABEL
          RUN=RUNX
          IDAT=IDATX
          IHRST=IHRSTX
          NTSD=NTSDX
#else
          READ(NFCST)RUN,IDAT,IHRST,NTSD,LABEL
#endif
          IF(NTSD.EQ.1)NTSD=0
          READ(NFCST)PDOMG,RESOMG
        ENDIF
C
        CALL MPI_BCAST(RUN,1,MPI_LOGICAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(IDAT(1),3,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(IHRST,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(NTSD,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
c       CALL MPI_BCAST(LABEL,1,MPI_CHARACTER,0,MPI_COMM_COMP,IRTN)
c       CALL MPI_BCAST(PDOMG,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
c       CALL MPI_BCAST(RESOMG,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
C
C-------------------------------------------------------------------
C***
C***  DISTRIBUTE OMGALF
C***
        DO L=1,LM
          IF(MYPE.EQ.0)THEN
            READ(NFCST)TEMP1
c           WRITE(0,*)'READ OMGALF'
          ENDIF
          CALL DSTRB(TEMP1,OMGALF,1,LM,L)
        ENDDO
C-------------------------------------------------------------------
C
        IF(MYPE.EQ.0)WRITE(LIST,*)'  READ  ',LABEL
C
        IF(MYPE.EQ.0)THEN
#ifdef DP_REAL
          READ(NFCST)RUNX,IDATX,IHRSTX,NTSDX,LABEL,FIRSTX,IOUTX,NSHDEX
          RUN=RUNX
          IDAT=IDATX
          IHRST=IHRSTX
          NTSD=NTSDX
          FIRST=FIRSTX
          IOUT=IOUTX
          NSHDE=NSHDEX
#else
          READ(NFCST)RUN,IDAT,IHRST,NTSD,LABEL,FIRST,IOUT,NSHDE
#endif
          IF(NTSD.EQ.1)NTSD=0
        ENDIF
C
        FIRST=.TRUE.
C
        CALL MPI_BCAST(RUN,1,MPI_LOGICAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(IDAT(1),3,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(IHRST,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(NTSD,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
c       CALL MPI_BCAST(LABEL,1,MPI_CHARACTER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(FIRST,1,MPI_LOGICAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(IOUT,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(NSHDE,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)

C-------------------------------------------------------------------
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1,TEMP2,TEMP3
c         WRITE(0,*)'READ PD'
        ENDIF
C
        CALL DSTRB(TEMP1,PD,1,1,1)
        CALL DSTRB(TEMP2,RES,1,1,1)
        CALL DSTRB(TEMP3,FIS,1,1,1)
C-------------------------------------------------------------------
C***
        LBM2=LB*LM*2
        IF(MYPE.EQ.0)THEN
          IF(NINT(TSTART).EQ.0)THEN
            READ(NFCST)PDB,TB,QB,UB,VB
          ELSE
            READ(NFCST)PDB,TB,QB,UB,VB,Q2B,CWMB
          ENDIF
        ENDIF
C
        CALL MPI_BCAST(PDB(1,1),LB,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(PDB(1,2),LB,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(TB(1,1,1),LBM2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(QB(1,1,1),LBM2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(UB(1,1,1),LBM2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(VB(1,1,1),LBM2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
C
        IF(NINT(TSTART).GT.0)THEN
          CALL MPI_BCAST(Q2B(1,1,1),LBM2,MPI_REAL,0,MPI_COMM_COMP
     1,                  IRTN)
          CALL MPI_BCAST(CWMB(1,1,1),LBM2,MPI_REAL,0,MPI_COMM_COMP
     1,                  IRTN)
        ENDIF
C-------------------------------------------------------------------
C***
C***  PRIMARY 3-D VARIABLES
C***
        DO 300 L=1,LM
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1   ! T(I,J,L)
          READ(NFCST)TEMP2   ! Q(I,J,L)
          READ(NFCST)TEMP3   ! U(I,J,L)
          READ(NFCST)TEMP4   ! V(I,J,L)
          READ(NFCST)TEMP5   ! Q2(I,J,L)
        ENDIF
C
        CALL DSTRB(TEMP1,T,1,LM,L)
        CALL DSTRB(TEMP2,Q,1,LM,L)
        CALL DSTRB(TEMP3,U,1,LM,L)
        CALL DSTRB(TEMP4,V,1,LM,L)
        CALL DSTRB(TEMP5,Q2,1,LM,L)
C
C     DUMMY READ OF THE TOTAL RADIATIVE TEMPERATURE TENDENCIES
C     WHICH ARE NOT USED EXPLICITLY IN THE INTEGRATION
C
        IF(MYPE.EQ.0)THEN
          READ(NFCST)
C
          READ(NFCST)((TEMP6(I,J),I=1,IM),J=1,JM)  ! CWM(I,J,L)
          READ(NFCST)((TEMP7(I,J),I=1,IM),J=1,JM)  ! TRAIN(I,J,L)
          READ(NFCST)((TEMP8(I,J),I=1,IM),J=1,JM)   ! TCUCN(I,J,L)
C
        ENDIF
C
        CALL DSTRB(TEMP6,CWM,1,LM,L)
        CALL DSTRB(TEMP7,TRAIN,1,LM,L)
        CALL DSTRB(TEMP8,TCUCN,1,LM,L)
C
  300   CONTINUE
C-------------------------------------------------------------------
C
        IF(MYPE.EQ.0)WRITE(LIST,*)'  READ  ',LABEL
C
        IF(MYPE.EQ.0)THEN
#ifdef DP_REAL
          READ(NFCST)RUNX,IDATX,IHRSTX,NTSDX,LABEL
     1,              TEMP1,TEMP2,TEMP3
     2,              TEMP4,((TEMP5(I,J),I=1,IM),J=1,JM),TEMP6
          RUN=RUNX
          IDAT=IDATX
          IHRST=IHRSTX
          NTSD=NTSDX
#else
          READ(NFCST)RUN,IDAT,IHRST,NTSD,LABEL
     1,              TEMP1,TEMP2,TEMP3
     2,              TEMP4,((TEMP5(I,J),I=1,IM),J=1,JM),TEMP6
#endif
          IF(NTSD.EQ.1)NTSD=0
        ENDIF
C
        CALL DSTRB(TEMP1,RSWIN,1,1,1)
        CALL DSTRB(TEMP2,RSWOUT,1,1,1)
        CALL DSTRB(TEMP3,TG,1,1,1)
        CALL DSTRB(TEMP4,Z0,1,1,1)
        CALL DSTRB(TEMP5,AKMS,1,1,1)
        CALL DSTRB(TEMP6,CZEN,1,1,1)
C
        CALL MPI_BCAST(RUN,1,MPI_LOGICAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(IDAT(1),3,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(IHRST,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(NTSD,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
c       CALL MPI_BCAST(LABEL,1,MPI_CHARACTER,0,MPI_COMM_COMP,IRTN)
C
C-------------------------------------------------------------------
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7
        ENDIF
C
        CALL DSTRB(TEMP1,AKHS,1,1,1)
        CALL DSTRB(TEMP2,THS,1,1,1)
        CALL DSTRB(TEMP3,QS,1,1,1)
        CALL DSTRB(TEMP4,TWBS,1,1,1)
        CALL DSTRB(TEMP5,QWBS,1,1,1)
        CALL DSTRB(TEMP6,HBOT,1,1,1)    ! Used to be CNVBOT
        CALL DSTRB(TEMP7,CFRACL,1,1,1)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1,TEMP2
     1,            ((TEMP3(I,J),I=1,IM),J=1,JM)
     2,            ((TEMP4(I,J),I=1,IM),J=1,JM)
     3,              TEMP5,TEMP6,TEMP7
        ENDIF
C
        CALL DSTRB(TEMP1,THZ0,1,1,1)
        CALL DSTRB(TEMP2,QZ0,1,1,1)
        CALL DSTRB(TEMP3,UZ0,1,1,1)
        CALL DSTRB(TEMP4,VZ0,1,1,1)
        CALL DSTRB(TEMP5,USTAR,1,1,1)
        CALL DSTRB(TEMP6,HTOP,1,1,1)   ! Used to be CNVTOP
        CALL DSTRB(TEMP7,CFRACM,1,1,1)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7
        ENDIF
C
        CALL DSTRB(TEMP1,SNO,1,1,1)
        CALL DSTRB(TEMP2,SI,1,1,1)
        CALL DSTRB(TEMP3,CLDEFI,1,1,1)
        CALL DSTRB(TEMP4,RF,1,1,1)
        CALL DSTRB(TEMP5,PSLP,1,1,1)
        CALL DSTRB(TEMP6,CUPPT,1,1,1)
        CALL DSTRB(TEMP7,CFRACH,1,1,1)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6
        ENDIF
C
        CALL DSTRB(TEMP1,SOILTB,1,1,1)
        CALL DSTRB(TEMP2,SFCEXC,1,1,1)
        CALL DSTRB(TEMP3,SMSTAV,1,1,1)
        CALL DSTRB(TEMP4,SMSTOT,1,1,1)
        CALL DSTRB(TEMP5,GRNFLX,1,1,1)
        CALL DSTRB(TEMP6,PCTSNO,1,1,1)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1
     1,              ((TEMP2(I,J),I=1,IM),J=1,JM)
     2,                TEMP3,TEMP4
        ENDIF
C
        CALL DSTRB(TEMP1,RLWIN,1,1,1)
        CALL DSTRB(TEMP2,RADOT,1,1,1)
        CALL DSTRB(TEMP3,CZMEAN,1,1,1)
        CALL DSTRB(TEMP4,SIGT4,1,1,1)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
#ifdef DP_REAL
          READ(NFCST)TEMP1,UL,ITEMPX,TEMP3
          ITEMP=ITEMPX
#else
          READ(NFCST)TEMP1,UL,ITEMP,TEMP3
#endif
        ENDIF
C
        CALL DSTRB(TEMP1,U00,1,1,1)
        CALL IDSTRB(ITEMP,LC)
        CALL DSTRB(TEMP3,SR,1,1,1)
        CALL MPI_BCAST(UL(1),2*LM,MPI_REAL,0,MPI_COMM_COMP,IRTN)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
#ifdef DP_REAL
          READ(NFCST)RUNX,IDATX,IHRSTX,NTSDX,LABEL
     1,              TEMP1,TEMP2,TEMP3,TEMP4
          RUN=RUNX
          IDAT=IDATX
          IHRST=IHRSTX
          NTSD=NTSDX
#else
          READ(NFCST)RUN,IDAT,IHRST,NTSD,LABEL
     1,              TEMP1,TEMP2,TEMP3,TEMP4
#endif
          IF(NTSD.EQ.1)NTSD=0
        ENDIF
C
        CALL DSTRB(TEMP1,PREC,1,1,1)
        CALL DSTRB(TEMP2,ACPREC,1,1,1)
        CALL DSTRB(TEMP3,ACCLIQ,1,1,1)
        CALL DSTRB(TEMP4,CUPREC,1,1,1)
        CALL MPI_BCAST(RUN,1,MPI_LOGICAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(IDAT(1),3,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(IHRST,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(NTSD,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
c       CALL MPI_BCAST(LABEL,1,MPI_CHARACTER,0,MPI_COMM_COMP,IRTN)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
#ifdef DP_REAL
          READ(NFCST)TEMP1,ITEMPX,TEMP3,ITEMP2X
          ITEMP=ITEMPX
          ITEMP2=ITEMP2X
#else
          READ(NFCST)TEMP1,ITEMP,TEMP3,ITEMP2
#endif
        ENDIF
C
        CALL DSTRB(TEMP1,ACFRCV,1,1,1)
        CALL IDSTRB(ITEMP,NCFRCV)
        CALL DSTRB(TEMP3,ACFRST,1,1,1)
        CALL IDSTRB(ITEMP2,NCFRST)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1,TEMP2,TEMP3,TEMP4
        ENDIF
C
        CALL DSTRB(TEMP1,ACSNOW,1,1,1)
        CALL DSTRB(TEMP2,ACSNOM,1,1,1)
        CALL DSTRB(TEMP3,SSROFF,1,1,1)
        CALL DSTRB(TEMP4,BGROFF,1,1,1)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6
     1,              TEMP7
        ENDIF
C
        CALL DSTRB(TEMP1,SFCSHX,1,1,1)
        CALL DSTRB(TEMP2,SFCLHX,1,1,1)
        CALL DSTRB(TEMP3,SUBSHX,1,1,1)
        CALL DSTRB(TEMP4,SNOPCX,1,1,1)
        CALL DSTRB(TEMP5,SFCUVX,1,1,1)
        CALL DSTRB(TEMP6,SFCEVP,1,1,1)
        CALL DSTRB(TEMP7,POTEVP,1,1,1)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6
        ENDIF
C
        CALL DSTRB(TEMP1,ASWIN,1,1,1)
        CALL DSTRB(TEMP2,ASWOUT,1,1,1)
        CALL DSTRB(TEMP3,ASWTOA,1,1,1)
        CALL DSTRB(TEMP4,ALWIN,1,1,1)
        CALL DSTRB(TEMP5,ALWOUT,1,1,1)
        CALL DSTRB(TEMP6,ALWTOA,1,1,1)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)ARDSW,ARDLW,ASRFC,AVRAIN,AVCNVC
        ENDIF
C
        CALL MPI_BCAST(ARDSW,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(ARDLW,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(ASRFC,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(AVRAIN,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(AVCNVC,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
C
        CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C-------------------------------------------------------------------
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7
     1,              TEMP8,TEMP9,TEMP10,TEMP11
        ENDIF
C
        CALL DSTRB(TEMP1,TH10,1,1,1)
        CALL DSTRB(TEMP2,Q10,1,1,1)
        CALL DSTRB(TEMP3,U10,1,1,1)
        CALL DSTRB(TEMP4,V10,1,1,1)
        CALL DSTRB(TEMP5,TSHLTR,1,1,1)
        CALL DSTRB(TEMP6,QSHLTR,1,1,1)
        CALL DSTRB(TEMP7,PSHLTR,1,1,1)
        CALL DSTRB(TEMP8,TH30,1,1,1)
        CALL DSTRB(TEMP9,Q30,1,1,1)
        CALL DSTRB(TEMP10,U30,1,1,1)
        CALL DSTRB(TEMP11,V30,1,1,1)
C-------------------------------------------------------------------
C***
C***  DISTRIBUTE SMC
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMPSOIL
        ENDIF
C
        CALL DSTRB(TEMPSOIL,SMC,NSOIL,NSOIL,NSOIL)
C-------------------------------------------------------------------
C***
C***  DISTRIBUTE CMC
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1
        ENDIF
C
        CALL DSTRB(TEMP1,CMC,1,1,1)
C-------------------------------------------------------------------
C***
C***  DISTRIBUTE STC
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMPSOIL
        ENDIF
C
        CALL DSTRB(TEMPSOIL,STC,NSOIL,NSOIL,NSOIL)
C-------------------------------------------------------------------
C***
C***  DISTRIBUTE SH2O
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMPSOIL
c         WRITE(0,*)'READ SH2O'
        ENDIF
C
        CALL DSTRB(TEMPSOIL,SH2O,NSOIL,NSOIL,NSOIL)
C-------------------------------------------------------------------
C***
C***
C***  DISTRIBUTE ALBEDO
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1
c         WRITE(0,*)'READ ALBEDO'
        ENDIF
C
        CALL DSTRB(TEMP1,ALBEDO,1,1,1)
C----------------------------------------------------------------------
C***
C***
C***  DISTRIBUTE VERT. INT. FLX
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7
     1,              TEMP8,TEMP9,TEMP10,TEMP11,TEMP12,TEMP13
     2,              TEMP14,TEMP15
c         WRITE(0,*)'READ VERT. INT. FLX.'
        ENDIF
C
        CALL DSTRB(TEMP1,FQU,1,1,1)
        CALL DSTRB(TEMP2,FQV,1,1,1)
        CALL DSTRB(TEMP3,DQFLX,1,1,1)
        CALL DSTRB(TEMP4,FCU,1,1,1)
        CALL DSTRB(TEMP5,FCV,1,1,1)
        CALL DSTRB(TEMP6,DCFLX,1,1,1)

        CALL DSTRB(TEMP7,FQU7,1,1,1)
        CALL DSTRB(TEMP8,FQV7,1,1,1)
        CALL DSTRB(TEMP9,DQFLX7,1,1,1)
        CALL DSTRB(TEMP10,FCU7,1,1,1)
        CALL DSTRB(TEMP11,FCV7,1,1,1)
        CALL DSTRB(TEMP12,DCFLX7,1,1,1)

        CALL DSTRB(TEMP13,DQADV,1,1,1)
        CALL DSTRB(TEMP14,FQNEV1,1,1,1)
        CALL DSTRB(TEMP15,FQSEV1,1,1,1)
C----------------------------------------------------------------------
C***
C***
C***  DISTRIBUTE VERT. INT. MOISTURE INCREMENT DUE TO PREC. ASSIM.
C***
        IF(MYPE.EQ.0)THEN
          READ(NFCST)TEMP1,TEMP2,TEMP3,TEMP4
c         WRITE(0,*)'READ VERT. INT. FLX.'
        ENDIF
C
        CALL DSTRB(TEMP1,VAPINC,1,1,1)
        CALL DSTRB(TEMP2,CLDINC,1,1,1)
        CALL DSTRB(TEMP3,VAPINC7,1,1,1)
        CALL DSTRB(TEMP4,CLDINC7,1,1,1)
C----------------------------------------------------------------------
C***
C***  IF FORECAST IS NOT BEGINNING AT TIME 0 
C***  THEN WE MUST READ ADDITIONAL INFORMATION
C***
        IF(NINT(TSTART).NE.0)THEN
C
          IF(MYPE.EQ.0)THEN
            READ(NFCST)TEMP1,TEMP2,TEMP3
     1,                ACUTIM,ARATIM,APHTIM
          ENDIF
          CALL DSTRB(TEMP1,POTFLX,1,1,1)
          CALL DSTRB(TEMP2,TLMIN,1,1,1)
          CALL DSTRB(TEMP3,TLMAX,1,1,1)
          CALL MPI_BCAST(ACUTIM,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
          CALL MPI_BCAST(ARATIM,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
          CALL MPI_BCAST(APHTIM,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
C
          DO L=1,LM
            IF(MYPE.EQ.0)THEN
              READ(NFCST)TEMP1         ! RSWTT
              READ(NFCST)TEMP2         ! RLWTT
            ENDIF
C
            CALL DSTRB(TEMP1,RSWTT,1,LM,L)
            CALL DSTRB(TEMP2,RLWTT,1,LM,L)
          ENDDO
C
          DO L=1,LM
            IF(MYPE.EQ.0)THEN
              READ(NFCST)TEMP1         ! T0
              READ(NFCST)TEMP2         ! Q0
            ENDIF
C
            CALL DSTRB(TEMP1,T0,1,LM,L)
            CALL DSTRB(TEMP2,Q0,1,LM,L)
          ENDDO
C
          IF(MYPE.EQ.0)THEN
            READ(NFCST)TEMP1           ! P0
            READ(NFCST)TEMP2           ! CNVBOT (used to be HBOT)
            READ(NFCST)TEMP3           ! CNVTOP (used to be HTOP)
            READ(NFCST)TEMP4           ! RSWTOA
            READ(NFCST)TEMP5           ! RLWTOA
          ENDIF
C
          CALL DSTRB(TEMP1,P0,1,1,1)
          CALL DSTRB(TEMP2,CNVBOT,1,1,1)
          CALL DSTRB(TEMP3,CNVTOP,1,1,1)
          CALL DSTRB(TEMP4,RSWTOA,1,1,1)
          CALL DSTRB(TEMP5,RLWTOA,1,1,1)
        ENDIF
C-------------------------------------------------------------------
C
c       IF(MYPE.EQ.0)WRITE(LIST,*)'  READ  ',LABEL
C
C-------------------------------------------------------------------
C***  CALL RADIATION TO OBTAIN THE SHORT AND LONGWAVE
C***  TEMPERATURE TENDENCIES
C***
C
c       CALL RADTN
C
      ENDIF
C     
C     DONE READING INITIAL CONDITIONS OR A RESTART FILE.
C     
C
C     END OF SUBROUTINE READ_RESTRT
C     
      IF(MYPE.EQ.0)THEN
        WRITE(LIST,*)'INIT:  EXIT READ_RESTRT'
        WRITE(LIST,*)' '
      ENDIF
C
      RETURN
      END
