C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE GOSSIP
C*************************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    GOSSIP      EXCHANGE OF FIELDS BETWEEN PROCESSORS
C   PRGRMMR: BLACK           ORG: W/NP2      DATE: 97-08-30
C
C ABSTRACT:
C     GOSSIP EXCHANGES MANY FIELDS BETWEEN PROCESSORS
C     IN ORDER TO FILL THE HALOES
C
C PROGRAM HISTORY LOG:
C   97-05-??  MEYS       - ORIGINATOR
C   98-10-23  BLACK      - MODIFIED FOR CURRENT VERSION OF MODEL 
C
C USAGE: CALL GOSSIP FROM MAIN PROGRAM EBU
C   INPUT ARGUMENT LIST:
C       NONE
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT FILES:
C     NONE
C
C   SUBPROGRAMS CALLED:
C
C     UNIQUE: EXCH
C
C     LIBRARY: NONE
C
C   COMMON BLOCKS: MASKS  
C                  LOOPS
C                  CONTIN 
C                  DYNAM
C                  VRBLS
C                  PVRBLS
C                  PHYS2  
C                  CLDWTR
C                  CNVCLD
C                  ACMCLH
C                  ACMCLD
C                  ACMPRE
C                  ACMRDL
C                  ACMRDS
C                  ACMSFC
C                  SOIL
C                  INDX
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C$$$
C***********************************************************************
      INCLUDE "PARMETA.comm"
      INCLUDE "PARMTBL.comm"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
         include "my_comm.h"
!#include "sp.h"
C-----------------------------------------------------------------------
!                          P A R A M E T E R
!    & (LP1=LM+1,JAM=6+2*(JM-10))
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "CONTIN.comm"
      INCLUDE "DYNAM.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "PHYS2.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "ACMCLH.comm"
      INCLUDE "ACMCLD.comm"
      INCLUDE "ACMPRE.comm"
      INCLUDE "ACMRDL.comm"
      INCLUDE "ACMRDS.comm"
      INCLUDE "ACMSFC.comm"
      INCLUDE "Z0EFFT.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "INDX.comm"
      INCLUDE "MASK1.comm"
C***********************************************************************
C
C***  THE NHB ARRAYS
C
      CALL IEXCH(LMH,1,5,5)
      CALL IEXCH(LMV,1,5,5)
      CALL EXCH(HBM1,1,5,5)
      CALL EXCH(VBM1,1,5,5)
      CALL EXCH(HBM2,1,5,5)
      CALL EXCH(HBM3,1,5,5)
      CALL EXCH(VBM2,1,5,5)
      CALL EXCH(VBM3,1,5,5)
      CALL EXCH(SM,1,5,5)
      CALL EXCH(SICE,1,5,5)
      CALL EXCH(HTM,LM,5,5)
      CALL EXCH(VTM,LM,5,5)
      CALL EXCH(DX,1,5,5)
      CALL EXCH(WPDAR,1,5,5)
      CALL EXCH(CPGFU,1,5,5)
      CALL EXCH(CURV,1,5,5)
      CALL EXCH(FCP,1,5,5)
      CALL EXCH(FDIV,1,5,5)
      CALL EXCH(FAD,1,5,5)
      CALL EXCH(F,1,5,5)
      CALL EXCH(DDMPU,1,5,5)
      CALL EXCH(DDMPV,1,5,5)
      CALL EXCH(GLAT,1,5,5)
      CALL EXCH(GLON,1,5,5)
      CALL EXCH(MXSNAL,1,5,5)
      CALL EXCH(EPSR,1,5,5)
      CALL EXCH(TG,1,5,5)
      CALL EXCH(GFFC,1,5,5)
      CALL EXCH(SST,1,5,5)
      CALL EXCH(ALBASE,1,5,5)
      CALL EXCH(HDAC,1,5,5)
      CALL EXCH(HDACV,1,5,5)
      CALL IEXCH(IVGTYP,1,5,5)
      CALL IEXCH(ISLTYP,1,5,5)
      CALL IEXCH(ISLOPE,1,5,5)
      CALL EXCH(VEGFRC,1,5,5)
C
C***  THE RESTRT FILE ARRAYS
C
      CALL EXCH (OMGALF,LM,5,5)
      CALL EXCH (PD,1,5,5)
      CALL EXCH (RES,1,5,5)
      CALL EXCH (FIS,1,5,5)
      CALL EXCH (T,LM,5,5)
      CALL EXCH (U,LM,5,5)
      CALL EXCH (V,LM,5,5)
      CALL EXCH (Q,LM,5,5)
      CALL EXCH (Q2,LM,5,5)
      CALL EXCH (CWM,LM,5,5)
      CALL EXCH (TRAIN,LM,5,5)
      CALL EXCH (TCUCN,LM,5,5)
      CALL EXCH (RSWIN,1,5,5)
      CALL EXCH (RSWOUT,1,5,5)
      CALL EXCH (TG,1,5,5)
      CALL EXCH (Z0,1,5,5)
      CALL EXCH (AKMS,1,5,5)
      CALL EXCH (CZEN,1,5,5)
      CALL EXCH (AKHS,1,5,5)
      CALL EXCH (THS,1,5,5)
      CALL EXCH (QS,1,5,5)
      CALL EXCH (TWBS,1,5,5)
      CALL EXCH (QWBS,1,5,5)
      CALL EXCH (HBOT,1,5,5)
      CALL EXCH (CFRACL,1,5,5)
      CALL EXCH (THZ0,1,5,5)
      CALL EXCH (QZ0,1,5,5)
      CALL EXCH (UZ0,1,5,5)
      CALL EXCH (VZ0,1,5,5)
      CALL EXCH (USTAR,1,5,5)
      CALL EXCH (HTOP,1,5,5)
      CALL EXCH (CFRACM,1,5,5)
      CALL EXCH (SNO,1,5,5)
      CALL EXCH (SI,1,5,5)
      CALL EXCH (CLDEFI,1,5,5)
      CALL EXCH (RF,1,5,5)
      CALL EXCH (CUPPT,1,5,5)
      CALL EXCH (CFRACH,1,5,5)
      CALL EXCH (SOILTB,1,5,5)
      CALL EXCH (SFCEXC,1,5,5)
      CALL EXCH (SMSTAV,1,5,5)
      CALL EXCH (SMSTOT,1,5,5)
      CALL EXCH (GRNFLX,1,5,5)
      CALL EXCH (PCTSNO,1,5,5)
      CALL EXCH (RLWIN,1,5,5)
      CALL EXCH (RADOT,1,5,5)
      CALL EXCH (CZMEAN,1,5,5)
      CALL EXCH (SIGT4,1,5,5)
      CALL EXCH (U00,1,5,5)
      CALL IEXCH (LC,1,5,5)
      CALL EXCH (SR,1,5,5)
      CALL EXCH (PREC,1,5,5)
      CALL EXCH (ACPREC,1,5,5)
      CALL EXCH (ACCLIQ,1,5,5)
      CALL EXCH (CUPREC,1,5,5)
      CALL EXCH (ACFRCV,1,5,5)
      CALL IEXCH (NCFRCV,1,5,5)
      CALL EXCH (ACFRST,1,5,5)
      CALL IEXCH (NCFRST,1,5,5)
      CALL EXCH (ACSNOW,1,5,5)
      CALL EXCH (ACSNOM,1,5,5)
      CALL EXCH (SSROFF,1,5,5)
      CALL EXCH (BGROFF,1,5,5)
      CALL EXCH (SFCSHX,1,5,5)
      CALL EXCH (SFCLHX,1,5,5)
      CALL EXCH (SUBSHX,1,5,5)
      CALL EXCH (SNOPCX,1,5,5)
      CALL EXCH (SFCUVX,1,5,5)
      CALL EXCH (SFCEVP,1,5,5)
      CALL EXCH (POTEVP,1,5,5)
      CALL EXCH (ASWIN,1,5,5)
      CALL EXCH (ASWOUT,1,5,5)
      CALL EXCH (ASWTOA,1,5,5)
      CALL EXCH (ALWIN,1,5,5)
      CALL EXCH (ALWOUT,1,5,5)
      CALL EXCH (ALWTOA,1,5,5)
      CALL EXCH (SMC,NSOIL,5,5)
      CALL EXCH (CMC,1,5,5)
      CALL EXCH (STC,NSOIL,5,5)
      CALL EXCH (SH2O,NSOIL,5,5)
      CALL EXCH (ALBEDO,1,5,5)
C
      DO J=MYJS_P4,MYJE_P4
        IVW(J)=IVWG(J+MY_JS_GLB-1)
        IVE(J)=IVEG(J+MY_JS_GLB-1)
        IHE(J)=IHEG(J+MY_JS_GLB-1)
        IHW(J)=IHWG(J+MY_JS_GLB-1)
      ENDDO
C
      CALL EXCH (ZEFFIJ,4,5,5)
C--------------------------------------------------------------
      RETURN
      END
