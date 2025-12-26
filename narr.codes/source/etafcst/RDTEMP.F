C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE RDTEMP
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    RDTEMP      RADIATIVE TEMPERATURE CHANGE
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 93-12-29
C     
C ABSTRACT:
C     RDTEMP APPLIES THE TEMPERATURE TENDENCIES DUE TO
C     RADIATION AT ALL LAYERS AT EACH ADJUSTMENT TIME STEP
C     
C PROGRAM HISTORY LOG:
C   87-09-??  BLACK      - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   95-11-20  ABELES     - PARALLEL OPTIMIZATION
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL RDTEMP FROM MAIN PROGRAM EBU
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
C  
C     UNIQUE: NONE
C  
C     LIBRARY: NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  MASKS
C                  VRBLS
C                  PVRBLS
C                  PHYS
C                  ACMRDS
C                  ACMRDL
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (LP1=LM+1)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMRDS.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMRDL.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     &  FACTR(IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
C
C  GET CURRENT VALUE OF COS(ZENITH ANGLE)
C
      TIMES=(NTSD-1)*DT
      CALL ZENITH(TIMES,DAYI,HOUR)
C
!$omp parallel do 
      DO 50 J=MYJS,MYJE
      DO 50 I=MYIS,MYIE
      IF(CZMEAN(I,J).GT.0.)THEN 
        FACTR(I,J)=CZEN(I,J)/CZMEAN(I,J)
      ELSE
        FACTR(I,J)=0.
      ENDIF
  50  CONTINUE
C
!$omp parallel do private(ttndkl)
      DO 100 L=1,LM
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
c       if(mype.eq.13.and.i.eq.40.and.j.eq.77) then
c        print*,'l=',l
c        print*,'rswtt,fact,rlwtt=',rswtt(i,j,l),factr(i,j),rlwtt(i,j,l)
c       endif
        TTNDKL=RSWTT(I,J,L)*FACTR(I,J)+RLWTT(I,J,L)
        T(I,J,L)=T(I,J,L)+TTNDKL*DT*HTM(I,J,L)*HBM2(I,J)
c       if(mype.eq.13.and.i.eq.40.and.j.eq.77) then
c        print*,'l=',l
c        print*,'ttndkl=',ttndkl
c        print*,'dt=',dt
c        print*,'htm(i,j,l)=',htm(i,j,l)
c        print*,'hbm2(i,j)=',hbm2(i,j)
c       endif
      ENDDO
      ENDDO
  100 CONTINUE
C-----------------------------------------------------------------------
                              RETURN
                              END
