C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                        SUBROUTINE NEWFLT
C-----------------------------------------------------------------
      INCLUDE "EXCHM.h"
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (PIQ=3.141592654,LP1=LM+1,JAM=6+2*(JM-10),LB=2*IM+JM-3)
C-----------------------------------------------------------------------
C     NTIM IS A SPAN IN TIME STEP UNITS FOR THE BACKWARD INTEGRATION
C
                             P A R A M E T E R
     & (NTIM=270)
C
                             P A R A M E T E R
     1 (CM1=2937.4,CM2=4.9283,CM3=23.5518,EPS=0.622)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "BOCO.comm"
C-----------------------------------------------------------------------
C
      DATA KNT/0/,IUNRH/51/,IUNDF/52/
C-----------------------------------------------------------------------
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C-----------------------------------------------------------------------
      NBOCO=0
C***
C***  SMOOTH THE INITIAL TEMPERATURE FIELD BEFORE EXECUTING
C***  THE FILTER
C
c     CALL FILT25(T(IDIM1,JDIM1,L),HTM(IDIM1,JDIM1,L),5)
C
C
      SPAN=FLOAT(NTIM)*DT/3600.
C
      IF(MYPE.EQ.0)WRITE(6,100)SPAN
 100  FORMAT(' ','INITIALIZATION CALLED WITH SPAN',F5.1,' HOURS')
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C***  RUN THE MODEL BACKWARD THEN FORWARD
C
C-----------------------------------------------------------------------
C
C      ADIABATIC BACKWARD INTEGRATION, STARTING FROM THE INITIAL TIME
C
C-----------------------------------------------------------------------
C
C***  CHANGE (SIGN ONLY OF) IMPORTANT TIME CONSTANTS
C
      DT   =-DT
      CPGFV=-CPGFV
      EN   =-EN
      ENT  =-ENT
      F4D  =-F4D
      F4Q  =-F4Q
      EF4T =-EF4T
C
      DO JK=1,JAM
        EM (JK)=-EM (JK)
        EMT(JK)=-EMT(JK)
      ENDDO
C
      DO L=1,LM
        F4Q2(L)=-F4Q2(L)
      ENDDO
C
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        WPDAR(I,J)=-WPDAR(I,J)
        CPGFU(I,J)=-CPGFU(I,J)
        CURV (I,J)=-CURV (I,J)
        FCP  (I,J)=-FCP  (I,J)
        FAD  (I,J)=-FAD  (I,J)
        F    (I,J)=-F    (I,J)
      ENDDO
      ENDDO
C
C--------------------------------------------------------------------
      NTSD=0
      FIRST=.TRUE.
      TSPH=-3600./DT
C-----------------------------------------------------------------------
C********ENTRY INTO THE TIME LOOP***************************************
 2020 NTSD=NTSD+1
      KNT=KNT+1
      IF(MYPE.EQ.0)WRITE(6,2015)NTSD,NTIM
C***********************************************************************
C
C***  DIVERGENCE AND HORIZONTAL PART OF THE OMEGA-ALPHA TERM
C
      IF(NTSD.GT.1)CALL EXCH(T,LM,U,LM,V,LM,2,2)    !Exchange T, U, and  V
C
      CALL DIVHOA
      call exit('divhoa')
C-----------------------------------------------------------------------
C
C***  PRESS. TEND.,ETA DOT & VERTICAL OMEGA-ALPHA
C
      CALL PDTE
      call exit('pdte')
C-----------------------------------------------------------------------
C
C***  VERTICAL ADVECTION
C
      IF(MOD(NTSD-1,IDTAD).EQ.0)THEN
        CALL EXCH(ETADT,LM-1,1,1)
C
        CALL VTADVF
      call exit('vtadvf')
C
        CALL EXCH(T,LM,U,LM,V,LM,Q2,LM,1,1)          !Exchange T, U, and V
      ENDIF
C-----------------------------------------------------------------------
C
C***  UPDATE SURFACE PRESSURE (MINUS PTOP)
C
      CALL PDNEW
C-----------------------------------------------------------------------
C
C***  UPDATE H BOUNDARY POINTS
C
      IF(MOD(NTSD,IDTAD).EQ.0)THEN
        CALL EXCH(T,LM,Q2,LM,1,1)
      ENDIF
      CALL EXCH(PD,1,1,1)
C
      CALL BOCOHF
C-----------------------------------------------------------------------
C
C***  PRESSURE GRADIENT AND CORIOLIS FORCE TERMS
C
      CALL EXCH(PD,1,T,LM,2,2)            !Exchange PD and T
C
      CALL PGCOR
      call exit('pgcor')
C
      CALL EXCH(PDSL,1,5,5)
C-----------------------------------------------------------------------
C
C***  UPDATE V BOUNDARY POINTS
C
      CALL EXCH(U,LM,V,LM,1,1)           !Exchange U and V
C
      CALL BOCOV
C-----------------------------------------------------------------------
C
C***  HORIZONTAL ADVECTION
C
      IF(MOD(NTSD,IDTAD).EQ.0)THEN
        CALL EXCH(T,LM,U,LM,V,LM,4,4)         !Exchange T, U, and V
        CALL EXCH(Q2,LM,5,5)
C
        CALL HZADV
      call exit('hzadv')
C
        CALL EXCH(U,LM,V,LM,2,2)         !Exchange U and V
      ENDIF
C-----------------------------------------------------------------------
      IF(NTSD.EQ.NTIM) GO TO 2022
C
C***********************************************************************
      GO TO 2020
C********EXIT FROM THE TIME LOOP****************************************
C
 2022 CONTINUE
C
C-----------------------------------------------------------------------
C***  READY FOR FORWARD INTEGRATION
C-----------------------------------------------------------------------
      NTSD=0
      FIRST=.TRUE.
      TSPH=-3600./DT
C
      IF(MYPE.EQ.0)THEN
        REWIND NBC
        READ(NBC)
        READ(NBC)BCHR
        READ(NBC)PDB
        READ(NBC)TB
        READ(NBC)QB
        READ(NBC)UB
        READ(NBC)VB
        READ(NBC)Q2B
        READ(NBC)CWMB
      ENDIF
C
      CALL MPI_BCAST(BCHR,1,MPI_REAL,0,
     1               MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(PDB(1,1),2*LB,MPI_REAL,0,
     1               MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(TB(1,1,1),2*LB*LM,MPI_REAL,0,
     1               MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(QB(1,1,1),2*LB*LM,MPI_REAL,0,
     1               MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(UB(1,1,1),2*LB*LM,MPI_REAL,0,
     1               MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(VB(1,1,1),2*LB*LM,MPI_REAL,0,
     1               MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(Q2B(1,1,1),2*LB*LM,MPI_REAL,0,
     1               MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(CWMB(1,1,1),2*LB*LM,MPI_REAL,0,
     1               MPI_COMM_COMP,IRTN)
C
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C
      IF(MYPE.EQ.0)WRITE(LIST,*)'  READ UNIT NBC=',NBC
C***
C***  COMPUTE THE 1ST TIME FOR BOUNDARY CONDITION READ
C***
C
      NBOCO=INT(BCHR*TSPH+0.5)
      if(mype.eq.0)write(6,*)' nboco=',nboco
C
      IF(NTSD.EQ.NBOCO)THEN
        IF(MYPE.EQ.0)THEN
          BACKSPACE NBC
          BACKSPACE NBC
          BACKSPACE NBC
          BACKSPACE NBC
          BACKSPACE NBC
          BACKSPACE NBC
          BACKSPACE NBC
          WRITE(LIST,*)'  BACKSPACE UNIT NBC=',NBC
        ENDIF
      ENDIF
C
C***  CHANGE BACK (SIGN ONLY) IMPORTANT TIME CONSTANTS
C
      DT   =-DT
      CPGFV=-CPGFV
      EN   =-EN
      ENT  =-ENT
      F4D  =-F4D
      F4Q  =-F4Q
      EF4T =-EF4T
C
      DO JK=1,JAM
        EM (JK)=-EM (JK)
        EMT(JK)=-EMT(JK)
      ENDDO
C
      DO L=1,LM
        F4Q2(L)=-F4Q2(L)
      ENDDO
C
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        WPDAR(I,J)=-WPDAR(I,J)
        CPGFU(I,J)=-CPGFU(I,J)
        CURV (I,J)=-CURV (I,J)
        FCP  (I,J)=-FCP  (I,J)
        FAD  (I,J)=-FAD  (I,J)
        F    (I,J)=-F    (I,J)
      ENDDO
      ENDDO
C--------------------------------------------------------------------
C***  INTEGRATE FORWARD WITH FULL PHYSICS 
C--------------------------------------------------------------------
C
      NTSD=0
C
C-----------------------------------------------------------------------
C********ENTRY INTO THE TIME LOOP***************************************
 2010 NTSD=NTSD+1
      KNT=KNT+1
      IF(MYPE.EQ.0)WRITE(6,2015)NTSD,NTIM
 2015 FORMAT(' NTSD=',I5,'  NTSTM=',I4)
C-----------------------------------------------------------------------
C------------------------RADIATION--------------------------------------
C-----------------------------------------------------------------------
      IF(MOD(NTSD-1,NRADS).EQ.0.OR.MOD(NTSD-1,NRADL).EQ.0)THEN
        CALL RADTN
      ENDIF
C***********************************************************************
C
C***  DIVERGENCE AND HORIZONTAL PART OF THE OMEGA-ALPHA TERM
C
      IF(NTSD.GT.1)CALL EXCH(T,LM,U,LM,V,LM,2,2)    !Exchange T, U, and V
C
      CALL DIVHOA
C-----------------------------------------------------------------------
C
C***  PRESS. TEND.,ETA DOT & VERTICAL OMEGA-ALPHA
C
      CALL PDTE
C-----------------------------------------------------------------------
C
C***  VERTICAL ADVECTION
C
      IF(MOD(NTSD-1,IDTAD).EQ.0)THEN
        CALL EXCH(ETADT,LM-1,1,1)
C
        CALL VTADV
C
        CALL EXCH(T,LM,U,LM,V,LM,Q2,LM,1,1)          !Exchange T, U, and V
      ENDIF
C-----------------------------------------------------------------------
C
C***  UPDATE SURFACE PRESSURE (MINUS PTOP)
C
      CALL PDNEW
C-----------------------------------------------------------------------
C
C***  UPDATE H BOUNDARY POINTS
C
      IF(MOD(NTSD,IDTAD).EQ.0)THEN
        CALL EXCH(T,LM,Q,LM,Q2,LM,1,1)
      ENDIF
      CALL EXCH(PD,1,1,1)
      call mpi_barrier(MPI_COMM_COMP,istat)
      CALL EXCH(CWM,LM,1,1)
C
      CALL BOCOH
C-----------------------------------------------------------------------
C
C***  PRESSURE GRADIENT AND CORIOLIS FORCE TERMS
C
      CALL EXCH(PD,1,T,LM,Q,LM,2,2)            !Exchange PD and T
C
      CALL PGCOR
C
      CALL EXCH(PDSL,1,5,5)
C-----------------------------------------------------------------------
C--------DIVERGENCE DAMPING---------------------------------------------
C-----------------------------------------------------------------------
C
      IF(MOD(NTSD,NTDDMP).EQ.0)THEN
        CALL EXCH(T,LM,U,LM,V,LM,DIV,LM,1,1)           !Exchange T, U, and V
C
        CALL DDAMP
      ENDIF
C-----------------------------------------------------------------------
C
C***  UPDATE V BOUNDARY POINTS
C
      CALL EXCH(U,LM,V,LM,1,1)           !Exchange U and V
C
      CALL BOCOV
C-----------------------------------------------------------------------
C--------APPLY TEMPERATURE TENDENCY DUE TO RADIATION--------------------
C-----------------------------------------------------------------------
C
      CALL RDTEMP
C
C-----------------------------------------------------------------------
C--------LATERAL DIFFUSION----------------------------------------------
C-----------------------------------------------------------------------
C
      CALL EXCH(T,LM,U,LM,V,LM,Q,LM,2,2)           !Exchange T, U, V, and Q
      CALL EXCH(Q2,LM,1,1)
C
      CALL HDIFF
C
C-----------------------------------------------------------------------
C
C***  HORIZONTAL ADVECTION
C
      IF(MOD(NTSD,IDTAD).EQ.0)THEN
        CALL EXCH(T,LM,U,LM,V,LM,4,4)         !Exchange T, U, and V
        CALL EXCH(Q2,LM,5,5)
C
        CALL HZADV
C
        CALL EXCH(U,LM,V,LM,CWM,LM,2,2)         !Exchange U and V
C
        CALL HZADV2
      ENDIF
C-----------------------------------------------------------------------
C--------TURBULENT PROCESSES AND PRECIPITATION--------------------------
C-----------------------------------------------------------------------
      IF(MOD(NTSD-NPHS/2,NPHS).EQ.0)THEN
        CALL EXCH(PD,1,UZ0,1,VZ0,1,T,LM,U,LM,V,LM,Q,LM,1,1)       !Exchange PD, T, U, V, and Q
C
        CALL TURBL                     !Contains calls to EXCH
      ENDIF
C-----------------------------------------------------------------------
C--------CONDENSATION/EVAPORATION OF CLOUD WATER------------------------
C-----------------------------------------------------------------------
      IF(MOD(NTSD-NPHS/2,NPHS).EQ.0)THEN
        CALL GSCOND
      ENDIF
C-----------------------------------------------------------------------
C--------CONVECTIVE PRECIPITATION---------------------------------------
C-----------------------------------------------------------------------
      IF(MOD(NTSD-NCNVC/2,NCNVC).EQ.0)THEN
        CALL CUCNVC
      ENDIF
C-----------------------------------------------------------------------
C--------GRIDSCALE PRECIPITATION----------------------------------------
C-----------------------------------------------------------------------
      IF(MOD(NTSD-NPHS/2,NPHS).EQ.0)THEN
        CALL PRECPD
      ENDIF
C-----------------------------------------------------------------------
C
      IF(NTSD.EQ.NTIM) GO TO 2013
C
C***********************************************************************
      GO TO 2010
C********EXIT FROM THE TIME LOOP****************************************
C
 2013 CONTINUE
C
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C-----------------------------------------------------------------------
C
C  RETURN BC FILE TO START FORECAST
C
      NTSD=0
      IF(MYPE.EQ.0)THEN
        REWIND NBC
C
        READ(NBC)
        READ(NBC)BCHR
      ENDIF
C
      CALL MPI_BCAST(BCHR,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
      NBOCO=INT(BCHR*TSPH+0.5)
C------------------- END OF CHANGE -------------------------
C-----------------------------------------------------------
      RETURN
      END
