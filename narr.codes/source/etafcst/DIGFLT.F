C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                        SUBROUTINE DIGFLT
C-----------------------------------------------------------------
      INCLUDE "EXCHM.h"
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (PIQ=3.141592654,LP1=LM+1,JAM=6+2*(JM-10))
C-----------------------------------------------------------------------
C     NTIM IS A HALF-SPAN IN TIME STEP UNITS
C     ACTUAL TIME IS NTIM*DT
C
                             P A R A M E T E R
     & (NTIM=30)
C
                             P A R A M E T E R
     1 (CM1=2937.4,CM2=4.9283,CM3=23.5518,EPS=0.622)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,NEST
C-----------------------------------------------------------------------
                             R E A L
     & PDQ(IDIM1:IDIM2,JDIM1:JDIM2)
     &,TQ(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,UQ(IDIM1:IDIM2,JDIM1:JDIM2,LM),VQ(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,RELHUM(IDIM1:IDIM2,JDIM1:JDIM2),PDSL0(IDIM1:IDIM2,JDIM1:JDIM2)
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
      DATA KNT/0/,IUNRH/51/,IUNDF/52/
C-----------------------------------------------------------------------
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C-----------------------------------------------------------------------
      NBOCO=0
      REWIND IUNRH
      REWIND IUNDF
C***
C***  COMPUTE AND WRITE OUT THE RELATIVE HUMIDITY 
C***
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        PDSL0(I,J)=RES(I,J)*PD(I,J)
      ENDDO
      ENDDO
C
      DO L=1,LM
C
        CALL ZERO2(RELHUM)
C
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          IF(HTM(I,J,L).GT.0.5)THEN
            CLOGES=-CM1/T(I,J,L)-CM2*ALOG10(T(I,J,L))+CM3
            ESE=10.**(CLOGES+2.)
            PRS=AETA(L)*PDSL0(I,J)+PT
            QIJ=Q(I,J,L)
            E=PRS*QIJ/(EPS*(1.-QIJ)+QIJ)
            RELHUM(I,J)=AMIN1(E/ESE,0.98)
          ELSE
            RELHUM(I,J)=0.
          ENDIF
        ENDDO
        ENDDO
C
C***  SMOOTH THE INITIAL RH FIELD THEN SAVE IT
C
ccccccc CALL FILT25(RELHUM,HTM(IDIM1,JDIM1,L),5,l,0)
C
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          RELHUM(I,J)=AMIN1(RELHUM(I,J),0.98)
          RELHUM(I,J)=AMAX1(RELHUM(I,J),0.02)
        ENDDO
        ENDDO
C***
C***  ASSEMBLE GLOBAL RELHUM
C***
        CALL LOC2GLB(RELHUM,TEMP1)
C
        IF(MYPE.EQ.0)WRITE(IUNRH)TEMP1
C
C***  SMOOTH THE INITIAL TEMPERATURE FIELD BEFORE EXECUTING
C***  THE DIGITAL FILTER
C
ccccccc CALL FILT25(T(IDIM1,JDIM1,L),HTM(IDIM1,JDIM1,L),5,l,1)
C
      ENDDO
C
C***  SAVE CURRENT PROG VARIABLES IN WORK FILE FOR LATER USE
C
      CALL LOC2GLB(PD,TEMP1)
      IF(MYPE.EQ.0)WRITE(IUNDF)TEMP1
C
      DO L=1,LM
        CALL LOC2GLB(T(IDIM1,JDIM1,L),TEMP1)
        IF(MYPE.EQ.0)WRITE(IUNDF)TEMP1
C
        CALL LOC2GLB(U(IDIM1,JDIM1,L),TEMP1)
        IF(MYPE.EQ.0)WRITE(IUNDF)TEMP1
C
        CALL LOC2GLB(V(IDIM1,JDIM1,L),TEMP1)
        IF(MYPE.EQ.0)WRITE(IUNDF)TEMP1
      ENDDO
C
C***  SET UP ARRAYS TO HOLD SUMS FOR DIGITAL FILTER
C
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        PDQ(I,J)=PD(I,J)
      ENDDO
      ENDDO
C
      DO L=1,LM
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          TQ(I,J,L)=T(I,J,L)
          UQ(I,J,L)=U(I,J,L)
          VQ(I,J,L)=V(I,J,L)
        ENDDO
        ENDDO
      ENDDO
C
      HSPAN=FLOAT(NTIM)*DT/3600.
C
      IF(MYPE.EQ.0)WRITE(6,100) HSPAN
 100  FORMAT(' ','INITIALIZATION CALLED WITH HALF-SPAN',F5.1,' HOURS')
C
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C-----------------------------------------------------------------------
C
C      RUN THE FORECAST MODEL FORWARD AND BACKWARD (DIGITAL FILTER)
C
C*****    FIRST, FORWARD INTEGRATION *************
C
C-----------------------------------------------------------------------
C      CALCULATE NORM NEEDED FOR LANCZOS WINDOW
C
C      'TETC' DEFINES CUT-OFF FREQUENCY FOR THE WINDOW
C   (FACTOR 2 APPEARS ONLY TO SHOW GENERAL FORMULA)
C   (IT SHOULD BE TETC=PIQ/FLOAT(NTIM))
C
C      'NTIM' IS A NUMBER OF TIME STEPS IN A HALF-SPAN
C
C      'TIME' CORRESPONDS TO NUMBER OF TIME STEPS OF A SPAN
C
      TIME=2.*FLOAT(NTIM)
      QNT=FLOAT(NTIM+1)
      TETC=2.*PIQ/TIME
      CSTT=TETC/PIQ
      SQ=CSTT
C
      DO NT=1,NTIM
        FNT=FLOAT(NT)
        AS1=PIQ*FNT/QNT
        AS2=FNT*TETC
        ASS1=SIN(AS1)/AS1
        ASS2=SIN(AS2)/AS2
        SQ=SQ+2.*CSTT*ASS1*ASS2
      ENDDO
C
C----------------------------------------------------------------
C
C***  NORMALIZATION OF THE WINDOW FUNCTION
C
      CSTT=CSTT/SQ
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        PDQ(I,J)=PDQ(I,J)*CSTT
      ENDDO
      ENDDO
C
      DO L=1,LM
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          TQ(I,J,L)=TQ(I,J,L)*CSTT        
          UQ(I,J,L)=UQ(I,J,L)*CSTT        
          VQ(I,J,L)=VQ(I,J,L)*CSTT        
        ENDDO
        ENDDO
      ENDDO
C
C----------------------------------------------------------------
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
        CALL VTADVF
C
        CALL EXCH(T,LM,U,LM,V,LM,Q2,LM,1,1)
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
C
        CALL EXCH(U,LM,V,LM,2,2)         !Exchange U and V
      ENDIF
C-----------------------------------------------------------------------
C
C***  CALCULATE WINDOW PARAMETER
C
      BNT=FLOAT(NTSD)
      BS1=BNT*PIQ/QNT
      BS2=BNT*TETC
      BSS1= SIN(BS1)/BS1
      BSS2= SIN(BS2)/BS2
      HNTSD=CSTT*BSS1*BSS2
C+++     HNTSD=HNTSD*1.042
C
      DO L=1,LM
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          TQ(I,J,L)=TQ(I,J,L)+HNTSD*T(I,J,L)
          UQ(I,J,L)=UQ(I,J,L)+HNTSD*U(I,J,L)
          VQ(I,J,L)=VQ(I,J,L)+HNTSD*V(I,J,L)
        ENDDO
        ENDDO
      ENDDO
C
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        PDQ(I,J)=PDQ(I,J)+HNTSD*PD(I,J)
      ENDDO
      ENDDO
C
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
C      NOW BACKWARD INTEGRATION, STARTING FROM THE INITIAL TIME
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
      CALL EXCH(WPDAR,1,CPGFU,1,2,2)
      CALL EXCH(CURV,1,FCP,1,2,2)
      CALL EXCH(FAD,1,F,1,2,2)
C
C------------------- END OF CHANGE -------------------------
C
C      DEFINE INITIAL DATA FOR BACKWARD INTEGRATION
C      (PDQ,TQ,UQ,VQ ARE SUMS NEEDED FOR DIGITAL FILTER)
C
      IF(MYPE.EQ.0)THEN
        REWIND IUNDF
        READ(IUNDF)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,PD,1,1,1)
C
      DO L=1,LM
        IF(MYPE.EQ.0)THEN
          READ(IUNDF)TEMP1
          READ(IUNDF)TEMP2
          READ(IUNDF)TEMP3
        ENDIF
C
        CALL DSTRB(TEMP1,T,1,LM,L)
        CALL DSTRB(TEMP2,U,1,LM,L)
        CALL DSTRB(TEMP3,V,1,LM,L)
      ENDDO
C
      CALL EXCH(T,LM,U,LM,V,LM,5,5)    !Exchange T, U, and V
      CALL EXCH(PD,1,5,5)
C--------------------------------------------------------------------
      NTSD=0
      FIRST=.TRUE.
      TSPH=-3600./DT
      IF(MYPE.EQ.0)THEN
        IF(.NOT.NEST)THEN
          REWIND NBC
          READ(NBC)
          READ(NBC)BCHR
        ELSE
          READ(NBC,REC=2)BCHR
        ENDIF
      ENDIF
C
      CALL MPI_BCAST(BCHR,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
      NBOCO=INT(BCHR*TSPH+0.5)
C
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
        CALL VTADVF
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
C
        CALL EXCH(T,LM,U,LM,V,LM,4,4)         !Exchange T, U, and V
        CALL EXCH(Q2,LM,5,5)
C
        CALL HZADV
C
        CALL EXCH(U,LM,V,LM,2,2)         !Exchange U and V
      ENDIF
C-----------------------------------------------------------------------
C
C***  CALCULATE WINDOW PARAMETER
C
      BNT=FLOAT(NTSD)
      BS1=BNT*PIQ/QNT
      BS2=BNT*TETC
      BSS1= SIN(BS1)/BS1
      BSS2= SIN(BS2)/BS2
      HNTSD=CSTT*BSS1*BSS2
C+++     HNTSD=HNTSD*1.042
C
      DO L=1,LM
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          TQ(I,J,L)=TQ(I,J,L)+HNTSD*T(I,J,L)
          UQ(I,J,L)=UQ(I,J,L)+HNTSD*U(I,J,L)
          VQ(I,J,L)=VQ(I,J,L)+HNTSD*V(I,J,L)
        ENDDO
        ENDDO
      ENDDO
C
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        PDQ(I,J)=PDQ(I,J)+HNTSD*PD(I,J)
      ENDDO
      ENDDO
C
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
C
      CALL EXCH(WPDAR,1,CPGFU,1,2,2)
      CALL EXCH(CURV,1,FCP,1,2,2)
      CALL EXCH(FAD,1,F,1,2,2)
C
C***  UPDATE INITIALIZED PROGNOSTIC VARIALBES
C
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        PD(I,J)=PDQ(I,J)
      ENDDO
      ENDDO
C
      DO L=1,LM
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          T(I,J,L)=TQ(I,J,L)
          U(I,J,L)=UQ(I,J,L)
          V(I,J,L)=VQ(I,J,L)
        ENDDO
        ENDDO
      ENDDO
C***
C***  RETRIEVE THE INITIAL RELATIVE HUMIDITY AND COMPUTE Q
C***  SO AS TO MAINTAIN THE RH GIVEN THE NEW TEMPERATURES
C***
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        PDSL0(I,J)=RES(I,J)*PD(I,J)
      ENDDO
      ENDDO
C
      IF(MYPE.EQ.0)REWIND IUNRH
C
      DO L=1,LM
        IF(MYPE.EQ.0)READ(IUNRH)TEMP1
        CALL DSTRB(TEMP1,RELHUM,1,1,1)
C
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          IF(HTM(I,J,L).GT.0.5)THEN
            CLOGES=-CM1/T(I,J,L)-CM2*ALOG10(T(I,J,L))+CM3
            ESE=10.**(CLOGES+2.)
            PRS=AETA(L)*PDSL0(I,J)+PT
            E=RELHUM(I,J)*ESE
            Q(I,J,L)=EPS*E/(PRS+E*(EPS-1.))
          ENDIF
        ENDDO
        ENDDO
      ENDDO
C
      CALL EXCH(T,LM,Q,LM,U,LM,V,LM,4,4)  !Exchange T, Q, U, and V
      CALL EXCH(PD,1,5,5)
C
C  RETURN BC FILE TO START FORECAST
C
      NTSD=0
      IF(MYPE.EQ.0)THEN
        IF(.NOT.NEST)THEN
          REWIND NBC
          READ(NBC)
          READ(NBC)BCHR
        ELSE
          READ(NBC,REC=2)BCHR
        ENDIF
      ENDIF
C
      CALL MPI_BCAST(BCHR,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
      NBOCO=INT(BCHR*TSPH+0.5)
C-----------------------------------------------------------
C------------------- END OF CHANGE -------------------------
C-----------------------------------------------------------
      RETURN
      END
