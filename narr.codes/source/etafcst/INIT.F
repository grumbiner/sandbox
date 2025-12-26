      SUBROUTINE INIT
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    INIT        INITIALIZE VARIABLE FOR MODEL RUN
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: ??-??-??
C     
C ABSTRACT:  INIT READS IN PRIMARY AND AUXILIARY VARIABLES AND CONSTANTS
C            AND SETS INITIAL VALUES FOR OTHERS
C     
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC  -
C   92-10-27  DEAVEN  - CHANGED READS OF NHB, NFC, AND NBC TO
C                       ACCOMODATE SHORTENED RECORD LENGTHS
C   95-03-27  BLACK   - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-10-31  BLACK   - ADDED NAMELIST BCEXDATA FOR THE NESTS
C   98-06-10  ROGERS  - MADE Y2K COMPLIANT BY REPLACING CALL TO W3FI13
C                       TO W3DOXDAT
C   98-09-04  PYLE    - CHANGED TO NOT RE-INITIALIZE TSHLTR AND QSHLTR IF
C                       RESTART=TRUE
C   98-10-21  BLACK   - CHANGES FOR DISTRIBUTED MEMORY
C   98-11-17  BLACK   - ADDED CODE TO LOCATE THE INNER DOMAIN BOUNDARIES
C                       ON THE RELEVANT PE's
C
C     
C USAGE:    CALL INIT FROM MAIN PROGRAM EBU
C
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   INPUT FILES:
C     NFC - THE INITIAL VALUES OF SFC PRESSURE, T, Q, U, AND V
C     NHB - A LARGE VARIETY OF ARRAY AND SCALAR CONSTANTS
C     NBC - THE BOUNDARY CONDITIONS AND TENDENCIES
C
C                              OR
C
C     RESTRT - A RESTART FILE WITH ALL NECESSARY QUANTITIES
C
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UNIQUE: READ_NHB
C             READ_RESTRT
C             ZERO2
C             ZERO3
C     UTILITIES: W3LIB - W3DOXDAT
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK
C                  LOOPS
C                  MASKS
C                  DYNAM
C                  PHYS2
C                  MAPOT1
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
C                  CUINIT
C                  SOIL
C                  INDX
C                  TEMPV 
C                  RD1TIM
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
      INCLUDE "cuparm"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
#include "sp.h"
C-----------------------------------------------------------------------
                              P A R A M E T E R
     & (CM1=2937.4,CM2=4.9283,CM3=23.5518,EPS=0.622,PI2=2.*3.14159265
     &, RLAG=14.8125
C
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
C    &, Q2INI=.01,EPSQ2=1.E-4,EPSQ=2.E-12,EPSWET=1.E-4
C    &, Q2INI=1.0,EPSQ2=1.E-4,EPSQ=2.E-12,EPSWET=1.E-4
C    &, Q2INI=.50,EPSQ2=1.E-4,EPSQ=2.E-12,EPSWET=1.E-4
C    &, Q2INI=.01,EPSQ2=1.E-4,EPSQ=2.E-12,EPSWET=0.0
     &, Q2INI=.50  ,EPSQ2=0.2  ,EPSWET=0.0
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
     &, Z0LAND=.10,Z0SEA=.001,FCM=.00001
     &, DTR=0.1745329E-1)
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (A1=610.78,WA=.10,WG=1.0-WA)
C
C-----------------------------------------------------------------------
                              P A R A M E T E R
     & (IMJM=IM*JM-JM/2,JMP1=JM+1,JAM=6+2*(JM-10),LB=2*IM+JM-3
     &, LM1=LM-1,LP1=LM+1,IMT=2*IM-1
     &, NSTAT=1000)
C-----------------------------------------------------------------------
C     
C                            DECLARE VARIABLES
C     
C-----------------------------------------------------------------------
                              L O G I C A L
     & RUN,RUNB,FIRST,RESTRT,SIGMA,EXBC,NEST
     &,INSIDEH,INSIDEV
C-----------------------------------------------------------------------
                              C H A R A C T E R *32
     & LABEL
                              C H A R A C T E R *40
     & CONTRL,FILALL,FILMST,FILTMP,FILTKE,FILUNV
     &,FILCLD,FILRAD,FILSFC
C-----------------------------------------------------------------------
                              R E A L
     & PHALF(LP1),NPEBND
C***
C***  NOTE: THE DIMENSION OF THE FOLLOWING ARRAYS IS ARBITRARILY CHOSEN
C***        TO EXCEED ANY NUMBER OF BOUNDARY POINTS THAT MIGHT EXIST IN
C***        ANY INNER DOMAIN
C***
                              R E A L
     & HLATI(1500),HLONI(1500),VLATI(1500),VLONI(1500)
     &,THLONI(1500),THLATI(1500),TVLONI(1500),TVLATI(1500)
     &,TSLAT(NSTAT),TSLON(NSTAT)
C-----------------------------------------------------------------------
                              I N T E G E R
     & IDATB(3),INIDAT(8),IBCDAT(8)
C-----------------------------------------------------------------------
#ifdef DP_REAL
      LOGICAL*8 RUNBX
      INTEGER*8 IDATBX(3),IHRSTBX
#endif
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
      INCLUDE "SOIL.comm"
      INCLUDE "INDX.comm"
      INCLUDE "Z0EFFT.comm"
      INCLUDE "TEMPV.comm"
      INCLUDE "PPTASM.comm"
      INCLUDE "QFLX.comm"
C-----------------------------------------------------------------------
C***
C***  THE FOLLOWING IS FOR TIMIMG PURPOSES ONLY
C***
      real*8 timef
      real nhb_tim
      common/timing/surfce_tim,nhb_tim,res_tim,exch_tim
C-----------------------------------------------------------------------
                             C O M M O N /RD1TIM/
     1 K400,CTHK(3),LTOP(3),PTOPC(4),TAUCV(3),RAD1
     2,LVL(IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
                             D A T A
     1 PLOMD/64200./,PMDHI/35000./,PHITP/15000./,P400/40000./
     2,PLBTM/105000./
                             D A T A
     1 NFILE/14/,IUNWGT/40/
!-----------------------------------------------------------------------
!
!--- Flag for initializing convective clouds for radiation
!
      COMMON /CUINIT/ CURAD
      LOGICAL CURAD
C-----------------------------------------------------------------------
C     
C     DECLARE NAMELISTS.
C
      NAMELIST /FCSTDATA/
     & TSTART,TEND,TCP,RESTRT,SINGLRST,SUBPOST,NMAP,TSHDE,SPL
     &,NPHS,NCNVC,NRADSH,NRADLH,NTDDMP
     &,TPREC,THEAT,TCLOD,TRDSW,TRDLW,TSRFC
     &,NEST
      CHARACTER ENVAR*4
C
C   Read in precip assim test points (global), compute the corresponding 
C   local coordinate
C   and the node it is on
      
c     CALL GETENV("tmmark",ENVAR)
c     itest=56
c     jtest=33
c     IF(ENVAR.NE.'tm00') then
c       READ(5,*) ITEST, JTEST
c       print*,'itest,jtest,mype=',itest,jtest,mype
c       CALL GLB2LOC(ITEST,JTEST,ITSTLOC,JTSTLOC,MTSTPE)
c     ENDIF
      itstloc=39
      jtstloc=26
      mtstpe=5
C
C***********************************************************************
C     START INIT HERE.
C     
C     CALCULATE THE I-INDEX EAST-WEST INCREMENTS
C
      DO J=1,JM
        IHEG(J)=MOD(J+1,2)
        IHWG(J)=IHEG(J)-1
        IVEG(J)=MOD(J,2)
        IVWG(J)=IVEG(J)-1
      ENDDO
C     
C     CALCULATE THE INDIRECT I INDICES FOR RADTN
C
      KNT=0
      DO I=1,IM
        KNT=KNT+1
        IRADG(KNT)=I
      ENDDO
      DO I=1,IM-1
        KNT=KNT+1
        IRADG(KNT)=IM+2+I
      ENDDO
C
C     ZERO OUT LOCALLY INDEXED ARRAYS
C
      CALL ZERO2(PDSL)
      CALL ZERO3(T,LM)
      CALL ZERO3(Q,LM)
      CALL ZERO3(U,LM)
      CALL ZERO3(V,LM)
      CALL ZERO2(RES)
      CALL ZERO3(RTOP,LM)
      CALL ZERO3(OMGALF,LM)
      CALL ZERO3(DIV,LM)
      CALL ZERO3(ETADT,LM-1)
      CALL ZERO3(HTM,LM)
      CALL ZERO3(VTM,LM)
      CALL ZERO2(HBM2)
      CALL ZERO2(AKMS)
      CALL ZERO2(UZ0)
      CALL ZERO2(VZ0)
      CALL ZERO2(FAD)
C---------------------------------------------------------------
C
C     READ Z0 EFFECTIVE
C
      DO N=1,4
        IF(MYPE.EQ.0)THEN
          READ(22)TEMP1
        ENDIF
        CALL DSTRB(TEMP1,ZEFFIJ,1,4,N)
      ENDDO
C---------------------------------------------------------------
C***
C***  READ "CONSTANT" DATA FROM UNIT CONNECTED TO NHB
C***
      NHB=12
      LSL  =LSM
      btim=timef()
      CALL READ_NHB(NHB)
      nhb_tim=timef()-btim
C
C---------------------------------------------------------------
      NHIBU = 12
      IF(MYPE.EQ.0)WRITE(LIST,*)'INIT:  READ CONSTANTS FILE'
C
C
C     READ NAMELIST FCSTDATA WHICH CONTROLS TIMESTEPS, 
C     ACCUMULATION PERIODS, STANDARD OUTPUT
C
      RESTRT = .FALSE.
      REWIND 11
      READ(11,FCSTDATA)
C     
      IF(MYPE.EQ.0)THEN
        WRITE(LIST,*)'INIT:  READ NAMELIST FCSTDATA - LISTED BELOW'
        WRITE(LIST,*)'  TSTART,TEND  :  ',TSTART,TEND
        WRITE(LIST,*)'  TCP          :  ',TCP
        WRITE(LIST,*)'  RESTRT       :  ',RESTRT
        WRITE(LIST,*)'  SINGLRST     :  ',SINGLRST
        WRITE(LIST,*)'  SUBPOST      :  ',SUBPOST
        WRITE(LIST,*)'  NMAP,NPHS    :  ',NMAP,NPHS
        WRITE(LIST,*)'  NCNVC        :  ',NCNVC
        WRITE(LIST,*)'  NRADSH,NRADLH:  ',NRADSH,NRADLH
        WRITE(LIST,*)'  NTDDMP       :  ',NTDDMP
        WRITE(LIST,*)'  TPREC,THEAT  :  ',TPREC,THEAT
        WRITE(LIST,*)'  TCLOD,TRDSW  :  ',TCLOD,TRDSW
        WRITE(LIST,*)'  TRDLW,TSRFC  :  ',TRDLW,TSRFC
        WRITE(LIST,*)'  TSHDE (POSTED FORECAST HOURS) BELOW:  '
        WRITE(LIST,75) (TSHDE(K),K=1,99)
        WRITE(LIST,*)'  SPL (POSTED PRESSURE LEVELS) BELOW: '
        WRITE(LIST,80) (SPL(L),L=1,LSM)
   75   FORMAT(14(F4.1,1X))
   80   FORMAT(8(F8.1,1X))
      ENDIF
C
C     
C     SET TIME STEPPING RELATED CONSTANTS.
C
      FIRST  = .TRUE.
      NSTART = INT(TSTART*TSPH+0.5)
      NTSTM  = INT(TEND  *TSPH+0.5)+1
      NCP    = INT(TCP   *TSPH+0.5)
      NPREC  = INT(TPREC *TSPH+0.5)
      NHEAT  = INT(THEAT *TSPH+0.5)
      NCLOD  = INT(TCLOD *TSPH+0.5)
      NRDSW  = INT(TRDSW *TSPH+0.5)
      NRDLW  = INT(TRDLW *TSPH+0.5)
      NSRFC  = INT(TSRFC *TSPH+0.5)
      IF(MYPE.EQ.0)THEN
        WRITE(0,*)' NTSTM=',NTSTM,' TSPH=',TSPH,' DT=',DT
      ENDIF
C     IF (NSTART.LT.NCP)      NSTART=0
C     
C     SET VARIOUS PHYSICS PACKAGE TIMESTEP VARIABLES.
C
      NRADS = NINT(TSPH)*NRADSH
      NRADL = NINT(TSPH)*NRADLH
      DTQ2  = NPHS * DT
      TDTQ2 = DTQ2 + DTQ2
      DTD   = 0.5  * DTQ2
      TDTD  = DTD  + DTD
      KTM   = INT(DTQ2/DTD+0.5)
C     
      IF(MYPE.EQ.0)THEN
        WRITE(LIST,*)' '
        WRITE(LIST,*)'SET TIME STEPPING CONSTANTS'
        WRITE(LIST,*)' FIRST             :  ',FIRST
        WRITE(LIST,*)' NSTART,NSTSM,NCP  :  ',NSTART,NTSTM,NCP
        WRITE(LIST,*)' NTDDMP,NPREC,NHEAT:  ',NTDDMP,NPREC,NHEAT
        WRITE(LIST,*)' NCLOD,NRDSW,NRDLW :  ',NCLOD,NRDSW,NRDLW
        WRITE(LIST,*)' NSRFC             :  ',NSRFC
        WRITE(LIST,*)' NRADS,NRADL,KTM   :  ',NRADS,NRADL,KTM
        WRITE(LIST,*)' DTQ2,TDTQ2        :  ',DTQ2,TDTQ2
        WRITE(LIST,*)' DTD,TDTD          :  ',DTD,TDTD
        WRITE(LIST,*)' '
      ENDIF
C
C     COMPUTE DERIVED MAP OUTPUT CONSTANTS.
      DO L = 1,LSL
         ALSL(L) = LOG(SPL(L))
      ENDDO
      DO I=1,NMAP
         ISHDE(I)=INT(TSHDE(I)*TSPH+0.5)+1
      ENDDO
C***
C***  SET UP ARRAY IRAD (INDICES FOR RADTN)
C***
      DO I=MYIS,MYIE
        IRAD(I)=IRADG(I+MY_IS_GLB-1)-MY_IS_GLB+1
      ENDDO
C-------------------------------------------------------------
C***
C***  READ INITIAL CONDITIONS OR RESTART FILE.
C***
      btim=timef()
      IF(SINGLRST)THEN
        CALL READ_RESTRT
      ELSE
        CALL READ_RESTRT2
      ENDIF
      res_tim=timef()-btim
C-------------------------------------------------------------
C
      CALL READGRDETA
C
C-------------------------------------------------------------
C
C     IF NOT RUNNING THE MODEL, PRINT DATE OF INITIAL CONDITIONS
C     JUST READ AND STOP.  OTHERWISE, CONTINUE.
C
C-------------------------------------------------------------
      IF (RUN) GO TO 190
C
      IF(MYPE.EQ.0)THEN 
        WRITE(LIST,165) IHRST,IDAT
        WRITE(LIST,166)
ccccc   CALL EXIT(2)
        CALL MPI_FINALIZE(IERR)
        STOP2
  165   FORMAT('0*** ',I2,' GMT ',2(I2,'/'),I4,' ***')
  166   FORMAT('0F*** NO INITIAL CONDITIONS. RUN TERMINATED.')
      ENDIF
C     
C     IF THE TIMESTEP COUNTER (NTSD) EXCEEDS THE "STOP MODEL" T
C     TIMESTEP,CONTINUE, STOP EXECUTION.  OTHERWISE, CONTINUE.
C
  190 IF(NTSD.GE.NTSTM)THEN
        IF(MYPE.EQ.0)THEN
          WRITE(LIST,165) IHRST,IDAT
          WRITE(LIST,195)
  195     FORMAT('0F*** FORECAST ALREADY DONE. RUN TERMINATED.')
ccccc     CALL EXIT(3)
          CALL MPI_FINALIZE(IERR)
          STOP3
        ENDIF
      ENDIF
C     
C-------------------------------------------------------------
C
C     READ BOUNDARY CONDITIONS.
C     
C-------------------------------------------------------------
      IF(MYPE.EQ.0)THEN
        IF(NEST)THEN
          KBI=2*IM+JM-3
          KBI2=KBI-4
          LRECBC=4*(1+(1+6*LM)*KBI*2+(KBI+KBI2)*(LM+1))
          OPEN(UNIT=NBC,ACCESS='DIRECT',RECL=LRECBC)
        ENDIF
C
        IF(.NOT.NEST)REWIND NBC
C
#ifdef DP_REAL
        IF(NEST)THEN
          READ(NBC,REC=1)RUNBX,IDATBX,IHRSTBX,TBOCO
        ELSE
          READ(NBC)RUNBX,IDATBX,IHRSTBX,TBOCO
        ENDIF
C
        RUNB=RUNBX
        IDATB=IDATBX
        IHRSTB=IHRSTBX
#else
        IF(NEST)THEN
          READ(NBC,REC=1)RUNB,IDATB,IHRSTB,TBOCO
        ELSE
          READ(NBC)RUNB,IDATB,IHRSTB,TBOCO
        ENDIF
#endif
      ENDIF
C
      CALL MPI_BCAST(RUNB,1,MPI_LOGICAL,0,MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(IDATB,3,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(IHRSTB,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(TBOCO,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
C
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C
      IF(MYPE.EQ.0.AND..NOT.NEST)THEN
        ISTART=NINT(TSTART)
C
        READ(NBC)BCHR
  205   READ(NBC)
        READ(NBC)
        READ(NBC)
        READ(NBC)
        READ(NBC)
        READ(NBC)
        READ(NBC)
C
        IF(ISTART.EQ.NINT(BCHR))THEN
          IF(ISTART.GT.0)READ(NBC)BCHR
          GO TO 215
        ELSE
          READ(NBC)BCHR
        ENDIF
C
        IF(ISTART.GE.NINT(BCHR))GO TO 205
      ENDIF
C
      IF(MYPE.EQ.0.AND.NEST)THEN
        ISTART=NINT(TSTART)
        NREC=1
C
  210   NREC=NREC+1
        READ(NBC,REC=NREC)BCHR
C
        IF(ISTART.EQ.NINT(BCHR))THEN
          IF(ISTART.GT.0)READ(NBC,REC=NREC+1)BCHR
          GO TO 215
        ELSE
          GO TO 210
        ENDIF
      ENDIF
C
  215 CONTINUE
C
      CALL MPI_BCAST(BCHR,1,MPI_REAL,0,
     1               MPI_COMM_COMP,IRTN)
C
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C
      IF(MYPE.EQ.0)WRITE(LIST,*)'  READ UNIT NBC=',NBC
C***
C***  COMPUTE THE 1ST TIME FOR BOUNDARY CONDITION READ
C***
      NBOCO=NINT(BCHR*TSPH)
C
      IF(NTSD.EQ.0)THEN
        IF(MYPE.EQ.0.AND..NOT.NEST)THEN
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
C-------------------------------------------------------------
C     
C     SET ARRAYS CONTROLLING POST PROCESSING.
C     
C-------------------------------------------------------------
      IF(MYPE.EQ.0)THEN
        WRITE(LIST,*)'INIT:  READ IOUT,NSHDE,NTSD=',IOUT,NSHDE,NTSD
      ENDIF
C
      DO I=1,NMAP
         IOUT=I
         IF(ISHDE(I).GE.NTSD)GO TO 220
      ENDDO
 220  NSHDE = ISHDE(IOUT)
C
      IF(MYPE.EQ.0)THEN
        WRITE(LIST,*)'INIT:  SET IOUT,NSHDE =',IOUT,NSHDE,
     1               ' FOR ISHDE,NTSD=',ISHDE(IOUT),NTSD
      ENDIF
C-------------------------------------------------------------
C     
C     INITIALIZE PHYSICS VARIABLES IF STARTING THIS RUN FROM SCRATCH.
C
      IF(NEST)THEN
        DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
        DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
C
          LLMH=LMH(I,J)
C
          IF(T(I,J,LLMH).EQ.0.)THEN
            T(I,J,LLMH)=T(I,J,LLMH-1)
          ENDIF
C
          TERM1=-0.068283/T(I,J,LLMH)
          PSHLTR(I,J)=(PD(I,J)+PT)*EXP(TERM1)
        ENDDO
        ENDDO
      ENDIF
C
      IF(.NOT.RESTRT)THEN
        DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
        DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
          LLMH=LMH(I,J)
          PDSL(I,J)   = PD(I,J)*RES(I,J)
          PREC(I,J)   = 0.
          ACPREC(I,J) = 0.
          CUPREC(I,J) = 0.
          VAPINC(I,J)=0.
          VAPINC7(I,J)=0.
          CLDINC(I,J)=0.
          CLDINC7(I,J)=0.
          Z0(I,J)     = SM(I,J)*Z0SEA+(1.-SM(I,J))*
     1                (FIS(I,J)*FCM+Z0LAND)
          QS(I,J)     = 0.
          AKMS(I,J)   = 0.
          AKHS(I,J)   = 0.
          TWBS(I,J)   = 0.
          QWBS(I,J)   = 0.
          CLDEFI(I,J) = 1.
          HTOP(I,J)   = 100.
          HBOT(I,J)   = 0.
C***
C***  AT THIS POINT, WE MUST CALCULATE THE INITIAL POTENTIAL TEMPERATURE
C***  OF THE SURFACE AND OF THE SUBGROUND.
C***  EXTRAPOLATE DOWN FOR INITIAL SURFACE POTENTIAL TEMPERATURE.
C***  ALSO DO THE SHELTER PRESSURE.
C***
          PM1=PDSL(I,J)*AETA(LLMH)+PT
          APEM1=(1.E5/PM1)**CAPA
          THS(I,J)=T(I,J,LLMH)*(1.+0.608*Q(I,J,LLMH))*APEM1
          TSFCK=T(I,J,LLMH)*(1.+0.608*Q(I,J,LLMH))
          PSFCK=PD(I,J)+PT
C
          IF(SM(I,J).LT.0.5) THEN
            QS(I,J)=PQ0/PSFCK*EXP(A2*(TSFCK-A3)/(TSFCK-A4))
          ELSEIF(SM(I,J).GT.0.5) THEN
            THS(I,J)=SST(I,J)*(1.E5/(PD(I,J)+PT))**CAPA
          ENDIF
C
          TERM1=-0.068283/T(I,J,LLMH)
          PSHLTR(I,J)=(PD(I,J)+PT)*EXP(TERM1)
C 
          USTAR(I,J)=0.1
          THZ0(I,J)=THS(I,J)
          QZ0(I,J)=QS(I,J)
          UZ0(I,J)=0.
          VZ0(I,J)=0.
C
        ENDDO
        ENDDO
C
C     INITIALIZE CLOUD FIELDS
C
        DO L=1,LM
          DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
          DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
            CWM(I,J,L)=0.
          ENDDO
          ENDDO
        ENDDO
C
C     INITIALIZE ACCUMULATOR ARRAYS TO ZERO.
C
        ARDSW=0.0
        ARDLW=0.0
        ASRFC=0.0
        AVRAIN=0.0
        AVCNVC=0.0
C
        DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
        DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
          ACFRCV(I,J)=0.
          NCFRCV(I,J)=0
          ACFRST(I,J)=0.
          NCFRST(I,J)=0
          ACSNOW(I,J)=0.
          ACSNOM(I,J)=0.
          SSROFF(I,J)=0.
          BGROFF(I,J)=0.
          ALWIN(I,J) =0.
          ALWOUT(I,J)=0.
          ALWTOA(I,J)=0.
          ASWIN(I,J) =0.
          ASWOUT(I,J)=0.
          ASWTOA(I,J)=0.
          SFCSHX(I,J)=0.
          SFCLHX(I,J)=0.
          SUBSHX(I,J)=0.
          SNOPCX(I,J)=0.
          SFCUVX(I,J)=0.
          SFCEVP(I,J)=0.
          POTEVP(I,J)=0.
          POTFLX(I,J)=0.
        ENDDO
        ENDDO
C
C     INITIALIZE SATURATION SPECIFIC HUMIDITY OVER THE WATER.
C
        DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
        DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
          IF(SM(I,J).GT.0.5)THEN
            CLOGES =-CM1/SST(I,J)-CM2*ALOG10(SST(I,J))+CM3
            ESE    = 10.**(CLOGES+2.)
            QS(I,J)= SM(I,J)*EPS*ESE/(PD(I,J)+PT-ESE*(1.-EPS))
          ENDIF
        ENDDO
        ENDDO
C     
C       PAD GROUND WETNESS IF IT IS TOO SMALL.
C
c        DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
c        DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
c          WET(I,J)=AMAX1(WET(I,J),EPSWET)
c        ENDDO
c        ENDDO
C     
C        INITIALIZE TURBULENT KINETIC ENERGY (TKE) TO A SMALL
C        VALUE (EPSQ2) ABOVE GROUND.  SET TKE TO ZERO IN THE
C        THE LOWEST MODEL LAYER.  IN THE LOWEST TWO ATMOSPHERIC
C        ETA LAYERS SET TKE TO A SMALL VALUE (Q2INI).
C
        DO L=1,LM1
          DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
          DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
            Q2(I,J,L)=HTM(I,J,L+1)*HBM2(I,J)*EPSQ2
          ENDDO
          ENDDO
        ENDDO
C
        DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
        DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
          Q2(I,J,LM)    = 0.
          LLMH          = LMH(I,J)
          Q2(I,J,LLMH-2)= HBM2(I,J)*Q2INI
          Q2(I,J,LLMH-1)= HBM2(I,J)*Q2INI
        ENDDO
        ENDDO
C     
C     PAD ABOVE GROUND SPECIFIC HUMIDITY IF IT IS TOO SMALL.
C     INITIALIZE LATENT HEATING ACCUMULATION ARRAYS.
C
        DO L=1,LM
          DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
          DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
            IF(Q(I,J,L).LT.EPSQ)Q(I,J,L)=EPSQ*HTM(I,J,L)
            TRAIN(I,J,L)=0.
            TCUCN(I,J,L)=0.
          ENDDO
          ENDDO
        ENDDO
C     
C     END OF SCRATCH START INITIALIZATION BLOCK.
C
        IF(MYPE.EQ.0)THEN
          WRITE(LIST,*)'INIT:  INITIALIZED ARRAYS FOR CLEAN START'
        ENDIF
      ENDIF
C
C
C
C     RESTART INITIALIZING.  CHECK TO SEE IF WE NEED TO ZERO
C     ACCUMULATION ARRAYS.
C
      IF(RESTRT)THEN
C
C       AVERAGE CLOUD AMOUNT ARRAY
C
        IF(MOD(NTSD,NCLOD).LT.NPHS)THEN
          IF(MYPE.EQ.0)WRITE(LIST,*)'  ZERO AVG CLD AMT ARRAY'
          DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
          DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
            ACFRCV(I,J)=0.
            NCFRCV(I,J)=0
            ACFRST(I,J)=0.
            NCFRST(I,J)=0
          ENDDO
          ENDDO
        ENDIF
C     
C        GRID-SCALE AND CONVECTIVE LATENT HEATING ARRAYS.
C     
        IF(MOD(NTSD,NHEAT).LT.NCNVC)THEN
          IF(MYPE.EQ.0)THEN
            WRITE(LIST,*)'  ZERO ACCUM LATENT HEATING ARRAYS'
          ENDIF
C
          AVRAIN=0.
          AVCNVC=0.
          DO L=1,LM
            DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
            DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
              TRAIN(I,J,L)=0.
              TCUCN(I,J,L)=0.
            ENDDO
            ENDDO
          ENDDO
        ENDIF
C***
C***  IF THIS IS NOT A NESTED RUN, INITIALIZE TKE
C***
c       IF(.NOT.NEST)THEN
c         DO L=1,LM
c           DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
c           DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
c             Q2(I,J,L)=AMAX1(Q2(I,J,L)*HBM2(I,J),EPSQ2)
c           ENDDO
c           ENDDO
c         ENDDO
c       ENDIF
C     
C     TOTAL AND CONVECTIVE PRECIPITATION ARRAYS.
C     TOTAL SNOW AND SNOW MELT ARRAYS.
C     STORM SURFACE AND BASE GROUND RUN OFF ARRAYS.
C     
        IF(MOD(NTSD,NPREC).LT.NPHS)THEN
          IF(MYPE.EQ.0)WRITE(LIST,*)'  ZERO ACCUM PRECIP ARRAYS'
          DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
          DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
            ACPREC(I,J)=0.
            CUPREC(I,J)=0.
            VAPINC(I,J)=0.
            VAPINC7(I,J)=0.
            CLDINC(I,J)=0.
            CLDINC7(I,J)=0.
            ACSNOW(I,J)=0.
            ACSNOM(I,J)=0.
            SSROFF(I,J)=0.
            BGROFF(I,J)=0.
C
            FQNEV(I,J)=0.             !dule
            FQSEV(I,J)=0.             !dule
            FCNEV(I,J)=0.             !dule
            FCSEV(I,J)=0.             !dule
            FCNEV7(I,J)=0.            !dule
            FCSEV7(I,J)=0.            !dule
            FQNEV7(I,J)=0.            !dule
            FQSEV7(I,J)=0.            !dule
            FQU(I,J)=0.               !dule
            FQV(I,J)=0.               !dule
            FCU(I,J)=0.               !dule
            FCV(I,J)=0.               !dule
            FQU7(I,J)=0.              !dule
            FQV7(I,J)=0.              !dule
            FCU7(I,J)=0.              !dule
            FCV7(I,J)=0.              !dule
            DQADV(I,J)=0.             !dule
            DQFLX(I,J)=0.             !dule
            DCFLX(I,J)=0.             !dule
            DQFLX7(I,J)=0.            !dule
            DCFLX7(I,J)=0.            !dule
            DO L=1,LM                 !dule
              QOLD(I,J,L)=Q(I,J,L)    !dule
            END DO                    !dule
C
          ENDDO
          ENDDO
        ENDIF
C     
C     LONG WAVE RADIATION ARRAYS.
C     
        IF(MOD(NTSD,NRDLW).LT.NPHS)THEN
          IF(MYPE.EQ.0)WRITE(LIST,*)'  ZERO ACCUM LW RADTN ARRAYS'
          ARDLW=0.
          DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
          DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
            ALWIN(I,J) =0.
            ALWOUT(I,J)=0.
            ALWTOA(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
C     
C     SHORT WAVE RADIATION ARRAYS.
C     
        IF(MOD(NTSD,NRDSW).LT.NPHS)THEN
          IF(MYPE.EQ.0)WRITE(LIST,*)'  ZERO ACCUM SW RADTN ARRAYS'
          ARDSW=0.
          DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
          DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
            ASWIN(I,J) =0.
            ASWOUT(I,J)=0.
            ASWTOA(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
C     
C     SURFACE SENSIBLE AND LATENT HEAT FLUX ARRAYS.
C     
        IF(MOD(NTSD,NSRFC).LT.NPHS)THEN
          IF(MYPE.EQ.0)WRITE(LIST,*)'  ZERO ACCUM SFC FLUX ARRAYS'
          ASRFC=0.
          DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
          DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
            SFCSHX(I,J)=0.
            SFCLHX(I,J)=0.
            SUBSHX(I,J)=0.
            SNOPCX(I,J)=0.
            SFCUVX(I,J)=0.
            SFCEVP(I,J)=0.
            POTEVP(I,J)=0.
            POTFLX(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
C
C     ENDIF FOR RESTART FILE ACCUMULATION ZERO BLOCK.
C
        IF(MYPE.EQ.0)THEN
          WRITE(LIST,*)'INIT:  INITIALIZED ARRAYS FOR RESTART START'
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     INITIALIZE CLOUD CONSTANTS
C
C-----------------------------------------------------------------------
      DO 350 J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
      DO 350 I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
      U00(I,J)=(1.-SM(I,J))*0.75+SM(I,J)*0.80
  350 CONTINUE
!
!--- Flag for initializing convective cloud arrays for radiation
!
      CURAD=.FALSE.
C
      DO 355 L=1,2*LM
      IF(L.GE.LM-10.AND.L.LE.LM)THEN
        UL(L)=0.1*FLOAT(L-LM+10)
      ELSE
        UL(L)=0.
      ENDIF
  355 CONTINUE
C
C----------------- INITIALIZE T0, Q0 & P0 FOR GSCOND -------------------
C
      IF(NSTART.EQ.0)THEN
        DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
        DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
          P0(I,J)=PD(I,J)
        ENDDO
        ENDDO
C
        DO L=1,LM
          DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
          DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
            T0(I,J,L)=T(I,J,L)
            Q0(I,J,L)=Q(I,J,L)
          ENDDO
          ENDDO
        ENDDO
      ENDIF
C***
C***  SET INDEX ARRAYS FOR UPSTREAM ADVECTION
C***
      KNT=0
      DO J=3,5
        KNT=KNT+1
        IHLA(KNT)=2
        IHHA(KNT)=IM-1-MOD(J+1,2)
        IVLA(KNT)=2 
        IVHA(KNT)=IM-1-MOD(J,2)
        JRA(KNT)=J
      ENDDO
      DO J=JM-4,JM-2
        KNT=KNT+1
        IHLA(KNT)=2
        IHHA(KNT)=IM-1-MOD(J+1,2)
        IVLA(KNT)=2 
        IVHA(KNT)=IM-1-MOD(J,2)
        JRA(KNT)=J
      ENDDO
      DO J=6,JM-5
        KNT=KNT+1
        IHLA(KNT)=2
        IHHA(KNT)=2+MOD(J,2)
        IVLA(KNT)=2 
        IVHA(KNT)=2+MOD(J+1,2)
        JRA(KNT)=J
      ENDDO
      DO J=6,JM-5
        KNT=KNT+1
        IHLA(KNT)=IM-2
        IHHA(KNT)=IM-2+MOD(J,2)
        IVLA(KNT)=IM-2
        IVHA(KNT)=IM-2+MOD(J+1,2)
        JRA(KNT)=J
      ENDDO
C
C*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS
C
      IF(NSTART.EQ.0)THEN
C
        DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
        DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
          PCTSNO(I,J)=-999.0
          IF(SM(I,J).LT.0.5)THEN
            IF(SICE(I,J).GT.0.5)THEN
C
C***  SEA-ICE CASE
C
              SMSTAV(I,J)=1.0
              SMSTOT(I,J)=1.0
              SSROFF(I,J)=0.0
              BGROFF(I,J)=0.0
              CMC(I,J)=0.0
              DO NS=1,NSOIL
                SMC(I,J,NS)=1.0
                SH2O(I,J,NS)=1.0
              ENDDO
            ENDIF
          ELSE
C
C***  WATER CASE
C
            SMSTAV(I,J)=1.0
            SMSTOT(I,J)=1.0
            SSROFF(I,J)=0.0
            BGROFF(I,J)=0.0
            SOILTB(I,J)=280.99
            GRNFLX(I,J)=0.
            SUBSHX(I,J)=0.0
            ACSNOW(I,J)=0.0
            ACSNOM(I,J)=0.0
            SNOPCX(I,J)=0.0
            CMC(I,J)=0.0
            SNO(I,J)=0.0
            DO NS=1,NSOIL
              SMC(I,J,NS)=1.0
              SH2O(I,J,NS)=1.0
              STC(I,J,NS)=273.16
            ENDDO
          ENDIF
C
        ENDDO
        ENDDO
C
        APHTIM=0.0
        ARATIM=0.0
        ACUTIM=0.0
C
      ENDIF
C
C-------------------------------------------------------------------
C     INITIALIZE RADTN VARIABLES
C     CALCULATE THE NUMBER OF STEPS AT EACH POINT.
C     THE ARRAY 'LVL' WILL COORDINATE VERTICAL LOCATIONS BETWEEN
C     THE LIFTED WORKING ARRAYS AND THE FUNDAMENTAL MODEL ARRAYS.
C     LVL HOLDS THE HEIGHT (IN MODEL LAYERS) OF THE TOPOGRAPHY AT
C     EACH GRID POINT.
C-------------------------------------------------------------------
C   
      DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
      DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
        LVL(I,J)=LM-LMH(I,J)
      ENDDO
      ENDDO
C   
C     DETERMINE MODEL LAYER LIMITS FOR HIGH(3), MIDDLE(2),
C     AND LOW(1) CLOUDS.  ALSO FIND MODEL LAYER THAT IS JUST BELOW
C     (HEIGHT-WISE) 400 MB. (K400)
C   
      K400=0
      PSUM=PT
      SLPM=101325.
      PDIF=SLPM-PT
      DO L=1,LM
        PSUM=PSUM+DETA(L)*PDIF
        IF(LTOP(3).EQ.0)THEN
          IF(PSUM.GT.PHITP)LTOP(3)=L
        ELSEIF(LTOP(2).EQ.0)THEN
          IF(PSUM.GT.PMDHI)LTOP(2)=L
        ELSEIF(K400.EQ.0)THEN
          IF(PSUM.GT.P400)K400=L
        ELSEIF(LTOP(1).EQ.0)THEN
          IF(PSUM.GT.PLOMD)LTOP(1)=L
        ENDIF
      ENDDO
C   
C    CALL GRADFS ONCE TO CALC. CONSTANTS AND GET O3 DATA
C   
      KCCO2=0
C   
C    CALCULATE THE MIDLAYER PRESSURES IN THE STANDARD ATMOSPHERE
C   
      PSS=101325.
      PDIF=PSS-PT
C
      DO L=1,LM1
        PHALF(L+1)=AETA(L)*PDIF+PT
      ENDDO
C
      PHALF(1)=0.
      PHALF(LP1)=PSS
C   
      CALL GRADFS(PHALF,KCCO2,NFILE)
C   
C    CALL SOLARD TO CALCULATE NON-DIMENSIONAL SUN-EARTH DISTANCE
C   
      IF(MYPE.EQ.0)CALL SOLARD(RAD1)
      CALL MPI_BCAST(RAD1,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
C   
C     CALL ZENITH SIMPLY TO GET THE DAY OF THE YEAR FOR
C     THE SETUP OF THE OZONE DATA
C   
      TIME=(NTSD-1)*DT
      CALL ZENITH(TIME,DAYI,HOUR)
      ADDL=0.
      IF(MOD(IDAT(3),4).EQ.0)ADDL=1.
      RANG=PI2*(DAYI-RLAG)/(365.25+ADDL)
      RSIN1=SIN(RANG)
      RCOS1=COS(RANG)
      RCOS2=COS(2.*RANG)
      CALL O3CLIM
C
C-------------------------------------------------------------------
C***  SOME INITIAL VALUES RELATED TO TURBULENCE SCHEME
C-------------------------------------------------------------------
C
      DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
      DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
C
C  TRY A SIMPLE LINEAR INTERP TO GET 2/10 M VALUES
C
        PDSL(I,J)=PD(I,J)*RES(I,J)
        LMHK=LMH(I,J)
        LMVK=LMV(I,J)
        ULM=U(I,J,LMVK)
        VLM=V(I,J,LMVK)
        TLM=T(I,J,LMHK)
        QLM=Q(I,J,LMHK)
        PLM=PDSL(I,J)*AETA(LMHK)+PT
        APELM=(1.0E5/PLM)**CAPA
        APELMNW=(1.0E5/PSHLTR(I,J))**CAPA
        EXNERR=(PSHLTR(I,J)*1.E-5)**CAPA
        THLM=TLM*APELM
        DPLM=PDSL(I,J)*DETA(LMHK)*0.5
        DZLM=287.04*DPLM*TLM*(1.+0.608*QLM)/(9.801*PLM)

        FAC1=30./DZLM
        FAC2=(DZLM-30.)/DZLM
        IF(DZLM.LE.30.)THEN
          FAC1=1.
          FAC2=0.
        ENDIF
C
        IF(.NOT.RESTRT)THEN
          TH30(I,J)=FAC2*THS(I,J)+FAC1*THLM
          Q30(I,J)=FAC2*QS(I,J)+FAC1*QLM
          U30(I,J)=ULM
          V30(I,J)=VLM
        ENDIF
C
        FAC1=10./DZLM
        FAC2=(DZLM-10.)/DZLM
        IF(DZLM.LE.10.)THEN
          FAC1=1.
          FAC2=0.
        ENDIF
C 
        IF(.NOT.RESTRT)THEN
          TH10(I,J)=FAC2*THS(I,J)+FAC1*THLM
          Q10(I,J)=FAC2*QS(I,J)+FAC1*QLM
          U10(I,J)=ULM
          V10(I,J)=VLM
        ENDIF
C
        FAC1=2./DZLM
        FAC2=(DZLM-2.)/DZLM
        IF(DZLM.LE.2.)THEN
          FAC1=1.
          FAC2=0.
        ENDIF
C
        IF(.NOT.RESTRT.OR.NEST)THEN
          TSHLTR(I,J)=(FAC2*THS(I,J)+FAC1*THLM)
          QSHLTR(I,J)=FAC2*QS(I,J)+FAC1*QLM
        ENDIF
C***
C***  NEED TO CONVERT TO THETA IF IS THE RESTART CASE
C***  AS CHKOUT.f WILL CONVERT TO TEMPERATURE
C***
        IF(RESTRT)THEN
          TSHLTR(I,J)=TSHLTR(I,J)*APELMNW
        ENDIF
      ENDDO
      ENDDO
C
C--------------------------------------------------------------------
C     END OF SUBROUTINE INIT.
C-------------------------------------------------------------------
C
      IF(MYPE.EQ.0)THEN
        WRITE(LIST,*)'INIT:  EXIT INIT AND START MODEL INTEGRATION'
        WRITE(LIST,*)' '
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      BLOCK DATA CLOUD
      INCLUDE "parmeta"
C-----------------------------------------------------------------------
                             C O M M O N /RD1TIM/
     1 K400,CTHK(3),LTOP(3),PTOPC(4),TAUCV(3),RAD1
     2,LVL(IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
			     D A T A
     1 CTHK/20000.0,20000.0,20000.0/
     1,TAUCV/0.16, 0.14, 0.12/, LTOP/0,0,0/
C-----------------------------------------------------------------------
      END BLOCK DATA CLOUD
