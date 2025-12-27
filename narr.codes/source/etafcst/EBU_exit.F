              PROGRAM EBU
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: ETAFCST      EARLY ETA MODEL FORECAST DRIVER
C   PRGMMR: JANJIC           ORG: NP22        DATE: 99-01-20  
C
C ABSTRACT: EBU3 CONTAINS THE PRIMARY RUNSTREAM FOR THE EARLY ETA
C   FORECAST MODEL.  AFTER AN INITIAL CALL TO SUBROUTINE INIT, CALLS
C   ARE MADE TO SUBROUTINES WHICH COMPUTE THE VARIOUS DYNAMICAL AND
C   PHYSICAL PROCESSES IN THE MODEL.  THE VARIABLE 'NTSD' IS THE
C   FUNDAMENTAL TIMESTEP COUNTER AND THUS ITS VALUE DETERMINES WHEN
C   THE SUBROUTINES ARE CALLED.  INFORMATION PERTAINING TO THE SCHEMES
C   USED IN THE MODEL AS WELL AS ADDITIONAL REFERENCES MAY BE FOUND
C   IN "THE STEP-MOUNTAIN ETA COORDINATE REGIONAL MODEL:  A DOCUMEN-
C   TATION" (BLACK 1988; DEVELOPMENT DIVISION) AND "THE NEW NMC MESO-
C   SCALE ETA MODEL:  DESCRIPTION AND FORECAST EXAMPLES (BLACK 1994;
C   WEATHER AND FORECASTING).
C
C PROGRAM HISTORY LOG:
C   87-08-??  JANJIC,     ORIGINATOR
C             BLACK
C   93-05-12  TREADON     DOCBLOCK INSERTED
C   93-10-25  BLACK       DOCBLOCK UPDATED
C   97-03-15  MESINGER    SPLITTING MODIFIED, TO SEPARATE THE
C                         ADJUSTMENT AND THE ADVECTION STEP
C   97-11-19  BLACK       MODIFIED FOR DISTRIBUTED MEMORY
C   98-10-20  BLACK       DISTRIBUTED MEMORY FORM FOR 
C                         CURRENT OPERATIONAL CODE
C   00-02-25 TUCCILLO     INCORPORATED ASYNCHRONOUS I/O SERVERS
C
C
C USAGE:  MAIN PROGRAM
C
C   INPUT FILES:  NONE (SEE SUBROUTINE INIT)
C
C   OUTPUT FILES:  NONE (SEE SUBROUTINE CHKOUT)
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C       INIT     - INITIALIZE VARIABLES AT START OF INTEGRATION
C       DIVHOA   - DIVERGENCE, AND HORIZONTAL PART OF THE OMEGA-ALPHA
C                  TERM
C       PGCOR    - PRESSURE GRADIENT AND CORIOLIS FORCE
C       PDTE     - UPDATE SURFACE PRESSURE TENDENCY AND ETADOT
C       VTADV    - VERTICAL ADVECTION
C       HZADV    - HORIZONTAL ADVECTION OF T,U,V, AND TKE
C       HZADV2   - HORIZONTAL ADVECTION OF Q AND CLOUD WATER
C       DDAMP    - APPLY DIVERGENCE DAMPING
C       PDNEW    - UPDATE SURFACE PRESSURE
C       HDIFF    - LATERAL DIFFUSION
C       BOCOH    - UPDATE H POINTS ON THE BOUNDARIES
C       BOCOV    - UPDATE V POINTS ON THE BOUNDARIES
C       RADTN    - RADIATION DRIVER
C       RDTEMP   - APPLY TEMPERATURE TENDENCY DUE TO RADIATION
C       TURBL    - PERFORM THE VERTICAL TURBULENT EXCHANGE
C       SURFACE  - UPDATE SURFACE TEMPERATURE, MOISTURE, AND OTHER
C                  GROUND HYDROLOGY
C       GSCOND   - CLOUD WATER/ICE PHYSICS PARAMETERIZATION
C       CUCNVC   - CONVECTIVE ADJUSTMENT FOR DEEP OR SHALLOW CONVECTION
C       PRECPD   - GRID SCALE PRECIPITATION
C       CHKOUT   - POST PROFILE DATA.  FOR INTERNAL POST,
C                  POSTS MODEL OUTPUT.  FOR EXTERNAL POST,
C                  WRITES TEMPORARY FILE CONTAINING ALL MODEL
C                  ARRAYS.
C
C   EXIT STATES:
C     COND =   1 - NORMAL EXIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C
C$$$
C
C     ******************************************************************
C     *                                                                *
C     *                   LIMITED AREA ETA MODEL                       *
C     *                WITH STEP-MOUNTAIN TOPOGRAPHY                   *
C     *                                                                *
C     *                                                                *
C     *    NOAA / NATIONAL CENTERS FOR ENVIRONMENTAL PREDICTION,       *
C     *                                             CAMP SPRINGS, MD   *
C     *                                                                *
C     *  GEOPHYSICAL FLUID DYNAMICS LABORATORY / NOAA, PRINCETON, NJ,  *
C     *                                                                *
C     *  UNIVERSITY CORPORATION FOR ATMOSPHERIC RESEARCH, BOULDER, CO, *
C     *                              &                                 *
C     * DEPARTMENT OF METEOROLOGY, UNIVERSITY OF BELGRADE, YUGOSLAVIA  *
C     *                                                                *
C     ******************************************************************
C
C     ******************************************************************
C     *                                                                *
C     *                        REFERENCES                              *
C     *             FOR THE DYNAMICAL PART OF THE MODEL                *
C     *                                                                *
C     *  STEP-MOUNTAIN ETA COORDINATE:                                 *
C     *      F. MESINGER, 1983, IN RES. ACTIVITIES IN ATMOS. AND       *
C     *      OCEANIC MODELING, REP. NO. 5, WMO, GENEVA, 4.9-4.10.      *
C     *                                                                *
C     *  HORIZONTAL ADVECTION, CONTINUITY EQUATION:                    *
C     *      Z.I. JANJIC, 1984, MWR, 112, NO.6, 1234-1245.             *
C     *                                                                *
C     *  INTERNAL BOUNDARIES, OMEGA-ALPHA TERM, CODING, PERFORMANCE:   *
C     *      MESINGER ET AL., 1988, MWR, 116 NO.7, 1493-1518.          *
C     *                                                                *
C     *  N.B. FOR MORE DETAILS ON THESE TOPICS SEE ALSO:               *
C     *                                                                *
C     *  1.  MESINGER, F., AND Z.I. JANJIC, 1985: PROBLEMS AND         *
C     *        NUMERICAL METHODS OF THE INCORPORATION OF MOUNTAINS IN  *
C     *        ATMOSPHERIC MODELS.  LECTURES IN APPLIED MATHEMATICS,   *
C     *        VOL 22, AMER. MATH. SOC.; ALSO, NUMERICAL METHODS FOR   *
C     *        WEATHER PREDICTION, SEMINAR 1983, ECMWF, 103-157;       *
C     *        ALSO, SHORT- AND MEDIUM-RANGE WEATHER PREDICTION        *
C     *        RESEARCH PUBL. SER., NO. 8, WMO, GENEVA, 175-233.       *
C     *                                                                *
C     *  2.  JANJIC, Z.I., AND F. MESINGER, 1983: FINITE-DIFFERENCE    *
C     *        METHODS FOR THE SHALLOW WATER EQUATIONS ON VARIOUS      *
C     *        HORIZONTAL GRIDS.  NUMERICAL METHODS FOR WEATHER        *
C     *        PREDICTION, SEMINAR 1983, ECMWF,29-101.                 *
C     *                                                                *
C     *                     SOME  REFERENCES                           *
C     *             FOR THE PHYSICS PART OF THE MODEL                  *
C     *                                                                *
C     *  JANJIC, Z.I., 1990: THE STEP-MOUNTAIN COORDINATE:             *
C     *     PHYSICAL PACKAGE.  MONTHLY WEATHER REVIEW, VOL. 118,       *
C     *     NO. 7, 1429-1443.                                          *
C     *  JANJIC, Z.I., 1994: THE STEP MOUNTAIN ETA COORDINATE:         *
C     *     FURTHER DEVELOPMENTS OF THER CONVECTION, VISCOUS SUBLAYER, *
C     *     AND TURBULENCE CLOSURE SCHEMES.  MONTHLY WEATHER REVIEW,   *
C     *     VOL. 122, 927-945.                                         *
C     *                                                                *
C     *  ALSO SEE REFERENCES IN PHYSICAL SUBROUTINES                   *
C     *                                                                *
C     ******************************************************************
C
C     ******************************************************************
C     *                                                                *
C     *  THIS VERSION OF THE PROGRAM IS WRITTEN IN STANDARD ANSI       *
C     *  FORTRAN 90                                                    *
C     *                                                                *
C     *  PRINCIPAL PROGRAMMERS:                                        *
C     *                                                                *
C     *  Z. JANJIC, UNIVERSITY OF BELGRADE,                            *
C     *  T. BLACK, NCEP
C     *                                                                *
C     ******************************************************************
C     *                                                                *
C     *  THE MODEL USES THE SEMI-STAGGERED E GRID IN ARAKAWA NOTATION. *
C     *  HORIZONTAL INDEXING IS TWO-DIMENSIONAL.
C     *                                                                *
C     *                                                                *
C     *                                                                *
C     * H(1,JM)  V(1,JM)  H(2,JM)  V(2,JM) ...... V(IM-1,JM)  H(IM,JM) *
C     *    .        .        .        .               .          .     *
C     *    .        .        .        .               .          .     *
C     *    .        .        .        .               .          .     *
C     *    .        .        .        .               .          .     *
C     *                                                                *
C     * H(1,3)   V(1,3)   H(2,3)   V(2,3) ....... V(IM-1,3)   H(IM,3)  *
C     *                                                                *
C     * V(1,2)   H(1,2)   V(2,2)   H(2,2) ....... H(IM-1,2)   V(IM,2)  *
C     *                                                                *
C     * H(1,1)   V(1,1)   H(2,1)   V(2,1) ....... V(IM-1,1)   H(IM,1)  *
C     *                                                                *
C     *                                                                *
C     *                                                                *
C     *  ARRAYS ARE DIMENSIONED (IM,JM).  NOTE THAT A PHANTOM COLUMN   *
C     *  OF POINTS MUST EXIST ALONG THE EASTERN EDGE FOR THE ARRAYS    *
C     *  TO BE COMPLETE.                                               *
C     *                                                                *
C     *  THE TOTAL NUMBER OF GRID POINTS IN THE HORIZONTAL EXCLUDING   *
C     *  THE PHANTOM COLUMN IS IMJM=IM*JM-JM/2.                        *
C     *                                                                *
C     *  AUXILIARY ARRAYS ARE USED TO LOCATE NEIGHBORING GRID POINTS   *
C     *  WITH RESPECT TO A GIVEN GRID POINT.  IHE(J) IS THE INCREMENT  *
C     *  TO THE I INDEX NEEDED TO REFER TO THE V POINT EAST OF AN      *
C     *  H POINT THUS IHE(J)=0 ON ODD ROWS AND =1 ON EVEN ROWS.        *
C     *  IHW(J)=IHE(J)-1 IS THE INCREMENT TO THE INDEX OF AN H POINT   *
C     *  TO REFER TO THE V POINT TO THE WEST OF THAT H POINT.  THE     *
C     *  ANALOG EXISTS FOR THE ARRAYS IVE(J) AND IVW(J).               *
C     *                                                                *
C     *  BOUNDARY MASKS AND TOPOGRAPHY MASKS ARE DEFINED FOR VECTOR    *
C     *  PROCESSING. THE BOUNDARY MASKS HBM2(K) AND VBM2(K) ARE        *
C     *  EQUAL TO ONE EVERYWHERE EXCEPT AT THE TWO OUTERMOST ROWS      *
C     *  WHERE THEY ARE EQUAL TO ZERO. THE BOUNDARY MASK VBM3(K) IS    *
C     *  EQUAL TO ONE EVERYWHERE EXCEPT AT THE THREE OUTERMOST ROWS    *
C     *  WHERE IT IS EQUAL TO ZERO. THE TOPOGRAPHY MASKS (HTM(K,L)     *
C     *  AND VTM(K,L)) ARE SET TO ZERO UNDERNEATH THE TOPOGRAPHY AND   *
C     *  TO ONE ELSWHERE. IN ADDITION, FOR TREATMENT OF PHYSICAL       *
C     *  PROCESSES, MAXIMUM VALUES OF THE VERTICAL INDEX ARE DEFINED   *
C     *  AND STORED (LMH(K) AND LMV(K).
C     *                                                                *
C     ******************************************************************
C
C
C************************************************************************************
C
C      THE NUMBER OF QUILT SERVERS MUST AGREE WITH THE FOLLOWING RELATIONSHIP:
C
C       0 <=  NUMBER_QUILT_SERVERS <= JNPES
C 
C       WHERE THE NUMBER_QUILT_SERVERS = ( NUMBER_OF MPI_TASKS - INPES*JNPES )
C
C      PREFERABLY, THE NUMBER OF QUILT SERVERS DIVIDES EVENLY INTO JNPES
C
C         Jim Tuccillo August 2000
C
C************************************************************************************
C      
       INCLUDE 'EXCHM.h'
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
      INCLUDE "CONTIN.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
C***
C***  THE FOLLOWING ARE USED FOR TIMIMG PURPOSES ONLY
C***
      real*8 timef
      real nhb_tim,mpp_tim,init_tim
      common/timing/surfce_tim,nhb_tim,res_tim,exch_tim
CYL
      character envar*4, srfile*7
CYL
C
C-----------------------------------------------------------------------
C***
C***  INITIALIZE MPI,
C***  SETUP I/O SERVER MECHANICS AND CHECK FOR WHETHER A 
C***  SUFFICIENT NUMBER OF MPI TASKS HAVE BEEN INITIATED.
C***  IF INSUFFICIENT MPI TASK HAVE BEEN INITIATED THE 
C***  CODE WILL STOP IN SETUP_SERVERS
C***
      CALL SETUP_SERVERS(INPES*JNPES,
     *                   MYPE,
     *                   NPES,
     *                   IQUILT_GROUP,
     *                   INUMQ,
     *                   MPI_COMM_COMP,
     *                   MPI_COMM_INTER,
     *                   MPI_COMM_INTER_ARRAY)
C
      IF(MYPE.EQ.0)THEN
        CALL W3TAGB('ETAFCST ',0097,0365,0060,'NP22   ')
      ENDIF
C
c     IF(MYPE.EQ.NPES)THEN
c       CALL START()
c     ENDIF
C
C***
C***  AT THIS POINT NPES IS THE NUMBER OF MPI TASKS WORKING ON THE
C***  MODEL INTEGRATION. ALL OTHER TASKS ARE I/O SERVERS.
C
C***  AND AWAY WE GO !
C***
       IF(MYPE.GE.NPES)THEN
C
C***  FIRE UP THE I/O SERVERS
C
         CALL QUILT
C
       ELSE
C***
C***  THESE ARE THE TASKS THAT DO THE MODEL INTEGRATION
C***
C-----------------------------------------------------------------------
      mpp_tim=   0.
C
      bocoh_tim= 0.
      bocov_tim= 0.
      chkout_tim=0.
      cucnvc_tim=0.
      ddamp_tim= 0.
      divhoa_tim=0.
      exch_tim=  0.
      goss_tim=  0.
      gscond_tim=0.
      hdiff_tim= 0.
      hzadv_tim= 0.
      hzadv2_tim=0.
      init_tim=  0.
      nhb_tim=   0.
      pdnew_tim= 0.
      pdte_tim=  0.
      pgcor_tim= 0.
      precpd_tim=0.
cyl
      pptadj_tim=0.
cyl
      radtn_tim= 0.
      rdtemp_tim=0.
      res_tim=   0.
      surfce_tim=0.
      turbl_tim= 0.
      vtadv_tim= 0.
C-----------------------------------------------------------------------
C***
C***  INITIALIZE ALL QUANTITIES ASSOCIATED WITH GRID DECOMPOSITION
C***
      btimx=timef()
      btim=timef()
      CALL MPPINIT
      mpp_tim=mpp_tim+timef()-btim
C-----------------------------------------------------------------------
C--------INITIALIZE CONSTANTS AND VARIABLES-----------------------------
C--------DISTRIBUTE THE VALUES TO THE VARIOUS NODES/PEs-----------------
C-----------------------------------------------------------------------
c     CALL EXIT('INIT')
      bbtim=timef()
      CALL INIT  
c     call exit('INIT')
      init_tim=timef()-bbtim
C
      btim=timef()
      CALL GOSSIP
      goss_tim=goss_tim+timef()-btim
C-----------------------------------------------------------------------
C--------INVOKE THE LYNCH DIGITAL FILTER IF DESIRED--------------------
C-----------------------------------------------------------------------
c     DO NFLT=1,5
c       IF(NFLT.GT.1.AND.MYPE.EQ.0)THEN
c         REWIND NBC
c         READ(NBC)
c         READ(NBC)BCHR
c       ENDIF
C
c       BCHR=0.
c       CALL DIGFLT
C
c     ENDDO
CYL
C Look at the environmental variable, tmmark, to see whether we are running
C fcst for Eta or EDAS (if Eta, we don't read in the ppt obs or call ADJPPT)
C
      CALL GETENV("tmmark",ENVAR)
      IF(MYPE.EQ.0) PRINT *, "EBU finds that tmmark =",ENVAR
      IF(ENVAR.NE.'tm00') then
        write(srfile,10) envar
10      format('SR.',a4)
        open(90,file=srfile,form='unformatted')
        CALL READPCP
      endif
CYL
C-----------------------------------------------------------------------
C********ENTRY INTO THE TIME LOOP***************************************
C-----------------------------------------------------------------------
 2000 CONTINUE
      NTSD=NTSD+1
      IF(MYPE.EQ.0)WRITE(6,2001) NTSD,(NTSD-1)*DT
      IF(MYPE.EQ.0)WRITE(0,2001) NTSD,(NTSD-1)*DT
 2001 FORMAT('EBU:  TIMESTEP NTSD=',I5,'  FCST TIME=',F8.1)
C-----------------------------------------------------------------------
C------------------------RADIATION--------------------------------------
C-----------------------------------------------------------------------
c     if(mype.eq.4) print*,'before RADTN'
      IF(MOD(NTSD-1,NRADS).EQ.0.OR.MOD(NTSD-1,NRADL).EQ.0)THEN
        btim=timef()
        CALL RADTN
c       call exit('RADTN')
        if(mype.eq.1) print*,'After RADTN, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After RADTN, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After RADTN, CWM(25,31,31)=',CWM(25,31,31)
        radtn_tim=radtn_tim+timef()-btim
      ENDIF
C-----------------------------------------------------------------------
C------------------GENERATE INITIAL OUTPUT------------------------------
C-----------------------------------------------------------------------
      IF(NTSD.EQ.1.OR.NTSD.GT.1.AND.NTSD-1.EQ.NSTART+1)THEN
        btim=timef()
        CALL CHKOUT
        chkout_tim=chkout_tim+timef()-btim
      ENDIF
C-----------------------------------------------------------------------
C***  START THE ADJUSTMENT STEP: INTEGRATE FORWARD THE CONTINUITY
C***  EQUATION (UPDATE THE MASS FIELD)
C-----------------------------------------------------------------------
C***
C***  DIVERGENCE AND HORIZONTAL PART OF THE OMEGA-ALPHA TERM
C***
      IF(NTSD.GT.1)CALL EXCH(T,LM,U,LM,V,LM,Q,LM,2,2)
c     if(mype.eq.4) print*,'Before DIVHOA'
      btim=timef()
      CALL DIVHOA
        if(mype.eq.1) print*,'After DIVHOA, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After DIVHOA, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After DIVHOA, CWM(25,31,31)=',CWM(25,31,31)
c     print*,'after divhoa, t=',t
      call exit('DIVHOA')
      divhoa_tim=divhoa_tim+timef()-btim
C
C-----------------------------------------------------------------------
C--------PRESS. TEND.,ETA DOT & VERTICAL OMEGA-ALPHA--------------------
C-----------------------------------------------------------------------
C
      btim=timef()
c     if(mype.eq.4) print*,'Before PDTE'
      CALL PDTE
        if(mype.eq.1) print*,'After PDTE, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After PDTE, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After PDTE, CWM(25,31,31)=',CWM(25,31,31)
      call exit('PDTE')
      pdte_tim=pdte_tim+timef()-btim
C
C-----------------------------------------------------------------------
C--------DO VERTICAL ADVECTION WITHIN THE FIRST ADJUSTMENT STEP---------
C-----------------------------------------------------------------------
C
      IF(MOD(NTSD-1,IDTAD).EQ.0)THEN
        btim=timef()
        CALL EXCH(ETADT,LM-1,1,1)
        exch_tim=exch_tim+timef()-btim
C
        btim=timef()
c       if(mype.eq.4) print*,'before VTADV'
        CALL VTADV
        if(mype.eq.1) print*,'After VTADV, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After VTADV, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After VTADV, CWM(25,31,31)=',CWM(25,31,31)
        call exit('VTADV')
        vtadv_tim=vtadv_tim+timef()-btim
C
        btim=timef()
        CALL EXCH(T,LM,U,LM,V,LM,Q,LM,Q2,LM,1,1)
        exch_tim=exch_tim+timef()-btim
      ENDIF
C
C-----------------------------------------------------------------------
C--------UPDATING PRESSURE DIFFERENCE-----------------------------------
C-----------------------------------------------------------------------
C
      btim=timef()
c     if(mype.eq.4) print*,'before PDNEW'
      CALL PDNEW
        if(mype.eq.1) print*,'After PDNEW, Q(25,31,31)=',q(25,31,31)
        if(mype.eq.1) print*,'After PDNEW, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After PDNEW, CWM(25,31,31)=',CWM(25,31,31)
      call exit('PDNEW')
      pdnew_tim=pdnew_tim+timef()-btim
C
C-----------------------------------------------------------------------
C--------UPDATING BOUNDARY VALUES AT HEIGHT POINTS----------------------
C-----------------------------------------------------------------------
C
      btim=timef()
      IF(MOD(NTSD,IDTAD).EQ.0)THEN
        CALL EXCH(T,LM,Q,LM,Q2,LM,1,1)
      ENDIF
      CALL EXCH(PD,1,CWM,LM,1,1)
      exch_tim=exch_tim+timef()-btim
C
      btim=timef()
c     if(mype.eq.6) print*,'before BOCOH'
      CALL BOCOH
        if(mype.eq.1) print*,'After BOCOH, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After BOCOH, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After BOCOH, CWM(25,31,31)=',CWM(25,31,31)
      call exit('BOCOH')
c     if(mype.eq.6) print*,'after BOCOH'
      bocoh_tim=bocoh_tim+timef()-btim
C
C-----------------------------------------------------------------------
C***  INTEGRATE BACKWARD THE MOMENTUM EQUATION
C***  (UPDATE THE WIND FIELD)
C-----------------------------------------------------------------------
C
C***  PRESSURE GRADIENT AND CORIOLIS FORCE TERMS
C
      btim=timef()
      CALL EXCH(PD,1,T,LM,Q,LM,2,2)
      exch_tim=exch_tim+timef()-btim
C
      btim=timef()
c     if(mype.eq.4) print*,'before PGCOR'
      CALL PGCOR
      call exit('PGCOR')
        if(mype.eq.1) print*,'After PGCOR, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After PGCOR, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After PGCOR, CWM(25,31,31)=',CWM(25,31,31)
      pgcor_tim=pgcor_tim+timef()-btim
C
      btim=timef()
      CALL EXCH(PDSL,1,5,5)
      exch_tim=exch_tim+timef()-btim
C
C-----------------------------------------------------------------------
C--------DIVERGENCE DAMPING---------------------------------------------
C-----------------------------------------------------------------------
C
      IF(MOD(NTSD,NTDDMP).EQ.0)THEN
        btim=timef()
        CALL EXCH(T,LM,U,LM,V,LM,DIV,LM,1,1)
        exch_tim=exch_tim+timef()-btim
C
        btim=timef()
c       if(mype.eq.4) print*,'before DDAMP'
        CALL DDAMP
        call exit('DDAMP')
        if(mype.eq.1) print*,'After DDAMP, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After DDAMP, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After DDAMP, CWM(25,31,31)=',CWM(25,31,31)
        ddamp_tim=ddamp_tim+timef()-btim
      ENDIF
C-----------------------------------------------------------------------
C--------UPDATING BOUNDARY VALUES AT VELOCITY POINTS--------------------
C-----------------------------------------------------------------------
C
      btim=timef()
      CALL EXCH(U,LM,V,LM,1,1)
      exch_tim=exch_tim+timef()-btim
C
      btim=timef()
c     if(mype.eq.4) print*,'before BOCOV'
      CALL BOCOV
      call exit('BOCOV')
        if(mype.eq.1) print*,'After BOCOV, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After BOCOV, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After BOCOV, CWM(25,31,31)=',CWM(25,31,31)
      bocov_tim=bocov_tim+timef()-btim
C
C-----------------------------------------------------------------------
C***
C***  THE ADJUSTMENT STEP IS NOW DONE.  MAKE THE REMAINING CALLS WHICH
C***  TRADITIONALLY (SO FAR) HAVE BEEN DONE EVERY ADJUSTMENT STEP
C***
C-----------------------------------------------------------------------
C--------APPLY TEMPERATURE TENDENCY DUE TO RADIATION--------------------
C-----------------------------------------------------------------------
C
      btim=timef()
c     if(mype.eq.4) print*,'Before RDTEMP'
      CALL RDTEMP
      call exit('RDTEMP')
        if(mype.eq.1) print*,'After RDTEMP, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After RDTEMP, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After RDTEMP, CWM(25,31,31)=',CWM(25,31,31)
      rdtemp_tim=rdtemp_tim+timef()-btim
C
C-----------------------------------------------------------------------
C--------LATERAL DIFFUSION----------------------------------------------
C-----------------------------------------------------------------------
C
      btim=timef()
      CALL EXCH(T,LM,U,LM,V,LM,Q,LM,2,2)
      CALL EXCH(Q2,LM,1,1)
      exch_tim=exch_tim+timef()-btim
C
      btim=timef()
c     if(mype.eq.4) print*,'before HDIFF'
      CALL HDIFF
      call exit('HDIFF')
        if(mype.eq.1) print*,'After HDIFF, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After HDIFF, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After HDIFF, CWM(25,31,31)=',CWM(25,31,31)
      hdiff_tim=hdiff_tim+timef()-btim
C
C-----------------------------------------------------------------------
C------- HORIZONTAL ADVECTION ------------------------------------------
C-----------------------------------------------------------------------
C
      IF(MOD(NTSD,IDTAD).EQ.0)THEN
        btim=timef()
        CALL EXCH(T,LM,U,LM,V,LM,4,4)
        CALL EXCH(Q2,LM,5,5)
        exch_tim=exch_tim+timef()-btim
C
        btim=timef()
c       if(mype.eq.4) print*,'before HZADV'
        CALL HZADV
        if(mype.eq.1) print*,'After HZADV, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After HZADV, T(25,31,31)=',T(25,31,31)
       if(mype.eq.1) print*,'After HZADV, CWM(25,31,31)=',CWM(25,31,31)
        call exit('HZADV')
        hzadv_tim=hzadv_tim+timef()-btim
C
        btim=timef()
        CALL EXCH(U,LM,V,LM,Q,LM,CWM,LM,2,2)
        exch_tim=exch_tim+timef()-btim
C
C***  HORIZONTAL ADVECTION OF WATER SUBSTANCE
C
        btim=timef()
        CALL HZADV2
        if(mype.eq.1) print*,'After HZADV2, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After HZADV2, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After HZADV2, CWM(25,31,31)=',CWM(25,31,31)
        call exit('HZADV2')
        hzadv2_tim=hzadv2_tim+timef()-btim
      ENDIF
C-----------------------------------------------------------------------
C
C***  IF THE TIME IS RIGHT, NOW DO VARIOUS PHYSICS CALLS
C***  (WARNING: TO AVOID ENDING THE INTEGRATION WITH PHYSICS CALLS
C***  WHICH HAVE NOT BEEN FOLLOWED BY ADJUSTMENT STEPS, PHYSICS CALLS
C***  ARE OFFSET BY HALVES OF VARIOUS CALLING INTERVALS.  IT IS
C***  ASSUMED THAT THE CALLING INTERVALS, NPHS AND NCNVC,
C***  ARE DIVISIBLE BY IDTAD.  IF NOT, INTEGRATION WILL END WITH AN
C***  INCORRECT NUMBER OF CALLS HAVING BEEN MADE.
C
C-----------------------------------------------------------------------
C--------TURBULENT PROCESSES AND PRECIPITATION--------------------------
C-----------------------------------------------------------------------
      IF(MOD(NTSD-NPHS/2,NPHS).EQ.0)THEN
        btim=timef()
        CALL EXCH(PD,1,UZ0,1,VZ0,1,T,LM,U,LM,V,LM,Q,LM,1,1)
        exch_tim=exch_tim+timef()-btim
C
        btim=timef()
c       if(mype.eq.4) print*,'before TURBL'
        CALL TURBL                     !Contains calls to EXCH
        call exit('TURBL')
        if(mype.eq.1) print*,'After TURBL, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After TURBL, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After TURBL, CWM(25,31,31)=',CWM(25,31,31)
        turbl_tim=turbl_tim+timef()-btim
      ENDIF
C-----------------------------------------------------------------------
C--------CONDENSATION/EVAPORATION OF CLOUD WATER------------------------
C-----------------------------------------------------------------------
      IF(MOD(NTSD-NPHS/2,NPHS).EQ.0)THEN
        btim=timef()
c       if(mype.eq.4) print*,'before GSCOND'
        CALL GSCOND
        if(mype.eq.1) print*,'After GSCOND, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After GSCOND, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After GSCOND, CWM(25,31,31)=',CWM(25,31,31)
        call exit('GSCOND')
        gscond_tim=gscond_tim+timef()-btim
      ENDIF
C-----------------------------------------------------------------------
C--------CONVECTIVE PRECIPITATION---------------------------------------
C-----------------------------------------------------------------------
      IF(MOD(NTSD-NCNVC/2,NCNVC).EQ.0)THEN
        btim=timef()
c       if(mype.eq.4) print*,'before CUCNVC'
        CALL CUCNVC
        call exit('CUCNVC')
        if(mype.eq.1) print*,'After CUCNVC, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After CUCNVC, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After CUCNVC, CWM(25,31,31)=',CWM(25,31,31)
        cucnvc_tim=cucnvc_tim+timef()-btim
      ENDIF
C-----------------------------------------------------------------------
C--------GRIDSCALE PRECIPITATION----------------------------------------
C-----------------------------------------------------------------------
      IF(MOD(NTSD-NPHS/2,NPHS).EQ.0)THEN
        btim=timef()
c       if(mype.eq.4) print*,'before PRECPD'
c       CALL PRECPD
        if(mype.eq.1) print*,'After PRECPD, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After PRECPD, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After PRECPD, CWM(25,31,31)=',CWM(25,31,31)
        call exit('PRECPD')
        precpd_tim=precpd_tim+timef()-btim
      ENDIF
C-----------------------------------------------------------------------
C--------PRECIPIPTATION ASSIMILATION------------------------------------
C-----------------------------------------------------------------------
      IF(MOD(NTSD-NPHS/2,NPHS).EQ.0 .AND. ENVAR.NE.'tm00')THEN
        btim=timef()
c       if(mype.eq.4) print*,'before CHKSNOW'
        CALL CHKSNOW
        call exit('CHKSNOW')
        if(mype.eq.1) print*,'After CHKSNOW, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After CHKSNOW, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After CHKSNOW, CWM(25,31,31)=',CWM(25,31,31)
c       if(mype.eq.4) print*,'before ADJPPT'
        CALL ADJPPT
        call exit('ADJPPT')
        if(mype.eq.1) print*,'After ADJPPT, Q(25,31,31)=',Q(25,31,31)
        if(mype.eq.1) print*,'After ADJPPT, T(25,31,31)=',T(25,31,31)
      if(mype.eq.1) print*,'After ADJPPT, CWM(25,31,31)=',CWM(25,31,31)
c       if(mype.eq.4) print*,'after ADJPPT'
        pptadj_tim=pptadj_tim+timef()-btim
      ENDIF
C-----------------------------------------------------------------------
C--------IS IT TIME FOR A CHECK POINT ON THE MODEL HISTORY FILE?--------
C-----------------------------------------------------------------------
c     if(mype.eq.4) print*,'before CHKOUT'
      IF(NTSD.GT.NSTART+1)THEN
        btim=timef()
        CALL CHKOUT
        chkout_tim=chkout_tim+timef()-btim
      ENDIF
C-----------------------------------------------------------------------
C--------CLEAN UP AFTER RESTART-----------------------------------------
C-----------------------------------------------------------------------
      IF(RESTRT)THEN
        RESTRT=.FALSE.
      ENDIF
C-----------------------------------------------------------------------
      IF(NTSD.LT.NTSTM)GO TO 2000
C***********************************************************************
C********EXIT FROM THE TIME LOOP****************************************
C***********************************************************************
C
 2005 continue
      tot2_tim=timef()-btimx
      tot_tim=mpp_tim+init_tim+goss_tim+radtn_tim+chkout_tim+
     1        divhoa_tim+pdte_tim+vtadv_tim+pdnew_tim+bocoh_tim+
     2        pgcor_tim+ddamp_tim+bocov_tim+rdtemp_tim+hdiff_tim+
     3        hzadv_tim+hzadv2_tim+turbl_tim+gscond_tim+cucnvc_tim+
     4        precpd_tim+exch_tim
cyl
     5       +pptadj_tim
cyl 
C
      if(mype.eq.0)then
        pct=mpp_tim/tot_tim*1.e2
        write(6,*)' mpp=',mpp_tim*1.e-3,'    pct=',pct
        pct=init_tim/tot_tim*1.e2
        write(6,*)' init=',init_tim*1.e-3,'   pct=',pct
        pct=goss_tim/tot_tim*1.e2
        write(6,*)' goss=',goss_tim*1.e-3,'   pct=',pct
        pct=radtn_tim/tot_tim*1.e2
        write(6,*)' radtn=',radtn_tim*1.e-3,'  pct=',pct
        pct=chkout_tim/tot_tim*1.e2
        write(6,*)' chkout=',chkout_tim*1.e-3,' pct=',pct
        pct=divhoa_tim/tot_tim*1.e2
        write(6,*)' divhoa=',divhoa_tim*1.e-3,' pct=',pct
        pct=pdte_tim/tot_tim*1.e2
        write(6,*)' pdte=',pdte_tim*1.e-3,'   pct=',pct
        pct=vtadv_tim/tot_tim*1.e2
        write(6,*)' vtadv=',vtadv_tim*1.e-3,'  pct=',pct
        pct=pdnew_tim/tot_tim*1.e2
        write(6,*)' pdnew=',pdnew_tim*1.e-3,'  pct=',pct
        pct=bocoh_tim/tot_tim*1.e2
        write(6,*)' bocoh=',bocoh_tim*1.e-3,'  pct=',pct
        pct=pgcor_tim/tot_tim*1.e2
        write(6,*)' pgcor=',pgcor_tim*1.e-3,'  pct=',pct
        pct=ddamp_tim/tot_tim*1.e2
        write(6,*)' ddamp=',ddamp_tim*1.e-3,'  pct=',pct
        pct=bocov_tim/tot_tim*1.e2
        write(6,*)' bocov=',bocov_tim*1.e-3,'  pct=',pct
        pct=rdtemp_tim/tot_tim*1.e2
        write(6,*)' rdtemp=',rdtemp_tim*1.e-3,' pct=',pct
        pct=hdiff_tim/tot_tim*1.e2
        write(6,*)' hdiff=',hdiff_tim*1.e-3,'  pct=',pct
        pct=hzadv_tim/tot_tim*1.e2
        write(6,*)' hzadv=',hzadv_tim*1.e-3,'  pct=',pct
        pct=hzadv2_tim/tot_tim*1.e2
        write(6,*)' hzadv2=',hzadv2_tim*1.e-3,' pct=',pct
        pct=turbl_tim/tot_tim*1.e2
        write(6,*)' turbl=',turbl_tim*1.e-3,'  pct=',pct
        pct=gscond_tim/tot_tim*1.e2
        write(6,*)' gscond=',gscond_tim*1.e-3,' pct=',pct
        pct=cucnvc_tim/tot_tim*1.e2
        write(6,*)' cucnvc=',cucnvc_tim*1.e-3,' pct=',pct
        pct=precpd_tim/tot_tim*1.e2
        write(6,*)' precpd=',precpd_tim*1.e-3,' pct=',pct
cyl
        pct=pptadj_tim/tot_tim*1.e2
        write(6,*)' pptadj=',pptadj_tim*1.e-3,' pct=',pct
cyl
        pct=exch_tim/tot_tim*1.e2
        write(6,*)' exch=',exch_tim*1.e-3,'   pct=',pct
        write(6,*)' total=',tot_tim*1.e-3
        write(6,*)' total2=',tot2_tim*1.e-3
      endif
C----------------------------------------------------------------------
C
C***  WE MUST NOW SHUT DOWN THE I/O SERVERS
C***  THIS IS DONE BY SENDING A -999 TO MPI TASK 0 OF EACH SERVER GROUP
C
      IF(MYPE.EQ.0)THEN
        DO I=1,IQUILT_GROUP
          CALL MPI_SEND(-999,1,MPI_INTEGER,0,0,
     *                  MPI_COMM_INTER_ARRAY(I),IER)
        ENDDO
      ENDIF
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      ENDIF    !  ENDIF ON TASKS FOR MODEL INTEGRATION VS I/O SERVING
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
C
      IF(MYPE.EQ.0) THEN 
        CALL W3TAGE('ETAFCST ')
      ENDIF
C
c     IF(MYPE.EQ.NPES)THEN
c       CALL SUMMARY()
c     ENDIF
C
      CALL MPI_FINALIZE(IERR)
C----------------------------------------------------------------------
                            STOP1
                            END
