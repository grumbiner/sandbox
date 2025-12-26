                              SUBROUTINE ADJPPT
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    ADJPPT      PRECIPITATION ASSIMILATION ADJUSTMENT
C   PRGRMMR: BALDWIN         ORG: W/NP22     DATE: 98-08-26       
C     
C ABSTRACT:
C     ADJPPT MODIFIES THE ENVIRONMENT AND RELEASES LATENT HEAT
C     TO ATTEMPT TO MATCH THE AMOUNT OF OBSERVED PRECIPITATION.
C     IF THE OBSERVED PRECIPITATION IS DETERMINED TO BE CONVECTIVE IN TYPE,
C     THE LATENT HEAT RELEASE AND MOISTURE CHANGE IS FORMULATED TO FOLLOW
C     THE BETTS-MILLER-JANJIC SCHEME.  OTHERWISE ADJUSTMENTS ARE MADE
C     IN A MANNER CONSISTENT WITH THE GRID-SCALE CLOUD PHYSICS SCHEME.
C     
C     
C PROGRAM HISTORY LOG:
C   98-08-26  BALDWIN    - ORIGINATOR
C     
C USAGE: CALL ADJPPT FROM MAIN PROGRAM EBU
C
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
C     UNIQUE:
C        TTBLEX
C  
C     LIBRARY:
C        NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  LOOPS
C                  MASKS
C                  PHYS
C                  VRBLS
C                  CNVCLD
C                  PVRBLS
C                  ACMCLH
C                  INDX
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C----------------------------------------------------------------------
      INCLUDE "cuparm"
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "mpp.h"
C----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM,JAM=6+2*(JM-10)
     &, IMJM_LOC=IDIM2*JDIM2
     &, LP1=LM+1,LM1=LM-1)
      PARAMETER (ELIV=2.834E6, DLDT=2274.E0)
C----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CNVCLD.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMCLH.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
      INCLUDE "PPTASM.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & TREFK (LM),QREFK (LM),PK    (LM),APEK  (LM),TK    (LM)
     &,THSK  (LM),PSK   (LM),APESK (LM),QK    (LM),THERK (LM)
     &,THVREF(LM),THEVRF(LM),THVMOD(LM),DIFT  (LM),DIFQ  (LM)
     &,QSATK (LM),FPK   (LM),RELH  (LM)
C
                             D I M E N S I O N
     & LTOP  (IDIM1:IDIM2,JDIM1:JDIM2),LBOT  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PTOP  (IDIM1:IDIM2,JDIM1:JDIM2),PBOT  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,IPTB  (IDIM1:IDIM2,JDIM1:JDIM2),ITHTB (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PDSL  (IDIM1:IDIM2,JDIM1:JDIM2),APEBT (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PRECL(IDIM1:IDIM2,JDIM1:JDIM2), QC(LM)
     &,TBT   (IDIM1:IDIM2,JDIM1:JDIM2),Q2BT  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,QQ    (IDIM1:IDIM2,JDIM1:JDIM2),PP    (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PSP   (IDIM1:IDIM2,JDIM1:JDIM2),THBT  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,THESP (IDIM1:IDIM2,JDIM1:JDIM2),P     (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ILRES (IMJM_LOC),JLRES (IMJM_LOC)
     &,IHRES (IMJM_LOC),JHRES (IMJM_LOC)
     &,DDATA(IDIM1:IDIM2,JDIM1:JDIM2), ADATA(IDIM1:IDIM2,JDIM1:JDIM2)
     &,APE   (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,TREF  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
C
C-----------------------------------------------------------------------
C  CPREC: model convective precip at each time step.  
C    PREC = CPREC + APREC
C
C  DDATA: Pobs at each time step
C
C  If observed precip is larger than model precip, give the 
C  convective adjustment the first chance to make the rain.  (Initially
C  I wanted to keep the conv/gridscale precip invariant through ADJPPT,
C  but then thought better of it - in the case of Pobs >> Pmod, the 
C  instability might build up too much if we don't let the conv adj
C  take care of as much of it as possible).  
C  
C  ADATA: After convective adjustment, let ADATA = DDATA - CPREC
C    (i.e. ADATA is the portion of the observed precip not able to
C     be accounted for by convective adjustment, and to be accounted
C     for in the grid-scale portion of the adjustment scheme).
C    'CPREC' is not an actual variable in this routine.  
C
C  PPTSUM is used to keep track of total observed precip at grid point
C  (itest,jtest) during the 3-hour assimilation period (to make sure that
C  we are partitioning the hourly precip obs correctly).  PPTSUM is zero
C  before the first call to ADJPPT, and the accumulated value is saved
C  between subsequent calls.
C
      LOGICAL IGSADJ(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL PFACTOR(IDIM1:IDIM2,JDIM1:JDIM2)
C
C  IGSADJ: flag for whether to make grid-scale adjustment 
C     (T - do GS adjustment.  F - don't do GS adjustment)
C  IGSADJ is false if
C     1) PPTDAT = 999., or
C     2) DDATA = 0., or
C     3) DDATA .LE. PREC, or
C     4) While DDATA > PREC, all (pre-adj) Pmod is convective, and
C        the entire amount of DDATA is accounted for during convective
C        adjustment
C  IGSADJ is true if the convective adjustment does not
C        produce enough convective precip to account for DDATA
      
C
      DATA PPTSUM/0./
      SAVE PPTSUM
C
C----------------------------------------------------------------------
      IF (NTSD.EQ.1) THEN
         TLAT=0.
         RETURN
      ENDIF
      IF (NTSD.GT.NHEAT) RETURN
C
C-----------------------------------------------------------------------
C--------------INITIALIZE GRID-SCALE ADJUSTMENT MASK--------------------
C--------------(later the mask will be updated during conv adj)---------
C-----------------------------------------------------------------------
C
      IGSADJ = .FALSE.
C
C PREPATORY CALCULATIONS
C-----------------------------------------------------------------------
      DTCNVC=NCNVC*DT
      RDTCNVC=1./DTCNVC
      TAUK=DTCNVC/TREL
      CTHRS=(0.006350/86400.)*DTCNVC
      TIMES=(NTSD-1)*DT
      COUNT=MOD(TIMES,3600.)
      IHR=(TIMES-1.0)/3600.+1
      PHYST=DTCNVC
C
      IF (COUNT.GE.PHYST.OR.COUNT.EQ.0.0) THEN
         FRACT=PHYST/3600.
      ELSE
         FRACT=COUNT/3600.
      END IF
C
c 
c  Check to see if this is the last time step before the end.  If so, 
c  applying the remaining fraction of precip to this time step.
c 
c  No, don't do that.  For the 80km runs, this would mean an 25% increase
c  of precip in the last time step, and it'll affect the temperature and
c  the moisture fields inappriately.  Better to increase the model precip
c  after precip assim adjustment by 25% in the end (i.e. multiply by
c  'ENDFCTR').
c
      TEND=3.0
c     print*,'tend=',tend
      IF (TEND*3600.-TIMES .LT. PHYST) THEN
c        FEND = TEND - TIMES/3600.
         ENDFCTR = (TEND*3600-TIMES+PHYST)/PHYST
      ELSE 
c        FEND = 0.
         ENDFCTR = 1.
      ENDIF
c
C
      IF (TIMES .LT. PHYST) THEN
         ZER=1.0E-05*FRACT
      ELSE
         ZER=1.0E-05*PHYST/3600.
      ENDIF
C 
C   ZER IS OUR ZERO THRESHOLD;  .01 MM PER HOUR
C   (CORRESPONDS TO 1 HUNDRETH OF AN INCH PER DAY)
C 
      SIXSIX=PHYST/3600.
C
C   Under one of the scenarios (when Pobs > 0 and Pmod=0), we need to 
C   create a layer of precipitating cloud from scratch.  We specify 3
C   cloud-thicknesses based on the precipitation amount:
C
      PTRES1=2.81E-03*SIXSIX
      PTRES2=3.75E-04*SIXSIX
      PTRES3=1.0E-03*SIXSIX
C
c     print*,'mype,mtstpe=',mype,mtstpe
c     print*,'itstloc,jtstloc=',itstloc,jtstloc
      IF (MYPE.EQ.MTSTPE) THEN
        WRITE(98,*) 'NTSD=',NTSD,' TIMES=',TIMES,' FRACT=',FRACT,
     &              ' ENDFCTR=', ENDFCTR
        WRITE(98,*) 'IHR=', IHR,' PPTDAT=',PPTDAT(ITSTLOC,JTSTLOC,IHR)
c     print*,'pptdat(itstloc,jtstloc,ihr)=',pptdat(itstloc,jtstloc,ihr)
      ENDIF
C-----------------------------------------------------------------------
C   FRACT IS THE FRACTION OF IHR'S PRECIP THAT WE WANT FOR
C   THIS ADJUSTMENT, WE WANT (PHYST/3600-FRACT) WORTH OF IHR-1 PRECIP
C   WE HAVE DATA ONLY FOR IHR=1,3
C-----------------------------------------------------------------------
C   SET UP OBSERVED PRECIP FOR THIS TIMESTEP IN DDATA
C-----------------------------------------------------------------------
C
      PFACTOR = 1.
C
!$omp parallel do private(pdiff,pexp,pptsum)
      DO 110 J=MYJS,MYJE
      DO 100 I=MYIS,MYIE
C---
        R2D=57.2957795   ! 180.0/PI
        GLATMIN=27.5
        GLATMAX=42.5
        GLATD=GLAT(I,J)*R2D
        IF (GLATD.GE.GLATMAX .AND. (SM(I,J)+SICE(I,J)).GT.0.5) THEN
           PPTDAT(I,J,IHR)=999.0
        END IF
C---
        IF (PPTDAT(I,J,IHR).GT.900.) GO TO 100
        IF (IHR.EQ.1 .OR. PPTDAT(I,J,IHR-1).GT.900.) THEN
           DDATA(I,J) = PPTDAT(I,J,IHR)*FRACT
        ELSE
           DDATA(I,J) =   PPTDAT(I,J,IHR)*FRACT
     &             + PPTDAT(I,J,IHR-1)*(SIXSIX-FRACT)
c    &             + PPTDAT(I,J,3)*FEND
        ENDIF
C---
CC        IF ((SM(I,J)+SICE(I,J)).GT.0.5) THEN
        IF (SM(I,J).GT.0.5) THEN
           IF (GLATD.GT.GLATMIN .AND. GLATD.LT.GLATMAX) THEN
              AFAC=1.0-((GLATD-GLATMIN)/(GLATMAX-GLATMIN))
              DDATA(I,J) = AFAC*DDATA(I,J) + (1.0-AFAC)*PREC(I,J)
           END IF
        END IF
C---
C
C  Use the difference between Pobs and Pmod to modify RH in the cloud
C  (M. Baldwin, 20 Apr 99) by a factor of 
C  1.0+0.2*(exp(r)-exp(-r))/(exp(r)+exp(-r)) where:  r=(Pobs-Pmod in mm)/25mm
C  (Pobs and Pmod are hourly precip).  The value of the factor would be
C  between 0.8 and 1.2.
C
CC  The moisture field at T0 seems to be too wet.  So only use PFACTOR to
CC  dry the atmosphere if Pobs < Pmod.  Otherwise set PFACTOR to 1.
CC  No, don't do that ... for now.
        PDIFF = (DDATA(I,J)-PREC(I,J))/(0.025*FRACT)
CC        PDIFF = AMIN1(0.,PDIFF)
        PEXP = EXP(PDIFF)
        PEXP = PEXP * PEXP
        PFACTOR(I,J) = 1.0 + 0.2 * (PEXP-1.)/(PEXP+1.)
C
C
C  If PREC > 0 (i.e. Pmod > 0), partition DDATA into 'convective' 
C  and 'grid scale', based on the ratio of APREC/PREC.
C
C  IF Pmod = 0, first assume that all observed precip are in fact
C  convective (we will try to let convective adjustment take care
C  of it first.  If there's any leftover DDATA un-accounted for, 
C  we then let grid-scale precip take care of it.
C
        IF (I.EQ.ITSTLOC .AND. J.EQ.JTSTLOC .AND. MYPE.EQ.MTSTPE) THEN
c         print*,'should be writing to unit 98'
          PPTSUM = PPTSUM + DDATA(I,J)*ENDFCTR
          WRITE(98,*) 'DDATA=',DDATA(I,J),' PREC=',PREC(I,J),
     &     ' APREC=', APREC(I,J), ' ZER=',ZER,' PPTSUM=', PPTSUM
          WRITE(98,*) 'PFACTOR=', PFACTOR(I,J), ' PDIFF=', PDIFF,
     &     ' PEXP=', PEXP
c         print*,'i,j,ddata(i,j),prec(i,j),pptsum=',
c    *       i,j,ddata(i,j),prec(i,j),pptsum
        ENDIF
 100  CONTINUE
 110  CONTINUE
C
C  Set minimum cloud depth for deep convection.  This would be scaled
C  by the total atmosphere depth (PSFCIJ) at this horizontal point later on.
C
      PSHNEW=20000.
C
C  The big loop - looping through all horizontal grid points
C  In M. Baldwin's original ADJPPT.f, there was no 'big (i,j) loop'.  
C  I am replacing the many little loops in his code with this big loop.
C  The upper and lower limits of I and J are chosen to be this way to
C  be consistent with his loop limits in the buoyancy calculation.
C
!$omp parallel do 
!$omp&  private(adjust,ai,apekl,apekxx,apekxy,apesp,apests,
!$omp&          bi,bq,bqs00k,bqs10k,climit,cratio,
!$omp&          delt,deltacp,delcwm,delq,depmin,depth,depwl,detacl,
!$omp&          dsp,dsp0k,dspbk,dsptk,dthem,
!$omp&          efi,efinew,elv,etabot,etatop,etbig,
!$omp&          factor,fi,fiw,fiwl1,
!$omp&          iq,iqtb,it,itb,ittb,ittbk,ivi,
!$omp&          knuml,knunh,l0,l0m1,lb,lbtk,lbm1,lcbottm,
!$omp&          lmhij,lmhk,ltp1,ltpk,numlev,oldcwm,oldq,oldrh,
!$omp&          p00k,p01k,p10k,p11k,petal,pk0,pkb,pkl,pkt,
!$omp&          pp1,prec1,preck,precmax,presk,psfc,psfck,
!$omp&          qbt,qckl,qkl,qq1,qi,qint,qw,
!$omp&          ratio,rdp0t,relhum,rhfctr,sq,sqs00k,sqs10k,stabdl,
!$omp&          therkx,therky,tkl,tmt0,tmt15,tpsp,tq,trefkx,
!$omp&          ttemp,tth,tthbt,tthes,wfix,wmin,yltmp)
      DO 910 J=MYJS,MYJE
      DO 900 I=MYIS,MYIE
        IF (PPTDAT(I,J,IHR).GT.900. .OR. 
     &      DDATA(I,J).LE.ZER .AND. PREC(I,J).LE.ZER) GOTO 900
C-----------------------------------------------------------------------
C--------------PREPARATIONS---------------------------------------------
C-----------------------------------------------------------------------
        THESP(I,J)=0.
        PDSL (I,J)=RES(I,J)*PD(I,J)
        LBOT (I,J)=LMH(I,J)
        PBOT(I,J)=AETA(LBOT(I,J))*PDSL(I,J)+PT
        TREF(I,J,1)=T(I,J,1)
C-----------------------------------------------------------------------
C---  CASE 1. Pobs = 0, Pmod > 0
C--------------IF OBSERVED PRECIP IS LESS THAN OR EQUAL TO ZER----------
C--------------TAKE BACK THE LATENT HEAT RELEASE------------------------
C-----------------------------------------------------------------------
        IF (DDATA(I,J).LE.ZER .AND. PREC(I,J).GT.ZER) THEN
          CLDEFI(I,J)=STEFI
          DO 130 L=1,LM
            IF (HTM(I,J,L).LT.0.5) GO TO 130
C--------- -FIND THE PRE-MODIFIED RELATIVE HUMIDITY FOR THIS POINT------
            PETAL=PDSL(I,J)*AETA(L)+PT
            QCKL=PQ0/PETAL
     2       *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))

            if (QCKL.eq.0.0) then
               write(0,*)"QCKL=0.0",QCKL,I,J,L,MYPE
               write(0,*)PQ0,PETAL,T(I,J,L)
               write(*,*)"QCKL=0.0",QCKL,I,J,L,MYPE
               write(*,*)PQ0,PETAL,T(I,J,L)
               CALL MPI_FINALIZE(IERR)
               STOP 8
            end if

            RELHUM=Q(I,J,L)/QCKL
            OLDRH=RELHUM
            OLDQ=Q(I,J,L)
            OLDCWM=CWM(I,J,L)
C   MODIFY THE TEMP AND PRECIP
            T(I,J,L)=T(I,J,L)-TLAT(I,J,L)
C   Reduce RH by the factor PFACTOR:
            QCKL=PQ0/PETAL
     2       *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
            RELHUM= RELHUM * PFACTOR(I,J)
            IF (TLAT(I,J,L).GT.0.) Q(I,J,L)=RELHUM*QCKL
            IF (I.EQ.ITSTLOC .AND. J.EQ.JTSTLOC .AND. MYPE.EQ.MTSTPE)
     2        WRITE(98,129) L, PETAL,TLAT(I,J,L), OLDRH, PFACTOR(I,J), 
     2        RELHUM, OLDQ, Q(I,J,L)
 129        FORMAT(I2,X,f7.0,x,f7.4,X,3(2X,F6.4), X, 2(2X,E12.6))
C If any part of model-predicted rainfall was grid-scale, decrease the
C cloud water mixing ratio ( if > WMIN) to the minimum value, WMIN:
            IF (APREC(I,J).GT.0.) THEN
              TTEMP=0.025*(T(I,J,L)-273.16)
              WFIX=0.9814*EXP(0.01873*L)
              WMIN=0.1E-3*EXP(TTEMP)*WFIX
c             if(mype.eq.17) then
c               print*,'in 130 loop'
c               print*,'i,j,l=',i,j,l
c               print*,'cwm(i,j,l),wmin=',cwm(i,j,l),wmin
c               print*,'ttemp=',ttemp
c               print*,'t(i,j,l)=',t(i,j,l)
c             endif
              CWM(I,J,L) = AMIN1(WMIN,CWM(I,J,L))
            ENDIF
c
c  Calculate the water vapor and cloud water/ice increments for
c    1) the entire column
c    2) sfc-700mb
c
            DELQ   = (Q(I,J,L)-OLDQ)     * DETA(L)*PDSL(I,J)/G
            DELCWM = (CWM(I,J,L)-OLDCWM) * DETA(L)*PDSL(I,J)/G
c
            VAPINC(I,J)=VAPINC(I,J)+DELQ
            CLDINC(I,J)=CLDINC(I,J)+DELCWM
C
            IF (PETAL.GE.70000.) THEN
              VAPINC7(I,J)=VAPINC7(I,J)+DELQ
              CLDINC7(I,J)=CLDINC7(I,J)+DELCWM
            ENDIF
 130      CONTINUE
C  Take back the PREC from ACPREC and CUPREC as well.  For CUPREC, 
C  the amount taken back depends how much convective precip there
C  was before the adjustment.  
          ACPREC(I,J)=ACPREC(I,J)-PREC(I,J)
          CUPREC(I,J)=CUPREC(I,J)-(PREC(I,J)-APREC(I,J))
           CUPPT(I,J)= CUPPT(I,J)-(PREC(I,J)-APREC(I,J))
          PREC(I,J)=0.
          APREC(I,J)=0.        
          GO TO 900
        ENDIF
C
C CASE 2, Pmod > Pobs > 0
C
        IF (DDATA(I,J).LE.PREC(I,J)) THEN
C  THIS IS THE ADJUSTMENT WE DO IF WE HAD TOO MUCH PRECIP, CONVECTIVE
C  OR OTHERWISE, IN THE MODEL.  MULTIPLY THE LATENT HEAT
C  AT EACH LEVEL BY THE FRACTION: DATA/MODEL RAINFALL
C  MATCH THE RH THAT THE PROFILE HAD PRIOR TO THIS ADJUSTMENT
C-----------FIND THE PRE-MODIFIED RELATIVE HUMIDITY FOR THIS POINT------
          ADJUST=DDATA(I,J)/PREC(I,J)
        if (i.eq.itstloc .and. j.eq.jtstloc .and. mype.eq.mtstpe) 
     &    write(98,*) 
     &    'Check for Case 2, DDATA=', DDATA(I,J),' PREC=',PREC(I,J),
     &    ' ADJUST=', ADJUST, ' SR=', SR(I,J)
C  Compute the ratio of convective precip/total precip:
          CRATIO=(PREC(I,J)-APREC(I,J))/PREC(I,J)
C
          DO 140 L=1,LM
            IF (HTM(I,J,L).LT.0.5) GO TO 140
            PETAL=PDSL(I,J)*AETA(L)+PT
c           if(i.eq.16.and.j.eq.35.and.mype.eq.13.and.ntsd.eq.26) then
c           print*,'i,j,l,aeta(l),pdsl(i,j)=',i,j,l,aeta(l),pdsl(i,j)
c           endif
            QCKL=PQ0/PETAL
     2       *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
            RELHUM=Q(I,J,L)/QCKL
            OLDRH=RELHUM
            OLDQ=Q(I,J,L)
            OLDCWM=CWM(I,J,L)
C   MODIFY THE TEMP CHANGE AND PRECIP
            T(I,J,L)=T(I,J,L)+TLAT(I,J,L)*(ADJUST-1.)
CYL
CYL Assume ice process below freezing, and water process otherwise.
CYL This is to be consistent with the latent heat calculation in PRECPD.
CYL
            IF (T(I,J,L).GE.273.15) THEN
              ELV=ELWV
            ELSE
              ELV=ELIV
            ENDIF
            DELT=TLAT(I,J,L)*(ADJUST-1.)
C   The following is the 'accum pcp' version of DELT to take care of the
C   fractional time step at the end of each EDAS segment:
            DELTACP=TLAT(I,J,L)*(ADJUST*ENDFCTR-1.)
            PREC(I,J)=DELT*DETA(L)*PDSL(I,J)*CP/(ROW*G*ELV)+PREC(I,J)
            CUPREC(I,J)=DELTACP*DETA(L)*PDSL(I,J)*CP/(ROW*G*ELV)*CRATIO
     2                 +CUPREC(I,J)
            ACPREC(I,J)=DELTACP*DETA(L)*PDSL(I,J)*CP/(ROW*G*ELV)
     2                 +ACPREC(I,J)
             CUPPT(I,J)=DELTACP*DETA(L)*PDSL(I,J)*CP/(ROW*G*ELV)*CRATIO
     2                 +CUPPT(I,J)
C   Reduce RH by the factor PFACTOR:
            QCKL=PQ0/PETAL
     2           *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
            RELHUM= RELHUM * PFACTOR(I,J)
            IF (TLAT(I,J,L).GT.0.) Q(I,J,L)=RELHUM*QCKL
            IF (I.EQ.ITSTLOC .AND. J.EQ.JTSTLOC .AND. MYPE.EQ.MTSTPE)
     2        WRITE(98,128) L, PETAL,TLAT(I,J,L), T(I,J,L), OLDRH, 
     3          RELHUM, OLDQ, Q(I,J,L)
 128        FORMAT(I2,X,f7.0,x,f7.4,X,F7.2,2(2X,F6.4), X, 2(2X,E12.6))
C
C  If the model had grid-scale precip prior to the adjustment, and 
C  the cloud water is above the minimum (WMIN) for generating rain,
C  reduce the cloud water(CWM) proportionally, but keep it above WMIN,
C
            IF (TLAT(I,J,L).GT.0. .and. APREC(I,J).GT.0. 
     2         .and. CWM(I,J,L).GT. WMIN) THEN
              TTEMP=0.025*(T(I,J,L)-273.16)
              WFIX=0.9814*EXP(0.01873*L)
              WMIN=0.1E-3*EXP(TTEMP)*WFIX
c             if(mype.eq.17) then
c               print*,'in 140 loop'
c               print*,'i,j,l=',i,j,l
c               print*,'cwm(i,j,l),wmin=',cwm(i,j,l),wmin
c               print*,'ttemp=',ttemp
c               print*,'t(i,j,l)=',t(i,j,l)
c               print*,'adjust=',adjust
c             endif
              CWM(I,J,L) = AMAX1(WMIN,CWM(I,J,L)*ADJUST)
            ENDIF
c
c  Calculate the water vapor and cloud water/ice increments for
c    1) the entire column
c    2) sfc-700mb
c
            DELQ   = (Q(I,J,L)-OLDQ)     * DETA(L)*PDSL(I,J)/G
            DELCWM = (CWM(I,J,L)-OLDCWM) * DETA(L)*PDSL(I,J)/G
c
            VAPINC(I,J)=VAPINC(I,J)+DELQ
            CLDINC(I,J)=CLDINC(I,J)+DELCWM
C
            IF (PETAL.GE.70000.) THEN
              VAPINC7(I,J)=VAPINC7(I,J)+DELQ
              CLDINC7(I,J)=CLDINC7(I,J)+DELCWM
            ENDIF
 140      CONTINUE
C
C  We didn't adjust APREC yet.  Here we'll reduce APREC proportionally:
          APREC(I,J) = PREC(I,J) * (1.-CRATIO) 
          GO TO 900
        ENDIF
C
C-----------------------------------------------------------------------
C Case 3 ------IF WE ARE HERE, THEN Pmod < Pobs ------------------------
C--------------IF OBSERVED PRECIP IS GREATER THAN ZER-------------------
C--------------DETERMINE IF IT IS CONVECTIVE OR GRID-SCALE--------------
C-----------------------------------------------------------------------
C   GO THROUGH THE BETTS/MILLER/JANJIC CLOUD SEARCH, IF THE CLOUD
C   IS CONSIDERED DEEP, THEN WE HAVE CONVECTION.
C-----------------------------------------------------------------------
C--------------PADDING SPECIFIC HUMIDITY IF TOO SMALL-------------------
C                  RESTORE APE TO SCRATCH ARRAY
        DO 150 L=1,LM
          APESTS=PDSL(I,J)*AETA(L)+PT
          APE(I,J,L)=(1.E5/APESTS)**CAPA
          IF(Q(I,J,L).LT.EPSQ)Q(I,J,L)=HTM(I,J,L)*EPSQ
  150   CONTINUE
C--------------SEARCH FOR MAXIMUM BUOYANCY LEVEL------------------------
        DO 170 KB=1,LM
          IF (HTM(I,J,L).LT.0.5) GO TO 170
C--------------TRIAL MAXIMUM BUOYANCY LEVEL VARIABLES-------------------
          PKL=AETA(KB)*PDSL(I,J)+PT
          LMHK=LMH(I,J)
          PSFCK=AETA(LMHK)*PDSL(I,J)+PT
C
          IF(KB.LE.LMHK .AND. PKL.GE.0.80*PSFCK) THEN
            QBT=Q(I,J,KB)
            TTHBT=T(I,J,KB)*APE(I,J,KB)
            TTH=(TTHBT-THL)*RDTH
            QQ1=TTH-AINT(TTH)
            ITTB=INT(TTH)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
            IF(ITTB.LT.1)THEN
              ITTB=1
              QQ1=0.
            ENDIF
C
            IF(ITTB.GE.JTB)THEN
              ITTB=JTB-1
              QQ1=0.
            ENDIF
C--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY---------------
            ITTBK=ITTB
            BQS00K=QS0(ITTBK)
            SQS00K=SQS(ITTBK)
            BQS10K=QS0(ITTBK+1)
            SQS10K=SQS(ITTBK+1)
C--------------SCALING SPEC. HUMIDITY & TABLE INDEX---------------------
            BQ  = (BQS10K-BQS00K)*QQ1+BQS00K
            SQ  = (SQS10K-SQS00K)*QQ1+SQS00K
            TQ  = (QBT-BQ)/SQ*RDQ
            PP1  =TQ - AINT(TQ)
            IQTB=INT(TQ)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
            IF(IQTB.LT.1)THEN
              IQTB=1
              PP1=0.
            ENDIF
C
            IF(IQTB.GE.ITB)THEN
              IQTB=ITB-1
              PP1=0.
            ENDIF
C--------------SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.-------
            IQ=IQTB
            IT=ITTB
            P00K=PTBL(IQ  ,IT  )
            P10K=PTBL(IQ+1,IT  )
            P01K=PTBL(IQ  ,IT+1)
            P11K=PTBL(IQ+1,IT+1)
C--------------SATURATION POINT VARIABLES AT THE BOTTOM-----------------
            TPSP=P00K+(P10K-P00K)*PP1+(P01K-P00K)*QQ1
     1          +(P00K-P10K-P01K+P11K)*PP1*QQ1
            APESP=(1.E5/TPSP)**CAPA
            TTHES=TTHBT*EXP(ELOCP*QBT*APESP/TTHBT)
C--------------CHECK FOR MAXIMUM BUOYANCY-------------------------------
            IF(TTHES.GT.THESP(I,J))THEN
              PSP  (I,J)=TPSP
              THBT (I,J)=TTHBT
              THESP(I,J)=TTHES
            ENDIF
          ENDIF
C-----------------------------------------------------------------------
  170   CONTINUE
C
C---------CHOOSE CLOUD BASE AS MODEL LEVEL JUST BELOW PSP--------------
C
        DO 190 L=1,LM1
          IF (HTM(I,J,L).LT.0.5) GO TO 190
          AETAL=AETA(L)
          P(I,J)=PDSL(I,J)*AETAL+PT
          IF(P(I,J).LT.PSP(I,J).AND.P(I,J).GE.PQM)LBOT(I,J)=L+1
 190    CONTINUE
C***
C*** WARNING: LBOT MUST NOT BE GT LMH(I,J)-1 IN SHALLOW CONVECTION
C*** MAKE SURE CLOUD BASE IS AT LEAST PONE ABOVE THE SURFACE
C***
        LMHIJ=LMH(I,J)
        PBOT(I,J)=AETA(LBOT(I,J))*PDSL(I,J)+PT
        PSFCK=AETA(LMHIJ)*PDSL(I,J)+PT
C
        IF(PBOT(I,J).GE.PSFCK-PONE.OR.LBOT(I,J).GE.LMHIJ)THEN
          DO 200 L=1,LMHIJ-1
            IF (HTM(I,J,L).LT.0.5) GO TO 200
            P(I,J)=AETA(L)*PDSL(I,J)+PT
            IF(P(I,J).LT.PSFCK-PONE)LBOT(I,J)=L
 200      CONTINUE 
          PBOT(I,J)=AETA(LBOT(I,J))*PDSL(I,J)+PT
        ENDIF 
C--------------CLOUD TOP COMPUTATION------------------------------------
        LTOP(I,J)=LBOT(I,J)
        PTOP(I,J)=PBOT(I,J)
C-----------------------------------------------------------------------
c
        DO 250 L=LM,1,-1
          IF (HTM(I,J,L).LT.0.5) GO TO 250
c
C--------------SCALING PRESSURE & TT TABLE INDEX------------------------
          KNUML=0
          KNUMH=0
C
          PRESK=PDSL(I,J)*AETA(L)+PT
C
          IF(PRESK.LT.PLQ)THEN
            KNUML=KNUML+1
            ILRES(KNUML)=I
            JLRES(KNUML)=J
          ELSE
            KNUMH=KNUMH+1
            IHRES(KNUMH)=I
            JHRES(KNUMH)=J
          ENDIF
C***
C***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE<PL
C**
          IF(KNUML.GT.0)THEN
            CALL TTBLEX(TREF(IDIM1,JDIM1,L),TTBL,ITB,JTB,KNUML
     1,                 ILRES,JLRES,PDSL,AETA(L),HTM(IDIM1,JDIM1,L)
     2,                 PT,PL,QQ(IDIM1,JDIM1),PP(IDIM1,JDIM1)
     3,                 RDP,THE0,STHE,RDTHE
     4,                 THESP(IDIM1,JDIM1),IPTB(IDIM1,JDIM1)
     5,                 ITHTB(IDIM1,JDIM1))
          ENDIF
C***
C***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE>PL
C**
          IF(KNUMH.GT.0)THEN
            CALL TTBLEX(TREF(IDIM1,JDIM1,L),TTBLQ,ITBQ,JTBQ,KNUMH
     1,          IHRES,JHRES,PDSL,AETA(L),HTM(IDIM1,JDIM1,L)
     2,          PT,PLQ,QQ(IDIM1,JDIM1),PP(IDIM1,JDIM1)
     3,          RDPQ,THE0Q,STHEQ,RDTHEQ
     4,          THESP(IDIM1,JDIM1),IPTB(IDIM1,JDIM1)
     5,          ITHTB(IDIM1,JDIM1))
          ENDIF
 250    CONTINUE
C--------------BUOYANCY CHECK-------------------------------------------
        DO 280 L=LM,1,-1
          IF (HTM(I,J,L).LT.0.5) GO TO 280
          IF(TREF(I,J,L).GT.T(I,J,L)-DTTOP)LTOP(I,J)=L
 280    CONTINUE
C-----------------CLOUD TOP PRESSURE------------------------------------
        PTOP(I,J)=AETA(LTOP(I,J))*PDSL(I,J)+PT
C--------------CLEAN UP AND GATHER DEEP CONVECTION POINTS---------------
        IF ((PPTDAT(I,J,IHR).LT.900 .AND. DDATA(I,J).LE.ZER).OR.
     &       PPTDAT(I,J,IHR).LT.ZER) THEN
          LTOP(I,J)=LBOT(I,J)
          PTOP(I,J)=PBOT(I,J)
        ENDIF
        IF(LTOP(I,J).GT.LBOT(I,J))THEN
          LTOP(I,J)=LBOT(I,J)
          PTOP(I,J)=PBOT(I,J)
        ENDIF
        IF(HBM2(I,J).LT.0.90)THEN
          LTOP(I,J)=LBOT(I,J)
          PTOP(I,J)=PBOT(I,J)
        ENDIF
C
C  If the cloud is too shallow for convective precip, go to grid scale.
C
        PSFCIJ=PD(I,J)+PT
        DEPMIN=PSHNEW*PSFCIJ*1.E-5
        DEPTH=PBOT(I,J)-PTOP(I,J)
        if (i.eq.itstloc .and. j.eq.jtstloc .and. mype.eq.mtstpe) 
     &  write(98,*) 'PTOP=',ptop(i,j), ' PBOT=',pbot(i,j),
     &      ' DEPTH=', DEPTH, ' DEPMIN=', DEPMIN
C
        IF(DEPTH .LT. DEPMIN) THEN
          IGSADJ(I,J) = .TRUE.
          ADATA(I,J) = DDATA(I,J)
          GOTO 600
        ENDIF
C***********************************************************************
C************* IF CLOUD IS DEEP ENOUGH THEN ASSUME CONVECTION **********
C************* IS OBSERVED, MAKE CONVECTIVE-TYPE ADJUSTMENT ************
C***********************************************************************
C***********************************************************************
C
C   ESTIMATE THE CHANGE IN EFI, BASIALLY MULTIPLYING CURRENT EFI
C   BY FORECAST PREC/OBS PREC
C
C   Don't worry - if we are here then DDATA > 0.
        FACTOR=PREC(I,J)/DDATA(I,J)
C
C   IF THERE WAS NO FORECAST PRECIP, LEAVE EFI ALONE
C
        IF (PREC(I,J).LE.ZER) FACTOR=1.
        EFINEW=CLDEFI(I,J)*FACTOR
        EFI=CLDEFI(I,J)*FCB+EFINEW*FCC
        IF (EFI.GT.1.0) EFI=1.0
        IF (EFI.LT.0.2) EFI=0.2
        IF (SM(I,J).LT.1.0.AND.DDATA(I,J).LT.CTHRS) EFI=1.0
        CLDEFI(I,J)=EFI
C
C   TAKE BACK ANY LATENT HEAT/PRECIP THAT WAS RELEASED PREVIOUSLY
C   SINCE WE'LL BE ADJUSTING TO THE PROFILE THAT RELEASES HEAT
C   THAT SUMS UP TO THE OBSERVED PRECIP
C
        DO L=1,LM
          IF (HTM(I,J,L).GT.0.5) T(I,J,L)=T(I,J,L)-TLAT(I,J,L)
        ENDDO
C
C   TAKE BACK THE PRECIP TOO
C
        CUPREC(I,J)=CUPREC(I,J)-(PREC(I,J)-APREC(I,J))
         CUPPT(I,J)= CUPPT(I,J)-(PREC(I,J)-APREC(I,J))
        ACPREC(I,J)=ACPREC(I,J)-PREC(I,J)
        PREC  (I,J)=0.
C
        LTPK=LTOP(I,J)
        LBTK=LBOT(I,J)
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
CDCDCDCDCDCDC  DEEP CONVECTION   DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
        LB   =LBTK
        EFI  =CLDEFI(I,J)
C--------------INITIALIZE VARIABLES IN THE CONVECTIVE COLUMN------------
C***
C***  ONE SHOULD NOTE THAT THE VALUES ASSIGNED TO THE ARRAY TREFK
C***  IN THE 410 LOOP ARE REALLY ONLY RELEVANT IN ANCHORING THE
C***  REFERENCE TEMPERATURE PROFILE AT LEVEL LB.  WHEN BUILDING THE
C***  REFERENCE PROFILE FROM CLOUD BASE, THEN ASSIGNING THE
C***  AMBIENT TEMPERATURE TO TREFK IS ACCEPTABLE.  HOWEVER, WHEN
C***  BUILDING THE REFERENCE PROFILE FROM SOME OTHER LEVEL (SUCH AS
C***  ONE LEVEL ABOVE THE GROUND), THEN TREFK SHOULD BE FILLED WITH
C***  THE TEMPERATURES IN TREF(I,J,L) WHICH ARE THE TEMPERATURES OF
C***  THE MOIST ADIABAT THROUGH CLOUD BASE.  BY THE TIME THE LINE 
C***  NUMBERED 450 HAS BEEN REACHED, TREFK ACTUALLY DOES HOLD THE
C***  REFERENCE TEMPERATURE PROFILE.
C***
        DO 410 L=1,LM
          IF (HTM(I,J,L).LT.0.5) GO TO 410
          DIFT  (L)=0.
          DIFQ  (L)=0.
          TKL      =T(I,J,L)
          TK    (L)=TKL
          TREFK (L)=TKL
          QKL      =Q(I,J,L)
          QK    (L)=QKL
          QREFK (L)=QKL
          PKL      =AETA(L)*PDSL(I,J)+PT
          PK    (L)=PKL
          PSK   (L)=PKL
          APEKL    =APE(I,J,L)
          APEK  (L)=APEKL
          THERK (L)=TREF(I,J,L)*APEKL
  410   CONTINUE
C--------------DEEP CONVECTION REFERENCE TEMPERATURE PROFILE------------
cdrun
        LTP1=LTPK+1
        LBM1=LB-1
        PKB=PK(LB)
        PKT=PK(LTPK)
C--------------TEMPERATURE REFERENCE PROFILE BELOW FREEZING LEVEL-------
        L0=LB
        PK0=PK(LB)
        TREFKX=TREFK(LB)
        THERKX=THERK(LB)
        APEKXX=APEK(LB)
        THERKY=THERK(LBM1)
        APEKXY=APEK(LBM1)
C
        DO 420 L=LTPK,LBM1
          IVI=LTPK+LBM1-L
          IF(T(I,J,IVI+1).LT.TFRZ)GO TO 430
          STABDL=STABD
          TREFKX=((THERKY-THERKX)*STABDL
     1              +TREFKX*APEKXX)/APEKXY
          TREFK(IVI)=TREFKX
          APEKXX=APEKXY
          THERKX=THERKY
          APEKXY=APEK(IVI-1)
          THERKY=THERK(IVI-1)
          L0=IVI
          PK0=PK(L0)
  420   CONTINUE
C--------------FREEZING LEVEL AT OR ABOVE THE CLOUD TOP-----------------
        L0M1=L0-1
        GO TO 445
C--------------TEMPERATURE REFERENCE PROFILE ABOVE FREEZING LEVEL-------
  430   L0M1=L0-1
        RDP0T=1./(PK0-PKT)
        DTHEM=THERK(L0)-TREFK(L0)*APEK(L0)
CCCCCCCCCCCCCCCDIR$ SHORTLOOP
        DO 440 L=LTPK,L0M1
          TREFK(L)=(THERK(L)-(PK(L)-PKT)*DTHEM*RDP0T)/APEK(L)
  440   CONTINUE
C-----------------------------------------------------------------------
C------------- ADJUST TEMP PROFILE TO MATCH OBSERVED PPT ---------------
C-----------------------------------------------------------------------
 445    CONTINUE
        PRECMAX=0.
C
        DO L=LTPK,LB
          PRECMAX=PDSL(I,J)*DETA(L)*(TREFK(L)-TK(L))*CPRLG+PRECMAX
        ENDDO
        if (i.eq.itstloc .and. j.eq.jtstloc .and. mype.eq.mtstpe) 
     &     write(98,*)'PRECMAX=', PRECMAX
C
        IF (PRECMAX.LE.0.) THEN
C    SEND THIS TO THE GRID-SCALE ADJUSTMENT
C    NOT ENOUGH POSITIVE AREA TO DO CONVECTIVE ADJUSTMENT
          LTOP(I,J)=LBOT(I,J)-3
          PTOP(I,J)=PBOT(I,J)-2.*PSHNEW
          ADATA(I,J) = DDATA(I,J)
          IGSADJ(I,J) = .TRUE.
          GOTO 600
        ENDIF
C
C    IF THE OBS PRECIP IS GREATER THAN THE MAX POSSIBLE PRECIP
C    (WHICH IS THE AMOUNT YOU'D GET BY GOING ALL THE WAY TO THE
C    REF PROFILE) ONLY DO THE MAX POSSIBLE PRECIP.  SET EFI TO EFIMN
C    IN AN ATTEMPT TO TRY TO GET THE GRID-SCALE PRECIP TO START
C    TAKING OVER
C
        RATIO=DDATA(I,J)/PRECMAX
        IF (RATIO.GT.1.) THEN
          RATIO=1.
          EFI=EFIMN
          CLDEFI(I,J)=EFIMN
          IGSADJ(I,J) = .TRUE.
        ENDIF
C
        DO L=LTPK,LB
          TREFK(L)=TK(L)+RATIO*(TREFK(L)-TK(L))
        ENDDO

C--------------DEEP CONVECTION REFERENCE HUMIDITY PROFILE---------------
C              DEFINE DSPS
        DSPBK=((EFI-EFIMN)*SLOPBS+DSPBSS)*SM(I,J)
     1       +((EFI-EFIMN)*SLOPBL+DSPBSL)*(1.-SM(I,J))
        DSP0K=((EFI-EFIMN)*SLOP0S+DSP0SS)*SM(I,J)
     1       +((EFI-EFIMN)*SLOP0L+DSP0SL)*(1.-SM(I,J))
        DSPTK=((EFI-EFIMN)*SLOPTS+DSPTSS)*SM(I,J)
     1       +((EFI-EFIMN)*SLOPTL+DSPTSL)*(1.-SM(I,J))
cccccccccccccccccCDIR$ SHORTLOOP
 450    CONTINUE
        DEPTH=PFRZ*PSFCIJ*1.E-5
        DEPWL=PKB-PK0
        DO 460 L=LTPK,LB
C--------------SATURATION PRESSURE DIFFERENCE---------------------------
          IF(DEPWL .GE. DEPTH) THEN 
            IF(L.LT.L0)THEN
              DSP=((PK0-PK(L))*DSPTK+(PK(L)-PKT)*DSP0K)/(PK0-PKT)
            ELSE
              DSP=((PKB-PK(L))*DSP0K+(PK(L)-PK0)*DSPBK)/(PKB-PK0)
            ENDIF
          ELSE
            DSP=DSP0K
            IF(L.LT.L0)
     1        DSP=((PK0-PK(L))*DSPTK+(PK(L)-PKT)*DSP0K)/(PK0-PKT)
          ENDIF
C--------------HUMIDITY PROFILE-----------------------------------------
          IF(PK(L).GT.PQM)THEN
            PSK(L)=PK(L)+DSP
            APESK(L)=(1.E5/PSK(L))**CAPA
            THSK(L)=TREFK(L)*APEK(L)
            QREFK(L)=PQ0/PSK(L)*EXP(A2*(THSK(L)-A3*APESK(L))
     1                                  /(THSK(L)-A4*APESK(L)))
          ELSE
            QREFK(L)=Q(I,J,L)
          ENDIF
  460   CONTINUE
C--------------HEATING, MOISTENING, PRECIPITATION-----------------------
        PRECK =0.
cccccccccccccccccccccccCDIR$ SHORTLOOP
        DO 530 L=LTPK,LB
          PRECK  =DETA(L)*(TREFK(L)-TK(L))+PRECK
  530   CONTINUE
C
C--------------UPDATE PRECIPITATION, TEMPERATURE & MOISTURE-------------
C
        PREC  (I,J)=PDSL(I,J)*PRECK*CPRLG+PREC  (I,J)
        CUPREC(I,J)=PDSL(I,J)*PRECK*CPRLG*ENDFCTR + CUPREC(I,J)
         CUPPT(I,J)=PDSL(I,J)*PRECK*CPRLG*ENDFCTR +  CUPPT(I,J)
        ACPREC(I,J)=PDSL(I,J)*PRECK*CPRLG*ENDFCTR + ACPREC(I,J)
        ADATA(I,J)=DDATA(I,J)-PDSL(I,J)*PRECK*CPRLG
        APREC(I,J) = 0.
        if (i.eq.itstloc .and. j.eq.jtstloc .and. mype.eq.mtstpe) 
     &    write(98,*) 'After deep conv, PREC=',PREC(I,J),' CUPREC=',
     &     CUPREC(I,J),' ACPREC=',ACPREC(I,J),' ADATA=',ADATA(I,J)
C
        DO 580 L=LTPK,LB
C  Calculate the relative humidity before the T and Q update:
          PETAL=PDSL(I,J)*AETA(L)+PT
          QCKL=PQ0/PETAL
     2        *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
          RELHUM=Q(I,J,L)/QCKL
C
          OLDRH=RELHUM
          OLDQ=Q(I,J,L)
C
          T(I,J,L)=TREFK(L)
          Q(I,J,L)=QREFK(L)
C
C  Increase RH by factor PFACTOR, but keep it under 90%:
C
          QCKL=PQ0/PETAL
     2        *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
C
          RELHUM= AMIN1(0.80, RELHUM*PFACTOR(I,J))
          Q(I,J,L)=RELHUM*QCKL
C
c  Calculate the water vapor increment for
c    1) the entire column
c    2) sfc-700mb
c
          DELQ = (Q(I,J,L)-OLDQ) * DETA(L)*PDSL(I,J)/G
          VAPINC(I,J)=VAPINC(I,J)+DELQ
C
          IF (PETAL.GE.70000.) THEN
            VAPINC7(I,J)=VAPINC7(I,J)+DELQ
          ENDIF

C tlat(l) in the following print is meaningless. Included so as to be
C consistent with the earlier prints (for Pobs < Pmod)
          IF (MYPE.EQ.MTSTPE .AND. I.EQ.ITSTLOC .AND. J.EQ.JTSTLOC)
     2 WRITE(98,129) L,PETAL,TLAT(I,J,L), OLDRH, PFACTOR(I,J),
     3   RELHUM, OLDQ, Q(I,J,L)
  580   CONTINUE
C
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
CDCDCDCDCDCDC          END OF DEEP CONVECTION            DCDCDCDCDCDCDCD
CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
C
C-----------------------------------------------------------------------
  600   CONTINUE
C-----------------------------------------------------------------------
C--------------GATHER GRID-SCALE PRECIP ADJUSTMENT POINTS---------------
C-----------------------------------------------------------------------
C
        IF(.NOT.IGSADJ(I,J)) GO TO 900
C
C***********************************************************************
C
        LMHK=LMH(I,J)
C
        IF (APREC(I,J).GT.ZER.AND.ADATA(I,J).GT.ZER) THEN
C
C  THIS IS THE ADJUSTMENT WE DO IF WE HAVE RAIN BOTH IN THE
C  DATA AND IN THE MODEL, MULTIPLY THE LATENT HEAT
C  AT EACH LEVEL BY THE FRACTION: DATA/MODEL RAINFALL
C  WHILE NOT CHANGING DELTA Q
C    THE Q CHANGE IS IMPLICIT, INCREASING Q 
C    AND THEN REMOVING IT VIA CONDENSATION.
C  MATCH THE RH THAT THE PROFILE HAD PRIOR TO THIS ADJUSTMENT
C  IF THE RATIO OF OBS PPT TO PREC(K) IS > 10, SEND IT TO THE
C  PARABOLIC PROFILE PART OF THIS ROUTINE
C  
C  Near the top of the model, the cirrus clouds produce a not-insignificant
C  amount of snow.  Whether this is physically true is debatable (the ice
C  crystals dropping from these cirri play a role, to be sure, but it should
C  be more in the sense of a catalyst (seeder-feeder mechanism) than in terms
C  of _amount_ of snow.  Anyway, if we increase the TLAT and Q at these levels
C  we could be in trouble.  Let's only do the adjustment below 200mb.
C
          ADJUST=ADATA(I,J)/APREC(I,J)
          if (i.eq.itstloc .and. j.eq.jtstloc .and. mype.eq.mtstpe)
     &      write(98,*) 'adjust=', adjust
          IF (ADJUST.LE.10.0) THEN
C-----------FIND THE PRE-MODIFIED RELATIVE HUMIDITY FOR THIS POINT------
            DO 640 L=1,LMHK
              IF (HTM(I,J,L).LT.0.5) GO TO 640
              PETAL=PDSL(I,J)*AETA(L)+PT
              IF (PETAL.LE.20000.) go to 640
              QCKL=PQ0/PETAL
     2            *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
              RELHUM=Q(I,J,L)/QCKL
              OLDQ=Q(I,J,L)
              OLDCWM=CWM(I,J,L)
C   MODIFY THE TEMP CHANGE AND PRECIP
              T(I,J,L)=T(I,J,L)+TLAT(I,J,L)*(ADJUST-1.)
CYL           IF (T(I,J,L).GT.258.) THEN
              IF (T(I,J,L).GT.258. .and. SR(I,J).LT.0.9) THEN
                ELV=ELWV-DLDT*(T(I,J,L)-A3)
              ELSE
                ELV=ELIV
              END IF
              DELT=TLAT(I,J,L)*(ADJUST-1.)
C   The following is the 'accum pcp' version of DELT to take care of the
C   fractional time step at the end of each EDAS segment:
              DELTACP=TLAT(I,J,L)*(ADJUST*ENDFCTR-1.)
              PREC(I,J)=DELT*DETA(L)*PDSL(I,J)*CP/(ELV*ROW*G)
     2                 +PREC(I,J)
              APREC(I,J)=DELTACP*DETA(L)*PDSL(I,J)*CP/(ELV*ROW*G)
     2                  +APREC(I,J)
              ACPREC(I,J)=DELTACP*DETA(L)*PDSL(I,J)
     2                    *CP/(ELV*ROW*G) + ACPREC(I,J)
C   SET THE RH TO BE THE SAME AS IT WAS BEFORE THE LATENT HEAT MODIFI.
              QCKL=PQ0/PETAL * EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
              RELHUM= RELHUM * PFACTOR(I,J)
              Q(I,J,L)=RELHUM*QCKL
C
C Cloud adjustment for grid-scale precip: increase/decrease
C the cloud water mixing ratio proportionally (take care not to go below
C the minimum CWM to produce rain).  If the precip is convective,
C not adjusting cloud.
C
C Cloud adjustment.
C First, calculate minimum cloud water for rain production.  Note: we're
C only doing the adjustment in levels where TLAT > 0, i.e. where rain was
C produced.
C
C 2002/10/11: only do the cloud adjustment if ADJUST < 1 (i.e. to only reduce 
C   cloud, not to increase it.  Remember that CWM is cloud water+cloud ice.
C   If we just increase CWM proportionally, we could be increasing cloud ice
C   by nearly 10 fold right underneath 200mb (10 fold is the cap).  
C   Much more prudent to not increase CWM at all, just make sure it's above
C   the minimum required to form precip.
C
              IF (TLAT(I,J,L).GT.0. .and. ADJUST.LT.1.) THEN
                TTEMP=0.025*(T(I,J,L)-273.16)
                WFIX=0.9814*EXP(0.01873*L)
                WMIN=0.1E-3*EXP(TTEMP)*WFIX
c             if(mype.eq.17) then
c               print*,'in 640 loop'
c               print*,'i,j,l=',i,j,l
c               print*,'cwm(i,j,l),wmin=',cwm(i,j,l),wmin
c               print*,'ttemp=',ttemp
c               print*,'t(i,j,l)=',t(i,j,l)
c               print*,'adjust=',adjust
c             endif
                CWM(I,J,L) = AMAX1(WMIN,CWM(I,J,L)*ADJUST)
              ENDIF
c
c  Calculate the water vapor and cloud water/ice increments for
c    1) the entire column
c    2) sfc-700mb
c
              DELQ   = (Q(I,J,L)-OLDQ)     * DETA(L)*PDSL(I,J)/G
              DELCWM = (CWM(I,J,L)-OLDCWM) * DETA(L)*PDSL(I,J)/G
c 
              VAPINC(I,J)=VAPINC(I,J)+DELQ
              CLDINC(I,J)=CLDINC(I,J)+DELCWM
C
              IF (PETAL.GE.70000.) THEN
                VAPINC7(I,J)=VAPINC7(I,J)+DELQ
                CLDINC7(I,J)=CLDINC7(I,J)+DELCWM
              ENDIF
 640        CONTINUE
          ENDIF
        ENDIF
C
        IF ((APREC(I,J).LE.ZER.AND.ADATA(I,J).GT.ZER)
     &     .OR.ADJUST.GT.10.) THEN
C   FINALLY, IF THE MODEL SAYS THAT NO RAIN OCCURED WHILE THE
C   DATA SAYS THAT WE GOT SOMETHING,
C   OR IF THE RATIO OF OBS PRECIP TO FORECASTED PRECIP IS > 10
C   THE FOLLOWING OCCURS
C   WE SPECIFY A PARABOLIC LATENT HEAT PROFILE
C   AND COLLECT THE APPROPRIATE AMOUNT OF RAIN FROM THIS HEAT PROFILE
C   WE ARE GOING TO INCREASE Q IN THE HEATED LAYERS SO THE RH WILL BE
C   80%.  THIS SHOULD HELP TO GET SOME MODEL PRECIP THE NEXT TIMESTEP
C   CLOUD BASE IS THE FIRST LEVEL ABOVE GROUND WHERE RH>80%
C   CLOUD TOP IS THE FIRST LEVEL ABOVE CLOUD BASE WHERE RH<80%
C    IF THIS CLOUD IS TOO SHALLOW, THEN SPECIFY A X MB CLOUD
C    X MB ABOVE GROUND, DEPENDING UPON PPT RATE
C-----------------------------------------------------------------------
          PSFC=PD(I,J)+PT
          FIWL1=0.
          CLIMIT=1.E-20
C
          DO 650 L=1,LMHK
            IF (HTM(I,J,L).LT.0.5) GO TO 650
            TMT0=(T(I,J,L)-273.16)
            TMT15=AMIN1(TMT0,-15.)
            AI=0.008855
            BI=1.
            IF(TMT0.LT.-20.)THEN
              AI=0.007225
              BI=0.9674
            ENDIF
            QW=PQ0/(PDSL(I,J)*AETA(L)+PT)                              
     1                 *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))                  
            QI=QW*(BI+AI*AMIN1(TMT0,0.))                                 
            QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))                          
            IF(TMT0.LE.-40.)QINT=QI
C-------------------ICE-WATER ID NUMBER IW------------------------------
            IF(TMT0.LT.-15.)THEN                                             
              FI=Q(I,J,L)-0.75*QI
              IF(FI.GT.0.0.OR.CWM(I,J,L).GT.CLIMIT) THEN                    
                FIW=1.
              ELSE                                                           
                FIW=0.
              ENDIF                                                         
            ENDIF                                                            
            IF(TMT0.LT.0.0.AND.TMT0.GE.-15.0)THEN                            
              FIW=0.                                                  
              IF(FIWL1.GT.0.0.AND.CWM(I,J,L).GT.CLIMIT)FIW=1.
            ENDIF                                                            
            IF(TMT0.GE.0.)THEN                                               
              FIW=0.                                                     
            ENDIF                                                            
            QC(L)=(1.-FIW)*QINT+FIW*QI
            FIWL1=FIW
            RELH(L)=Q(I,J,L)/QC(L)
 650      CONTINUE
C
          PBOT=0.
          PTOP=0.
C
          DO 660 L=LMHK,2,-1
            IF (HTM(I,J,L).LT.0.5) GO TO 660
            PETAL=PDSL(I,J)*AETA(L)+PT
            IF (PETAL.LT.20000.) go to 660
            IF(PBOT(I,J).EQ.0.0.AND.RELH(L).GE.0.80) THEN
              PBOT(I,J)=PDSL(I,J)*AETA(L)+PT
              PTOP(I,J)=PBOT(I,J)
            ENDIF
            IF(PBOT(I,J).GT.0.0.AND.PTOP(I,J).EQ.PBOT(I,J)
     &        .AND. RELH(L).LT.0.80) PTOP(I,J)=PDSL(I,J)*AETA(L)+PT
 660      CONTINUE
C
          IF (PBOT(I,J)-PTOP(I,J).LT.20000.0) THEN
C
C  CLOUD SEARCH BASED UPON RH FAILED TO PRODUCE A SIGNIFICANT CLOUD
C  SO SPECIFIY CLOUD FROM PRECIP RATE
C
C   the following have been specified before loop 900:
C            PTRES1=2.81E-03*SIXSIX
C            PTRES2=3.75E-04*SIXSIX
C            PTRES3=1.0E-03*SIXSIX
C
C THIS IS THE THRESHOLD VALUE FOR DETERMINING THE DEPTH OF CLOUD 
C TO BE HEATED (OR WHETHER TO INCREASE RH IN Q ENHANCEMENT)
C
C CLOUD BASE IS 150 MB ABOVE SURFACE FOR ALL CLOUDS
C
            PBOT(I,J)=PSFC-15000.
C
C CLOUD TOP IS AT 200 MB FOR INTENSE PRECIP
C
            IF (ADATA(I,J).GE.PTRES1) PTOP(I,J)=20000.
C
C CLOUD DEPTH IS 450 MB FOR MODERATE PRECIP
C (HIGHEST CLOUD TOP ALLOWED IS AT 200 MB)
C
            IF (ADATA(I,J).GE.PTRES2.AND.ADATA(I,J).LT.PTRES1) THEN
              PTOP(I,J)=PBOT(I,J)-45000.
              IF (PTOP(I,J).LT.20000.) PTOP(I,J)=20000.
            END IF
C
C CLOUD DEPTH IS 300 MB FOR LIGHT PRECIP
C (HIGHEST CLOUD TOP ALLOWED IS AT 200 MB)
C
            IF (ADATA(I,J).LT.PTRES2) THEN
              PTOP(I,J)=PBOT(I,J)-30000.
              IF (PTOP(I,J).LT.20000.) PTOP(I,J)=20000.
            ENDIF
          ENDIF
C
C FIND LAYERS JUST ABOVE PTOP AND PBOT
C
          DO 670 L=1,LM
            IF (HTM(I,J,L).LT.0.5) GO TO 670
            PK(L)=PDSL(I,J)*AETA(L)+PT
            IF (PK(L).LT.PBOT(I,J)) LCBOT=L
            IF (PK(L).LT.PTOP(I,J)) LCTOP=L
 670      CONTINUE
C
          NUMLEV=LCBOT-LCTOP+1
          PREC1=(ADATA(I,J)-APREC(I,J))/NUMLEV
          DETACL=0.0
C
          DO 680 L=LCTOP,LCBOT
            DETACL=DETACL+DETA(L)
 680      CONTINUE
C
C THIS VERSION HAS A PARABOLIC PROFILE OF PRECIP
C WHICH ALLOWS FOR A CHANGE IN LATENT HEATING WITH
C TEMPERATURE, ESPECIALLY NEAR THE FREEZING LEVEL
C
C I DEFINE THE ETATOP AND ETABOT TO BE THE INTERFACIAL
C LAYERS OF THE CLOUD OUTSIDE THE ACTUAL AETA(LCTOP)
C AND AETA(LCBOT)
C
          ETATOP=AETA(LCTOP)-DETA(LCTOP)/2.0
          ETABOT=AETA(LCBOT)+DETA(LCBOT)/2.0
          DO 690 L=LCTOP,LCBOT
            IF (T(I,J,L).GT.258.) THEN
              ELV=ELWV-DLDT*(T(I,J,L)-A3)
            ELSE
              ELV=ELIV
            END IF
            OLDQ=Q(I,J,L)
            ETBIG=AETA(L)*AETA(L)-(ETATOP+ETABOT)*AETA(L)+ETABOT*ETATOP
            PRECL(I,J)=-6.0*PREC1*ETBIG/
     &                   ((ETATOP-ETABOT)*(ETATOP-ETABOT))      
            DELT=PRECL(I,J)*G*ROW*ELV/(CP*DETA(L)*PDSL(I,J))
            T(I,J,L)=DELT+T(I,J,L)
            PREC(I,J)=DELT*DETA(L)*PDSL(I,J)*CP/(ELV*ROW*G)+PREC(I,J)
            ACPREC(I,J)=DELT*DETA(L)*PDSL(I,J)*CP/
     &                   (ELV*ROW*G)*ENDFCTR + ACPREC(I,J)
            APREC(I,J)=DELT*DETA(L)*PDSL(I,J)*CP/
     &                   (ELV*ROW*G)*ENDFCTR + APREC(I,J) 
         IF (I.EQ.ITSTLOC .AND. J.EQ.JTSTLOC .AND. MYPE.EQ.MTSTPE) THEN
               yltmp = DELT*DETA(L)*PDSL(I,J)*CP/(ELV*ROW*G)
               write(98,689) L, etbig,precl(i,j),yltmp, 
     &           aprec(i,j), prec(i,j)
 689           format(i2,3x,5(2x,e12.6))
            endif
C
C  KICK THE RH UP TO 80% IN THE HEATED LAYERS IF THE OBS PRECIP
C  RATE IS MORE THAN 1.0 MM/HR
            IF (ADATA(I,J).GE.PTRES3) THEN
              QC(L)=HTM(I,J,L)*PQ0/(PDSL(I,J)*AETA(L)+PT)
     2             *EXP(HTM(I,J,L)*A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
              Q(I,J,L)=0.80*QC(L)
            END IF
C
c
c  Calculate the water vapor increment for
c    1) the entire column
c    2) sfc-700mb
c
            DELQ   = (Q(I,J,L)-OLDQ)     * DETA(L)*PDSL(I,J)/G
            VAPINC(I,J)=VAPINC(I,J)+DELQ
            IF (PETAL.GE.70000.) THEN
              VAPINC7(I,J)=VAPINC7(I,J)+DELQ
            ENDIF

 690      CONTINUE
CYL If below this newly added layer of cloud, the air is too dry, the
c   added rain will quickly evaporate, leading to rapid cooling (could 
c   be 3 degs in one timestep in PRECPD), and the shock might lead to
c   a blowup.  So we need to moisten the air below the cloud layer too.
c   We first tried setting it to 80%, minimum (80% or the original RH, 
c   whichever is greater), but that proved to be too much.  So now
c   I'm just moistening (when necessary) the three layers underneath 
c   the cloud base - 80% at LCBOT+1, 70% at LCBOT+2, and 60% at LCBOT+3.
c
c   The cloud base shouldn't go below the ground surface.
c
          LCBOTTM = MIN0(LMHK,LCBOT+3)
          DO 700 L = LCBOT+1, LCBOTTM
            IF (ADATA(I,J).GE.PTRES3) THEN
              OLDQ=Q(I,J,L)
              RHFCTR = 1. - 0.1*FLOAT(L-LCBOT+1)
              QC(L)=HTM(I,J,L)*PQ0/(PDSL(I,J)*AETA(L)+PT)
     2             *EXP(HTM(I,J,L)*A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
              Q(I,J,L)=AMAX1(Q(I,J,L),RHFCTR*QC(L))
C
c  Calculate the water vapor increment for
c    1) the entire column
c    2) sfc-700mb
c
              DELQ = (Q(I,J,L)-OLDQ) * DETA(L)*PDSL(I,J)/G
              VAPINC(I,J)=VAPINC(I,J)+DELQ
C
              IF (PETAL.GE.70000.) THEN
                VAPINC7(I,J)=VAPINC7(I,J)+DELQ
              ENDIF
            END IF
 700      CONTINUE
C
C Cloud adjustment: for the layer of cloud we specified, 
C we set cloud water mixing ratio to WMIN or the original CWM, which 
C ever is greater.
C
          DO 710 L = LCTOP, LCBOT
            OLDCWM=CWM(I,J,L)
            TTEMP=0.025*(T(I,J,L)-273.16)
            WFIX=0.9814*EXP(0.01873*L)
            WMIN=0.1E-3*EXP(TTEMP)*WFIX
c             if(mype.eq.17) then
c               print*,'in 710 loop'
c               print*,'i,j,l=',i,j,l
c               print*,'cwm(i,j,l),wmin=',cwm(i,j,l),wmin
c               print*,'ttemp=',ttemp
c               print*,'t(i,j,l)=',t(i,j,l)
c           endif
            CWM(I,J,L) = AMAX1(WMIN,CWM(I,J,L))
c
c  Calculate the cloud water/ice increment for
c    1) the entire column
c    2) sfc-700mb
c
            DELCWM = (CWM(I,J,L)-OLDCWM) * DETA(L)*PDSL(I,J)/G
            CLDINC(I,J)=CLDINC(I,J)+DELCWM
C
            IF (PETAL.GE.70000.) THEN
              CLDINC7(I,J)=CLDINC7(I,J)+DELCWM
            ENDIF
 710      CONTINUE
C
        ENDIF
C
C-----------------------------------------------------------------------
C***********************************************************************
C*******END OF HORIZONTAL LOOP FOR GRID-SCALE TYPE ADJUSTMENT **********
C***********************************************************************
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C--------------SAVE CLOUD TOP AND BOTTOM FOR RADIATION------------------
        HTOP(I,J)=MIN(FLOAT(LTOP(I,J)),HTOP(I,J))
        HBOT(I,J)=MAX(FLOAT(LBOT(I,J)),HBOT(I,J))
C***********************************************************************
C
 900  CONTINUE
 910  CONTINUE
C
C Zero out latent heat array to be ready for the next round of tracking/
C adjustments:
C
      TLAT = 0.0
C
      IF (MYPE.EQ.MTSTPE) THEN
        WRITE(98,*) ' AT END OF ADJPPT, PREC=',PREC(ITSTLOC,JTSTLOC),
     &   ' APREC=',APREC(ITSTLOC,JTSTLOC),
     &   ' ACPREC=',ACPREC(ITSTLOC,JTSTLOC), 
     &   ' CUPREC=', CUPREC(ITSTLOC,JTSTLOC)
        WRITE(98,*)
      ENDIF
C
      RETURN
      END
