C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                        SUBROUTINE READ_NHB(NHB)
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .

C SUBPROGRAM:    READ_NHB    READ AND DISTRIBUTE NHB FILE
C   PRGRMMR: BLACK           ORG: W/NP2      DATE: 98-10-22
C
C ABSTRACT:
C     READ_NHB READS IN QUANTITIES FROM THE NHB FILE AND THEN
C     DISTRIBUTES THEM TO THE OTHER NODES/PEs AS NEEDED
C
C PROGRAM HISTORY LOG:
C   97-??-??  MEYS       - ORIGINATOR
C   97-08-??  BLACK      - REWROTE FOR BENCHMARK
C   98-??-??  TUCCILLO   - MODIFIED FOR SINGLE OR DOUBLE PRECISION
C
C USAGE: CALL READ_NHB FROM SUBROUTINE INIT
C   INPUT ARGUMENT LIST:
C       NHB: FILE NUMBER OF THE NHB FILE
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
C                  CLDWTR
C                  CNVCLD
C                  SOIL
C                  INDX
C    
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM SP
C$$$  
C
C-----------------------------------------------------------------------
C     INCLUDE/SET PARAMETERS.
C-----------------------------------------------------------------------
      INCLUDE "PARMETA.comm"
      INCLUDE "PARMTBL.comm"
      INCLUDE "parmsoil"
C-----------------------------------------------------------------------
                              P A R A M E T E R
     & (G=9.8,CM1=2937.4,CM2=4.9283,CM3=23.5518,EPS=0.622
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
!                             P A R A M E T E R
!    & (IMJM=IM*JM-JM/2,JMP1=JM+1,JAM=6+2*(JM-10),LB=2*IM+JM-3
!    &, LM1=LM-1,LP1=LM+1,IMT=2*IM-1)
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
      INCLUDE "CLDWTR.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
      INCLUDE "mpif.h"
      include "my_comm.h"
      INCLUDE "mpp.h"
!#include "sp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
C
!#ifdef DP_REAL
!      INTEGER*8 ITEMPX(IM,JM)
!      INTEGER*8 NFCSTX,NBCX,LISTX,IDTADX
!      INTEGER*8 KHLAX,KHHAX,KVLAX,KVHAX,KHL2X,KHH2X,KVL2X,KVH2X
!      INTEGER*8 IXMX,IYMX
!      LOGICAL*8 SIGMAX
!#endif
C-----------------------------------------------------------------------
C     
C     DECLARE NAMELIST.
C
      NAMELIST /FCSTDATA/
     & TSTART,TEND,TCP,RESTRT,SUBPOST,NMAP,TSHDE,SPL
     &,NPHS,NCNVC,NRADSH,NRADLH
     &,TPREC,THEAT,TCLOD,TRDSW,TRDLW,TSRFC
C
       real(4),pointer::save_full_sst(:),save_full_sm(:)
       common/full_sst_sm/save_full_sst,save_full_sm
C***********************************************************************
      IF(MYPE.EQ.0)THEN
!#ifdef DP_REAL
!        READ(NHB)
!     1     NFCSTX,NBCX,LISTX
!     2,    DT,IDTADX,SIGMAX
!     3,    KHLAX,KHHAX,KVLAX,KVHAX,KHL2X,KHH2X,KVL2X,KVH2X
!        NFCST=NFCSTX
!        NBC=NBCX
!        LIST=LISTX
!        IDTAD=IDTADX
!        SIGMA=SIGMAX
!        KHLA=KHLAX
!        KHHA=KHHAX
!        KVLA=KVLAX
!        KVHA=KVHAX
!        KHL2=KHL2X
!        KHH2=KHH2X
!        KVL2=KVL2X
!        KVH2=KVH2X
!#else
        READ(NHB)
     1     NFCST,NBC,LIST
     2,    DT,IDTAD,SIGMA
     3,    KHLA,KHHA,KVLA,KVHA,KHL2,KHH2,KVL2,KVH2
!#endif
c       WRITE(0,*)'READ NFCST= ',NFCST,' FROM FILE NHB'
           idtad=1                                ! these changes suggested by
           factr=real(idtad)/2.                   ! zavisha for adjustment and advection done together
      ENDIF
C
      CALL MPI_BCAST(NFCST,1,MPI_INTEGER,0,my_comm,IRTN)
      CALL MPI_BCAST(NBC,1,MPI_INTEGER,0,my_comm,IRTN)
      CALL MPI_BCAST(LIST,1,MPI_INTEGER,0,my_comm,IRTN)
      CALL MPI_BCAST(DT,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(IDTAD,1,MPI_INTEGER,0,my_comm,IRTN)
      CALL MPI_BCAST(SIGMA,1,MPI_LOGICAL,0,my_comm,IRTN)
C
      CALL MPI_BARRIER(my_comm,IRTN)
C
      LIST=6
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE LMH
C***
      IF(MYPE.EQ.0)THEN
!#ifdef DP_REAL
!        READ(NHB)ITEMPX
!        ITEMP=ITEMPX
!#else
        READ(NHB)ITEMP
!#endif
      ENDIF
c     WRITE(0,*)'READ LMH'
C
      CALL IDSTRB(ITEMP,LMH)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE LMV
C***
      IF(MYPE.EQ.0)THEN
!#ifdef DP_REAL
!        READ(NHB)ITEMPX
!        ITEMP = ITEMPX
!#else
        READ(NHB)ITEMP
!#endif
      ENDIF
c     WRITE(0,*)'READ LMV'
C
      CALL IDSTRB(ITEMP,LMV)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE HBM2
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ HBM2'
      ENDIF
C
      CALL DSTRB(TEMP1,HBM2,1,1,1)
C-----------------------------------------------------------------------
C***
C***  FILL HBM3 ON EACH PE
C***
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        HBM3(I,J)=0.
      ENDDO
      ENDDO
C
      DO J=MYJS,MYJE
        JG=J+MY_JS_GLB-1
        IF(JG.GE.4.AND.JG.LE.JM-3)THEN
          IHL=2-IHWG(JG)
          IHH=IM-2
          DO I=MYIS,MYIE
            IG=I+MY_IS_GLB-1
            IF(IG.GE.IHL.AND.IG.LE.IHH)HBM3(I,J)=1.
          ENDDO
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE VBM2
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ VBM2'
      ENDIF
C
      CALL DSTRB(TEMP1,VBM2,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE VBM3
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ VBM3'
      ENDIF
C
      CALL DSTRB(TEMP1,VBM3,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE SM
C***
!   save a copy of sm on full grid for sat data processing
       allocate(save_full_sm(im*jm))
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ SM'

       ii=0
       do j=1,jm
        do i=1,im
         ii=ii+1
         save_full_sm(ii)=temp1(i,j)
        end do
       end do
      ENDIF
      call mpi_bcast(save_full_sm,im*jm,mpi_real4,0,
     *               my_comm,irtn)
C
      CALL DSTRB(TEMP1,SM,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE SICE
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ SICE'
      ENDIF
C
      CALL DSTRB(TEMP1,SICE,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE HTM
C***
      DO L=1,LM
        IF(MYPE.EQ.0)THEN
          READ(NHB)TEMP1
c         WRITE(0,*)'READ HTM'
        ENDIF
        CALL DSTRB(TEMP1,HTM,1,LM,L)
      ENDDO
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE VTM
C***
      DO L=1,LM
        IF(MYPE.EQ.0)THEN
          READ(NHB)TEMP1
c         WRITE(0,*)'READ VTM'
        ENDIF
        CALL DSTRB(TEMP1,VTM,1,LM,L)
      ENDDO
C-----------------------------------------------------------------------
C
      IF(MYPE.EQ.0)THEN
        READ(NHB)DY,CPGFV,EN,ENT,R,PT,TDDAMP
     1,            F4D,F4Q,EF4T,DETA,RDETA,AETA,F4Q2,ETA,DFL
     2,            EM,EMT
c       WRITE(0,*)'READ DY'
      en=en*factr                 ! part added by 
      ent=ent*factr               !  zavisha
      do n=1,jam                  !   to account for
        em(n)=em(n)*factr         !   adjustment and
        emt(n)=emt(n)*factr       !   advection steps 
      enddo                       !   done together
      f4q=f4q*factr               !
      f4d=f4d*factr               !
      do l=1,lm                   !
        f4q2(l)=f4q2(l)*factr     !
      enddo                       !
      ENDIF
C
      CALL MPI_BCAST(DY,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(CPGFV,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(EN,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(ENT,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(R,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(PT,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(TDDAMP,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(F4D,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(F4Q,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(EF4T,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(DETA(1),LM,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(RDETA(1),LM,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(AETA(1),LM,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(F4Q2(1),LM,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(ETA(1),LP1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(DFL(1),LP1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(EM(1),JAM,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(EMT(1),JAM,MPI_REAL,0,my_comm,IRTN)
C
      CALL MPI_BARRIER(my_comm,IRTN)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE DX
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ DX'
      ENDIF
C
      CALL DSTRB(TEMP1,DX,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE WPDAR
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ WPDAR'
      ENDIF
C
      CALL DSTRB(TEMP1,WPDAR,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE CPGFU
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ CPGFU'
      ENDIF
C
      CALL DSTRB(TEMP1,CPGFU,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE CURV
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ CURV'
      ENDIF
C
      CALL DSTRB(TEMP1,CURV,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE FCP
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ FCP'
      ENDIF
C
      CALL DSTRB(TEMP1,FCP,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE FDIV
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ FDIV'
      ENDIF
C
      CALL DSTRB(TEMP1,FDIV,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE FAD
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ FAD'
      do j=1,jm                         !  modification suggested
      do i=1,im                         !    by zavisha for
        temp1(i,j)=temp1(i,j)*factr     !    adjustment and advection
      enddo                             !    done together
      enddo                             !
      ENDIF
C
      CALL DSTRB(TEMP1,FAD,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE F
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ F'
      ENDIF
C
      CALL DSTRB(TEMP1,F,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE DDMPU
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ DDMPU'
      ENDIF
C
      CALL DSTRB(TEMP1,DDMPU,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE DDMPV
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ DDMPV'
      ENDIF
C
      CALL DSTRB(TEMP1,DDMPV,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE PT, GLAT
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)PT2,TEMP1
c       WRITE(0,*)'READ PT, GLAT'
      ENDIF
C
      CALL DSTRB(TEMP1,GLAT,1,1,1)
      CALL MPI_BCAST(PT2,1,MPI_REAL,0,my_comm,IRTN)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE GLON
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ GLON'
      ENDIF
C
      CALL DSTRB(TEMP1,GLON,1,1,1)
C-----------------------------------------------------------------------
C
      IF(MYPE.EQ.0)THEN
        READ(NHB)PLQ,RDPQ,RDTHEQ,STHEQ,THE0Q
c       WRITE(0,*)'READ PLQ'
      ENDIF
C
      CALL MPI_BCAST(PLQ,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(RDPQ,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(RDTHEQ,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(STHEQ(1),ITBQ,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(THE0Q(1),ITBQ,MPI_REAL,0,my_comm,IRTN)
C
      CALL MPI_BARRIER(my_comm,IRTN)
C-----------------------------------------------------------------------
C
      IF(MYPE.EQ.0)THEN
        READ(NHB)ROS,CS,DS,ROI,CI,DI
     1,            PL,THL,RDQ,RDTH,RDP,RDTHE
     2,            DETA2,AETA2,DFRLG
     3,            QS0,SQS,STHE,THE0
c       WRITE(0,*)'READ ROS'
      ENDIF
C
      CALL MPI_BCAST(ROS,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(CS,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(DS,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(ROI,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(CI,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(DI,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(PL,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(THL,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(RDQ,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(RDTH,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(RDP,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(RDTHE,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(DETA2(1),LM,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(AETA2(1),LM,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(DFRLG(1),LP1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(QS0(1),JTB,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(SQS(1),JTB,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(STHE(1),ITB,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(THE0(1),ITB,MPI_REAL,0,my_comm,IRTN)
C
      CALL MPI_BARRIER(my_comm,IRTN)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE MXSNAL
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ MXSNAL'
      ENDIF
C
      CALL DSTRB(TEMP1,MXSNAL,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE EPSR
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ EPSR'
      ENDIF
C
      CALL DSTRB(TEMP1,EPSR,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE TG
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ TG'
      ENDIF
C
      CALL DSTRB(TEMP1,TG,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE GFFC
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ GFFC'
      ENDIF
C
      CALL DSTRB(TEMP1,GFFC,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE SST
C***
!   save a copy of sst on full grid for sat data processing
       allocate(save_full_sst(im*jm))
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ SST'


       ii=0
       do j=1,jm
        do i=1,im
         ii=ii+1
         save_full_sst(ii)=temp1(i,j)
        end do
       end do
      ENDIF
      call mpi_bcast(save_full_sst,im*jm,mpi_real4,0,
     *               my_comm,irtn)
C
      CALL DSTRB(TEMP1,SST,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE ALBASE
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ ALBASE'
      ENDIF
C
      CALL DSTRB(TEMP1,ALBASE,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE HDAC
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ HDAC'
      ENDIF
C
      CALL DSTRB(TEMP1,HDAC,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE HDACV
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
c       WRITE(0,*)'READ HDACV'
      ENDIF
C
      CALL DSTRB(TEMP1,HDACV,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE TTBLQ
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TTBLQ
c       WRITE(0,*)'READ TTBLQ'
      ENDIF
C
      CALL MPI_BCAST(TTBLQ(1,1),ITBQ*JTBQ,MPI_REAL,0,
     1               my_comm,IRTN)
C
      CALL MPI_BARRIER(my_comm,IRTN)
C-----------------------------------------------------------------------
C
      IF(MYPE.EQ.0)THEN
!#ifdef DP_REAL
!        READ(NHB)PTBL,TTBL
!     1,            R1,PT1,TSPH
!     2,            WBD,SBD,TLM0D,TPH0D,DLMD,DPHD,CMLD,DP30
!     3,            X1P,Y1P,IXMX,IYMX
!     4,            DETA1,AETA1,ETA1
!        IXM=IXMX
!        IYM=IYMX
!#else
        READ(NHB)PTBL,TTBL
     1,            R1,PT1,TSPH
     2,            WBD,SBD,TLM0D,TPH0D,DLMD,DPHD,CMLD,DP30
     3,            X1P,Y1P,IXM,IYM
     4,            DETA1,AETA1,ETA1
!#endif
c       WRITE(0,*)'READ PTBL'
      ENDIF
c
      CALL MPI_BCAST(PTBL(1,1),ITB*JTB,MPI_REAL,0,
     1               my_comm,IRTN)
      CALL MPI_BCAST(TTBL(1,1),JTB*ITB,MPI_REAL,0,
     1                my_comm,IRTN)
      CALL MPI_BCAST(R1,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(PT1,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(TSPH,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(WBD,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(SBD,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(TLM0D,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(TPH0D,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(DLMD,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(DPHD,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(CMLD,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(DP30,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(X1P,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(Y1P,1,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(IXM,1,MPI_INTEGER,0,my_comm,IRTN)
      CALL MPI_BCAST(IYM,1,MPI_INTEGER,0,my_comm,IRTN)
      CALL MPI_BCAST(DETA1(1),LM,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(AETA1(1),LM,MPI_REAL,0,my_comm,IRTN)
      CALL MPI_BCAST(ETA1(1),LP1,MPI_REAL,0,my_comm,IRTN)
C
      CALL MPI_BARRIER(my_comm,IRTN)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE IVGTYP
C***
      IF(MYPE.EQ.0)THEN
!#ifdef DP_REAL
!        READ(NHB)ITEMPX
!        ITEMP=ITEMPX
!#else
        READ(NHB)ITEMP
!#endif
      ENDIF
C
      CALL IDSTRB(ITEMP,IVGTYP)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE ISLTYP
C***
      IF(MYPE.EQ.0)THEN
!#ifdef DP_REAL
!        READ(NHB)ITEMPX
!        ITEMP=ITEMPX
!#else
        READ(NHB)ITEMP
!#endif
      ENDIF
C
      CALL IDSTRB(ITEMP,ISLTYP)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE ISLOPE
C***
      IF(MYPE.EQ.0)THEN
!#ifdef DP_REAL
!        READ(NHB)ITEMPX
!        ITEMP=ITEMPX
!#else
        READ(NHB)ITEMP
!#endif
      ENDIF
C
      CALL IDSTRB(ITEMP,ISLOPE)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE VEGFRC
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)TEMP1
      ENDIF
C
      CALL DSTRB(TEMP1,VEGFRC,1,1,1)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE SLDPTH
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)SLDPTH
      ENDIF
C
      CALL MPI_BCAST(SLDPTH(1),NSOIL,MPI_REAL,0,my_comm,IRTN)
C-----------------------------------------------------------------------
C***
C***  DISTRIBUTE RTDPTH
C***
      IF(MYPE.EQ.0)THEN
        READ(NHB)RTDPTH
      ENDIF
C
      CALL MPI_BCAST(RTDPTH(1),NSOIL,MPI_REAL,0,my_comm,IRTN)
C-----------------------------------------------------------------------
C
C     END OF SUBROUTINE READ_NHB
C     
C-----------------------------------------------------------------------
      RETURN
      END

