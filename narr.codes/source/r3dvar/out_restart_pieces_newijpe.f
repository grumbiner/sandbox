subroutine out_restart_pieces_newijpe(inpes_out,jnpes_out)

!   starting from CHKOUT, modify extensively so that a restart file in pieces can be
!    generated efficiently for arbitrary partitioning (so analysis can be
!      run with different number of processes relative to the model, without the
!       requirement to run expensive quilt jobs to always recover a monolithic restart file.

      INCLUDE "PARMETA.comm"
      INCLUDE "PARMTBL.comm"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
        include "my_comm.h"
  include "old_2_new_type.h"
!#include "sp.h"
!--------------------------------------------------------------------     
!                           P A R A M E T E R
!    & (IMJM=IM*JM-JM/2,IMT=2*IM-1,JMT=JM/2+1,LB=2*IM+JM-3)
!--------------------------------------------------------------------     
!                           P A R A M E T E R
!    & (LM1=LM-1,LP1=LM+1,JAM=6+2*(JM-10)
!    &, NRLX1=250,NRLX2=100)
!--------------------------------------------------------------------     
                            PARAMETER &
       (CAPA=0.285896)
!--------------------------------------------------------------------     
!     
!     DECLARE VARIABLES.
!
!--------------------------------------------------------------------     
                            LOGICAL &
       RUN,FIRST,RESTRT,SIGMA,STDRD,MESO,ONHOUR,EXBC,NEST
!--------------------------------------------------------------------     
      CHARACTER*2  FHR                             
      CHARACTER*8  OUTJOB
      CHARACTER*13 ASSIGN
      CHARACTER*4  ASTMRK,TMYY
      CHARACTER*15 SUBMIT
      CHARACTER*32 LABEL
      CHARACTER*80 LINE
      CHARACTER*1  LINE1(80)
      CHARACTER*4 RESTHR
      character(2) restihr
      EQUIVALENCE  (LINE,LINE1)
!--------------------------------------------------------------------     
                            REAL &
       PSLP  (IDIM1:IDIM2,JDIM1:JDIM2) &
      ,PDS   (IDIM1:IDIM2,JDIM1:JDIM2) &
      ,FACTR (IDIM1:IDIM2,JDIM1:JDIM2) &
!     ,SWTTC (IDIM1:IDIM2,JDIM1:JDIM2,LM) &
      ,TTND  (IDIM1:IDIM2,JDIM1:JDIM2,LM)
!
                            INTEGER &
       IKNTS(0:INPES*JNPES-1),IDISP(0:INPES*JNPES-1)
!
                            REAL &
      ,ALLOCATABLE,DIMENSION(:,:,:) :: TEMPSOIL
!
!--------------------------------------------------------------------     
      CHARACTER FINFIL*50,DONE*10
!--------------------------------------------------------------------     
!     
!     INCLUDE COMMON BLOCKS.
!
!--------------------------------------------------------------------     
      INCLUDE "OUTFIL.comm"
      INCLUDE "CTLBLK.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "DYNAMD.comm"
      INCLUDE "PHYS2.comm"
      INCLUDE "BOCO.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "ACMCLD.comm"
      INCLUDE "ACMCLH.comm"
      INCLUDE "ACMPRE.comm"
      INCLUDE "ACMRDL.comm"
      INCLUDE "ACMRDS.comm"
      INCLUDE "ACMSFC.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "PRFHLD.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "INDX.comm"
      INCLUDE "CONTIN.comm"
      INCLUDE "QFLX.comm"
      INCLUDE "PPTASM.comm"
!--------------------------------------------------------------------     
!     
!     DECLARE EQUIVALENCES.
!
!--------------------------------------------------------------------     
!                           E Q U I V A L E N C E
!    & (TTND (1,1,1),SWTTC(1,1,1))
!--------------------------------------------------------------------     
                            INTEGER &
       JSTAT(MPI_STATUS_SIZE)
!--------------------------------------------------------------------     
      REAL(8) SUMT(LM), &
              SUMT_0(LM), &
              SUMT2(LM), &
              SUMT2_0(LM)
      REAL(8) STDEV,RMS,TMEAN
      REAL    TMAX(LM), TMAX_0(LM), TMIN(LM), TMIN_0(LM)
!--------------------------------------------------------------------     
!***
!***  THE FOLLOWING ARE USED FOR TIMIMG PURPOSES ONLY
!***
      real*8 timef
      real nhb_tim,mpp_tim,init_tim
      common/timing/surfce_tim,nhb_tim,res_tim,exch_tim
      common/timchk/slp_tim,gath_tim,wrt_tim,prof_tim &
      ,             bcex_tim,stat_tim

!    stuff for conversion to new partition (inpes_out,jnpes_out)

  type(old_2_new_cons) o_2_n
  integer(4) ips_out(0:inpes_out*jnpes_out-1),ipe_out(0:inpes_out*jnpes_out-1)
  integer(4) jps_out(0:inpes_out*jnpes_out-1),jpe_out(0:inpes_out*jnpes_out-1)
  real(4),allocatable::out_f1(:,:),out_f2(:,:),out_f3(:,:),out_f4(:,:),out_f5(:,:),out_f6(:,:),out_f7(:,:)
  real(4),allocatable::out_f8(:,:),out_f9(:,:),out_f10(:,:),out_f11(:,:)
  real(4),allocatable::out_f12(:,:),out_f13(:,:),out_f14(:,:),out_f15(:,:)
  real(4),allocatable::out_fsoil(:,:,:)
  integer(4),allocatable::iout_f1(:,:),iout_f2(:,:)

!***********************************************************************
!     START CHKOUT HERE.
!***********************************************************************

            if(mype.eq.0) write(0,*)' at 1 in out_restart_pieces_newijpe'
  allocate(o_2_n%nsend(0:NPES-1))
  allocate(o_2_n%ndsend(0:NPES))
  allocate(o_2_n%nrecv(0:NPES-1))
  allocate(o_2_n%ndrecv(0:NPES))
  allocate(o_2_n%pe_of_injn_new(inpes_out,jnpes_out))
  allocate(o_2_n%in_of_i_new(IM))
  allocate(o_2_n%jn_of_j_new(JM))

!  compute new partition information

            if(mype.eq.0) write(0,*)' at 2 in out_restart_pieces_newijpe'
  call mppinit_new(inpes_out,jnpes_out,IM,JM, &
      o_2_n%ids,  o_2_n%ide, o_2_n%jds, o_2_n%jde, &
      ips_out,  ipe_out, jps_out, jpe_out, &
      o_2_n%pe_of_injn_new, o_2_n%in_of_i_new, o_2_n%jn_of_j_new )
  o_2_n%npes_old=NPES
  o_2_n%mype=MYPE
  o_2_n%inpes_old=INPES
  o_2_n%jnpes_old=JNPES
       !rite(0,*)' after mppinit_new, mype,inpes_out,jnpes_out,im,jm=',mype,inpes_out,jnpes_out,im,jm
     !rite(0,*)' after mppinit_new, mype,o_2_n%ids,ide,jds,jde=',mype,o_2_n%ids,o_2_n%ide,o_2_n%jds,o_2_n%jde
       !rite(0,*)' after mppinit_new, mype,ips_out=',mype,ips_out
       !rite(0,*)' after mppinit_new, mype,ipe_out=',mype,ipe_out
       !rite(0,*)' after mppinit_new, mype,jps_out=',mype,jps_out
       !rite(0,*)' after mppinit_new, mype,jpe_out=',mype,jpe_out
       !rite(0,*)' after mppinit_new, mype,o_2_n%pe_of_injn_new=',mype,o_2_n%pe_of_injn_new
   !  do i=1,im
   !   write(0,*)' after mppinit_new, mype,o_2_n%in_of_i_new(',i,')=',mype,o_2_n%in_of_i_new(i)
   !  end do
   !  do j=1,jm
   !   write(0,*)' after mppinit_new, mype,o_2_n%jn_of_j_new(',j,')=',mype,o_2_n%jn_of_j_new(j)
   !  end do
  npes_out=inpes_out*jnpes_out
  loop_out_max=1+(npes_out-1)/npes

  idone_pes=-NPES
  do loop_out=1,loop_out_max          !   begin big loop--generate NPES pieces at a time, until finished

   idone_pes=idone_pes+NPES
   mype_out=idone_pes+MYPE

!    compute send and recv info for alltoallv commands

            if(mype.eq.0) write(0,*)' at 3 in out_restart_pieces_newijpe, loop_out,idone_pes=', &
                                                    loop_out,idone_pes
   call get_sendrecv(o_2_n,MY_IS_GLB,MY_IE_GLB,MY_JS_GLB,MY_JE_GLB,NPES, &
         inpes_out,jnpes_out,idone_pes, &
      o_2_n%ids,  o_2_n%ide, o_2_n%jds, o_2_n%jde, &
      o_2_n%pe_of_injn_new, o_2_n%in_of_i_new, o_2_n%jn_of_j_new , &
      o_2_n%nsend,o_2_n%ndsend,o_2_n%nrecv,o_2_n%ndrecv)
            !rite(0,*)' at 4 in out_restart_pieces_newijpe, mype,loop_out,idone_pes=', &
                 !                                  mype,loop_out,idone_pes
                 !rite(0,*)' after get_sendrecv, mype,loop_out,my_is,ie,js,je_glb,npes=', &
                 !                           mype,loop_out,my_is_glb,my_ie_glb,my_js_glb,my_je_glb,npes
                 !rite(0,*)' after get_sendrecv, mype,inpes_out,jnpes_out,idone_pes=', &
                 !                           mype,inpes_out,jnpes_out,idone_pes
                 !rite(0,*)' after get_sendrecv, mype,o_2_n%nsend=',mype,o_2_n%nsend
                 !rite(0,*)' after get_sendrecv, mype,o_2_n%ndsend=',mype,o_2_n%ndsend
                 !rite(0,*)' after get_sendrecv, mype,o_2_n%nrecv=',mype,o_2_n%nrecv
                 !rite(0,*)' after get_sendrecv, mype,o_2_n%ndrecv=',mype,o_2_n%ndrecv

   ips_old=MY_IS_GLB
   ipe_old=MY_IE_GLB
   jps_old=MY_JS_GLB
   jpe_old=MY_JE_GLB
   ims_old=IDIM1-MY_IS_LOC+MY_IS_GLB
   ime_old=IDIM2-MY_IS_LOC+MY_IS_GLB
   jms_old=JDIM1-MY_JS_LOC+MY_JS_GLB
   jme_old=JDIM2-MY_JS_LOC+MY_JS_GLB
   ips_new=ips_out(min(mype_out,npes_out-1))
   ipe_new=ipe_out(min(mype_out,npes_out-1))
   jps_new=jps_out(min(mype_out,npes_out-1))
   jpe_new=jpe_out(min(mype_out,npes_out-1))
   allocate(out_f1(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f2(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f3(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f4(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f5(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f6(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f7(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f8(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f9(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f10(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f11(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f12(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f13(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f14(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_f15(ips_new:ipe_new,jps_new:jpe_new))
   allocate(out_fsoil(ips_new:ipe_new,jps_new:jpe_new,nsoil))
   allocate(iout_f1(ips_new:ipe_new,jps_new:jpe_new))
   allocate(iout_f2(ips_new:ipe_new,jps_new:jpe_new))

!!!!!!!!!! eliminated everything before writing of restart file

!***
!***  CREATE NAME FOR RESTART FILE.
!***
      ITAG=NTSD/TSPH+0.5
      if(itag.eq.0) then

!-------------- this is write of analysis restart file

       call getenv("restrtahr",restihr)
!      call getenv("tmmarka",resthr)
       call getenv("tmmark",resthr)
       write(rstfil,1150)restihr,mype_out,resthr
 1150     FORMAT('restrt',a2, &
                  '.',I3.3,'.',a4)
      ELSE

!--------------- guess file was only available in monolithic form, and
!--------------- so is being written out as mini-files for re-reading
!--------------- at end of analysis

!      call getenv("tmmarkb",resthr)
       call getenv("tmmark",resthr)
          WRITE(RSTFIL,1155)ITAG,MYPE_out,RESTHR
 1155     FORMAT('restrt',I2.2 &
      ,           '.',I3.3,'.',a4)
      ENDIF
      print*,'rstfil=',rstfil
!***
!***  OPEN UNIT TO RESTART FILE.
!***
        LRSTRT=8
!
        wrt_tim=0.
        btimw=timef()
        btim0=timef()
!
        if(mype_out.le.npes_out-1) then
         CLOSE(LRSTRT)
         OPEN(UNIT=LRSTRT,FILE=RSTFIL,FORM='UNFORMATTED',IOSTAT=IER) 
         IF(IER.NE.0)WRITE(LIST,*)' LRSTRT OPEN UNIT ERROR IER=',IER
        end if
        
!***
!***  WRITE DATE AND TIMESTEP INFORMATION TO RESTART FILE.
!***
        LABEL='OMEGA-ALPHA*DT/CP'
        if(mype_out.le.npes_out-1) WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
!     ENDIF
!----------------------------------------------------------------------
!***
!***  BEGIN WRITING THE RESTRT FILE
!***
!----------------------------------------------------------------------
!
            if(mype.eq.0) write(0,*)' at 6 in out_restart_pieces_newijpe, loop_out,idone_pes=', &
                                                    loop_out,idone_pes
!       if(mype.eq.0) write(0,*)' before 1st old_to_new, mype,ims_old,ime_old,jms_old,jme_old=', &
!                                     mype,ims_old,ime_old,jms_old,jme_old
!       if(mype.eq.0) write(0,*)' before 1st old_to_new, mype,ips_new,ipe_new,jps_new,jpe_new=', &
!                                     mype,ips_new,ipe_new,jps_new,jpe_new
!       if(mype.eq.0) write(0,*)' before 1st old_to_new, pd(1,1),pd(1,myje),pd(myie,1),pd(myie,myje)=', &
!                                    pd(1,1),pd(1,myje),pd(myie,1),pd(myie,myje)
      call old_to_new(PD,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(RES,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2
!----------------------------------------------------------------------
!
      DO L=1,LM
       call old_to_new(OMGALF(idim1,jdim1,L),o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
        if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
      ENDDO
! rec46
!
      LABEL = 'BND,PD,RES,T,Q,U,V,Q2,TTND,CWM,TRAIN,TCUCN'
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL &
      ,              FIRST,IOUT,NSHDE
! rec47
!----------------------------------------------------------------------
!
      call old_to_new(FIS,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3
!CCCC
!CCCC
!CCCC   BOUNDARY CONDITION WRITE CHANGED TO BLANK RECORD
!CCCC
!CCCC
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)
! rec48
!----------------------------------------------------------------------
!
      ttnd=0.
      DO L = 1,LM
        call old_to_new(T(idim1,jdim1,L),o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
        if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
!
        call old_to_new(Q(idim1,jdim1,L),o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
        if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
!
        call old_to_new(U(idim1,jdim1,L),o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
        if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
!
        call old_to_new(V(idim1,jdim1,L),o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
        if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
!
        call old_to_new(Q2(idim1,jdim1,L),o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
        if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
!
        call old_to_new(TTND(idim1,jdim1,L),o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
        if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
!
        call old_to_new(CWM(idim1,jdim1,L),o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
        if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
!
        call old_to_new(TRAIN(idim1,jdim1,L),o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
        if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
!
        call old_to_new(TCUCN(idim1,jdim1,L),o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
        if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
      ENDDO
! rec453
!----------------------------------------------------------------------
!
      LABEL = 'MISC VARIABLES'
!?????????????????????????????????/checked to here
            if(mype.eq.0) write(0,*)' at 7 in out_restart_pieces_newijpe, loop_out,idone_pes=', &
                                                    loop_out,idone_pes
      call old_to_new(RSWIN,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(RSWOUT,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(TG,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(Z0,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(AKMS,o_2_n,out_f5, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(CZEN,o_2_n,out_f6, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL, &
                                              out_f1,out_f2,out_f3,out_f4,out_f5,out_f6
! rec454
!----------------------------------------------------------------------
!
      call old_to_new(AKHS,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(THS,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(QS,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(TWBS,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(QWBS,o_2_n,out_f5, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(HBOT,o_2_n,out_f6, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(CFRACL,o_2_n,out_f7, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3,out_f4,out_f5,out_f6,out_f7
! rec455
!----------------------------------------------------------------------
!
      call old_to_new(THZ0,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(QZ0,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(UZ0,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(VZ0,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(USTAR,o_2_n,out_f5, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(HTOP,o_2_n,out_f6, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(CFRACM,o_2_n,out_f7, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3,out_f4,out_f5,out_f6,out_f7
! rec456
!----------------------------------------------------------------------
!
      call old_to_new(SNO,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SI,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(CLDEFI,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(RF,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      pslp=0.
      call old_to_new(PSLP,o_2_n,out_f5, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(CUPPT,o_2_n,out_f6, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(CFRACH,o_2_n,out_f7, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3,out_f4,out_f5,out_f6,out_f7
! rec457
!----------------------------------------------------------------------
!
            if(mype.eq.0) write(0,*)' at 8 in out_restart_pieces_newijpe, loop_out,idone_pes=', &
                                                    loop_out,idone_pes
      call old_to_new(SOILTB,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SFCEXC,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SMSTAV,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SMSTOT,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(GRNFLX,o_2_n,out_f5, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(PCTSNO,o_2_n,out_f6, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3,out_f4,out_f5,out_f6
! rec458
!----------------------------------------------------------------------
!
      call old_to_new(RLWIN,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(RADOT,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(CZMEAN,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SIGT4,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3,out_f4
! rec459
!----------------------------------------------------------------------
!
      call old_to_new(U00,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call iold_to_new(LC,o_2_n,iout_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SR,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,ul,iout_f1,out_f2
! rec460
!----------------------------------------------------------------------
!
      LABEL = 'ACCUMULATED VARIABLES'
      call old_to_new(PREC,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(ACPREC,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(ACCLIQ,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(CUPREC,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL,out_f1,out_f2,out_f3,out_f4
                     
! rec461
!----------------------------------------------------------------------
!
            if(mype.eq.0) write(0,*)' at 9 in out_restart_pieces_newijpe, loop_out,idone_pes=', &
                                                    loop_out,idone_pes
      call old_to_new(ACFRCV,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call iold_to_new(NCFRCV,o_2_n,iout_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(ACFRST,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call iold_to_new(NCFRST,o_2_n,iout_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,iout_f1,out_f2,iout_f2
! rec462
!----------------------------------------------------------------------
!
      call old_to_new(ACSNOW,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(ACSNOM,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SSROFF,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(BGROFF,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3,out_f4
! rec463
!----------------------------------------------------------------------
!
      call old_to_new(SFCSHX,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SFCLHX,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SUBSHX,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SNOPCX,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SFCUVX,o_2_n,out_f5, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(SFCEVP,o_2_n,out_f6, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(POTEVP,o_2_n,out_f7, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3,out_f4,out_f5,out_f6,out_f7
! rec464
!----------------------------------------------------------------------
!
      call old_to_new(ASWIN,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(ASWOUT,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(ASWTOA,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(ALWIN,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(ALWOUT,o_2_n,out_f5, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(ALWTOA,o_2_n,out_f6, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3,out_f4,out_f5,out_f6
!
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)ARDSW,ARDLW,ASRFC,AVRAIN,AVCNVC
! rec465
!
      call old_to_new(TH10,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(Q10,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(U10,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(V10,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(TSHLTR,o_2_n,out_f5, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(QSHLTR,o_2_n,out_f6, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(PSHLTR,o_2_n,out_f7, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(TH30,o_2_n,out_f8, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(Q30,o_2_n,out_f9, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(U30,o_2_n,out_f10, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(V30,o_2_n,out_f11, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3,out_f4,out_f5,out_f6,out_f7,out_f8,out_f9,out_f10,out_f11
! rec466
!----------------------------------------------------------------------
!
            if(mype.eq.0) write(0,*)' at 10 in out_restart_pieces_newijpe, loop_out,idone_pes=', &
                                                    loop_out,idone_pes
      do N=1,NSOIL
       call old_to_new(SMC(idim1,jdim1,N),o_2_n,out_fsoil(ips_new:ipe_new,jps_new:jpe_new,N), &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      end do
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_fsoil
! rec467
!----------------------------------------------------------------------
!
      call old_to_new(CMC,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1
! rec468
!----------------------------------------------------------------------
!
      do N=1,NSOIL
       call old_to_new(STC(idim1,jdim1,N),o_2_n,out_fsoil(ips_new:ipe_new,jps_new:jpe_new,N), &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      end do
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_fsoil
!
      do N=1,NSOIL
       call old_to_new(SH2O(idim1,jdim1,N),o_2_n,out_fsoil(ips_new:ipe_new,jps_new:jpe_new,N), &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      end do
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_fsoil
      call old_to_new(ALBEDO,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1
! rec???
!----------------------------------------------------------------------
      call old_to_new(FQU,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(FQV,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(DQFLX,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(FCU,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(FCV,o_2_n,out_f5, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(DCFLX,o_2_n,out_f6, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)

      call old_to_new(FQU7,o_2_n,out_f7, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(FQV7,o_2_n,out_f8, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(DQFLX7,o_2_n,out_f9, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(FCU7,o_2_n,out_f10, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(FCV7,o_2_n,out_f11, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(DCFLX7,o_2_n,out_f12, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)

      call old_to_new(DQADV,o_2_n,out_f13, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(FQNEV1,o_2_n,out_f14, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(FQSEV1,o_2_n,out_f15, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)

      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3,out_f4,out_f5,out_f6, &
               out_f7,out_f8,out_f9,out_f10,out_f11,out_f12, &
               out_f13,out_f14,out_f15
! rec???
!----------------------------------------------------------------------
      call old_to_new(VAPINC,o_2_n,out_f1, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(CLDINC,o_2_n,out_f2, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(VAPINC7,o_2_n,out_f3, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      call old_to_new(CLDINC7,o_2_n,out_f4, &
                      ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
      if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3,out_f4
! rec469
!----------------------------------------------------------------------
!     call old_to_new(POTFLX,o_2_n,out_f1, &
!                     ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
!     call old_to_new(TLMIN,o_2_n,out_f2, &
!                     ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
!     call old_to_new(TLMAX,o_2_n,out_f3, &
!                     ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
!     if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f1,out_f2,out_f3  &
!     ,              ACUTIM,ARATIM,APHTIM &
!     ,              NHEAT,NPHS,NCNVC,NPREC,NRDSW,NRDLW,NSRFC &
!     ,              TPH0D,TLM0D,RESTRT
! rec470
!----------------------------------------------------------------------
!           if(mype.eq.0) write(0,*)' at 11 in out_restart_pieces_newijpe, loop_out,idone_pes=', &
!                                                   loop_out,idone_pes
!     DO L=1,LM
!       call old_to_new(RSWTT(idim1,jdim1,L),o_2_n,out_f4, &
!                     ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
!       if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
!       call old_to_new(RLWTT(idim1,jdim1,L),o_2_n,out_f4, &
!                     ims_old,ime_old,jms_old,jme_old,ips_new,ipe_new,jps_new,jpe_new)
!       if(mype_out.le.npes_out-1) WRITE(LRSTRT)out_f4
!     ENDDO
! rec560
!----------------------------------------------------------------------
!***
!***  CLOSE THE RESTART FILE.
!***
      CLOSE(LRSTRT)

   deallocate(out_f1)
   deallocate(out_f2)
   deallocate(out_f3)
   deallocate(out_f4)
   deallocate(out_f5)
   deallocate(out_f6)
   deallocate(out_f7)
   deallocate(out_f8)
   deallocate(out_f9)
   deallocate(out_f10)
   deallocate(out_f11)
   deallocate(out_f12)
   deallocate(out_f13)
   deallocate(out_f14)
   deallocate(out_f15)
   deallocate(out_fsoil)
   deallocate(iout_f1)
   deallocate(iout_f2)
   deallocate(o_2_n%isend)
   deallocate(o_2_n%jsend)
   deallocate(o_2_n%irecv)
   deallocate(o_2_n%jrecv)

  end do                       !  end big loop , loop variable loop_max
!
      dif_tim=timef()-btim0
      wrt_tim=wrt_tim+dif_tim
      call mpi_reduce(wrt_tim,wrt_tim_0,1,MPI_REAL,MPI_MAX,0, &
                      my_comm,ierr)
      if(mype.eq.0)then
        write(6,*)' WROTE RESTRT FILE, time = ', wrt_tim_0*1.e-03
      endif
      CALL MPI_BARRIER(my_comm,ISTAT)
!***
!***  SEND SIGNAL THAT ALL TASKS HAVE FINISHED WRITING
!***
      IF(MYPE.EQ.0)THEN
        DONE='DONE'
        WRITE(FINFIL,1190)ITAG,RESTHR
 1190   FORMAT('3dvrdone',I2.2,'.',A4)
        LFINFIL=91
        CLOSE(LFINFIL)
        OPEN(UNIT=LFINFIL,FILE=FINFIL,FORM='UNFORMATTED',IOSTAT=IER)
        WRITE(LFINFIL)DONE
        CLOSE(LFINFIL)
        IF(IER.NE.0)WRITE(LIST,*)' SIGNAL SENT TO FINFIL:  DONE'
      ENDIF
!----------------------------------------------------------------------

!!!!!!!!!!!!!!!! eliminated everything from here to end of CHKOUT 

      RETURN
      END
