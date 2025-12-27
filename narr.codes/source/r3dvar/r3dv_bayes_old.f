      program r3dv
!$$$  main program documentation block
!                .      .    .                                       .
! main program: R3DVAR      onal 3-d variational analysis
!   PRGMMR: ROGERS           ORG: NP22        DATE: 98-01-05  
!
! abstract: the regional 3-d variational analysis routine
!   performs a regional analysis adapted to the eta model.  this
!   version of the analysis is done on pressure coordinates.  The 
!   horizontal analysis grid is every other row of the eta h-grid.
!   The analysis increments are interpolated in the horizontal
!   as necessary to get to the staggered points left out of the analysis
!   grid.  Then vertical interpolation to the eta coordinate is done
!   before the increment is added to the eta guess.
!   
!   The background error covariance is defined here not spectrally as
!   in the SSI, but using a recursive filter (J. Purser).  The form
!   used here is fast and flexible, allowing for
!   spatially varying error variance and correlation length.  There is
!   a small amount of distortion near the boundaries, which may not
!   matter very much, since correlation models are gross approximations
!   anyway.  However, there is allowance for a buffer zone around the
!   analysis domain, should this be a problem.
!
!   Because this analysis was initially created to assimilate doppler
!   radar winds, all winds are treated as line-of-sight winds.  A
!   conventional wind observation becomes two line-of-sight observations,
!   one along a north pointing line (earth v component) and one along
!   an east pointing line (earth u component).  All line-of-sight
!   winds, radar or conventional, are then assigned the angle that
!   each observation line-of-sight makes with the eta grid x-axis.
!   the forward model (which computes a simulated observation from 
!   model variables) is just
!           wobs(theta) = u*cos(theta) + v*sin(theta)
!   
! program history log:
!   95-01-24  parrish
!   95-02-03  parrish:  add wind analysis capability
!   95-02-15  parrish:  add ref level, sfc pressure data
!   95-02-16  parrish:  add stuff to update eta restart file
!   95-05-03  parrish:  begin conversion to observation space form
!                        using descent methods devised by J. Purser
!   95-08-04  parrish:  continue with rewrite, adding eta guess capability
!
! **** note: the following rules are mandatory for cray programs ****
! *                                                                 *
! *          (1) use unit numbers 11-49 for all input files.        *
! *          (2) use unit numbers 51-89 for all output files.       *
! *          (3) use unit numbers 90-99 for work files that are     *
! *              internal to the program and used only in the       *
! *              program unit.                                      *
! *          (4) unit numbers 1-4, 8-10 and 50 are reserved for     *
! *              future use.                                        *
! *                                                                 *
! ****  except for work files, a unit number should not be used     *
! *     for both input and output.                                  *
! *******************************************************************
!
! usage:
!   input files:
! **************************
!    unit numbers are specified for most files on
!    input data cards. can be changed by changing data cards
! **************************
!     inprep   - input bufr data file from quality control routine
!     inges    - six hour forecast guess field
!
!   output files:  (including scratch files)
!     fort.6   - printout
!
!   subprograms called:
!     unique:    - subroutines: filterp filterx filtery 
!                -  fitoftemp fitoftemp2 fitofwind fitofwind2
!                -  gdcrdn gdcrdp getbufrdata
!                -  getgofp getguess gradpen
!                -  pcgr3dv r3dv r3dvex testfilter 
!                -  tfilterp tfilterx tfiltery tll
!                - 
!                - 
!
!                - function conmc
!     library:
!       w3lib    -
!
!   exit states:
!     cond =   0 - successful run
!          =  15 - no prepda data
!          =  56 - trouble in conmc
!
! remarks: resolution, unit numbers and several constants are
!          in the input data cards
!
! attributes:
!   language: fortran 8x (extensive use of dynamic memory feature)
!   machine:  cray
!
!$$$ 
!--------
!-------------input namelist variables:
!--------
!---  inbufr:        input unit number for bufr data file
!---  inradar:       input unit number for bufr radar data
!---  imeta,jmeta,lmeta:  IM, JM, LM--x,y,eta dimensions of eta model
!---  itbeta,jtbeta:      ITB, JTB--dimensions of sat-pres lookup table for
!---                                 eta model
!---  grosst:          observations that sneak through other qc
!---  grossw:          observations that sneak through other qc
!---  erlon0,erlat0: earth lon,lat of center of grid (degrees)
!---  dlon0,dlat0:   grid-lon,lat increments (x,y directions, degrees)
!---                 of eta model, anl grid inc = 2*dlon0,2*dlat0
!---  pbot,ptop:     pressure at bottom, top (mb)
!---  bshrinkp:      background pd cor length fudge factor
!---  bshrinkt:      background temp cor length fudge factor
!---  bshrinkq:      background moisture cor length fudge factor
!---  bshrinkpsi:    background psi cor length fudge factor
!---  bshrinkchi:    background chi cor length fudge factor
!---  pshrinkt:      background temp vert cor length fudge factor
!---  pshrinkq:      background moisture vert cor length fudge factor
!---  pshrinkpsi:    background psi vert cor length fudge factor
!---  pshrinkchi:    background chi vert cor length fudge factor
!---  bscalep:       background reference pd err fudge factor
!---  bscalet:       background temp error fudge factor
!---  bscaleq:       background moisture error fudge factor
!---  bscalepsi:     background psi error fudge factor
!---  bscalechi:     background chi error fudge factor
!---  bscalepred:    background bias corrector error fudge factor
!---  maxiter,teststop,icondition: stuff for cg iteration
!---  thetatest:     direction of test wind field for simulated data.
!---  verify:        =false, then this is regular analysis
!---                 =true, then this is just verification, so
!---                     stop after computation of statistics.
!---  delhour:       defines size of time window for accepting data.
!---  drift:         =true, then use information on radiosonde balloon drift
!---  linear_step:   =true if linear stepsize is used
!---  iuniterr:      unit number for qc errors
!---  delpmin_fill:  min pressure spacing (mb) for filling in t,q between sig levels for type 120 data
!----------

 use initialize
 use error_handler
 use transmittance_coefficients

  include 'mpif.h'
  include 'types.h'
  include 'satbias_type.h'
  include 'my_comm.h'

  type(satbias_stuff),allocatable::allbias_old(:),allbias_new(:)
  parameter(nlambda=14)
  logical bayes,binom,idiag_rad,isoq,diagnostic,test_pieces
  real(4) blambda_in(nlambda,nlambda),blambda_out(nlambda,nlambda),lambda_in(nlambda),lambda_out(nlambda)
  real(4) dlambda(nlambda)

  integer(4) idate5(5)
  character(20)ges_format
  real(4),allocatable::etaiex(:),etamex(:)
  real(4),allocatable::all(:)
  type(general_obs),pointer::alldata(:)
  integer(2),pointer::lev_val(:),lev_max(:)
  common/databuf/lev_val,lev_max,alldata
  real(4),pointer::allrad0(:,:)
  common/radbuf/allrad0
  type(rad_obs),allocatable::rad_dat(:)
  logical verify,coldstart,cwm_adjust,diagvar
  logical userad,drift,linear_step
  logical obs_space
  logical t_on,p_on,pw_on,w_on,q_on
  integer(4)one_type
  real(8) wb8,sb8,dlon8,dlat8
  integer(4),allocatable::lmh(:)
  real(4),allocatable::tetaanl(:,:),qetaanl(:,:),qsatetaanl(:,:),q2etaanl(:,:),cwmetaanl(:,:)
  real(4),allocatable::wetaanl(:,:,:),pdr01etaanl(:)
  real(4),allocatable::tetages(:,:),qetages(:,:),q2etages(:,:),cwmetages(:,:)
  real(4),allocatable::wetages(:,:,:),pdr01etages(:)
  integer(4) tasks_per_node_available,tasks_per_node_desired
  namelist/namr3dv/ &
    tasks_per_node_available,tasks_per_node_desired, &
    imetaglb,jmetaglb,ingesglob, &
    lmeta,itbeta,jtbeta,itbqeta,jtbqeta, &
    inradar,inbufr,inpes,jnpes,inpes_out,jnpes_out, &
    nvmodes,maxouter,maxinner,maxinnerqc,icondition, &
    iorddata,lhalfges,iordges,one_type, &
    teststop,grosst, &
    grossw,erlon0,erlat0,dlon0, &
    dlat0,pbot,ptop,bshrinkp,bshrinkt, &
    bshrinkq,bshrinkpsi,bshrinkchi,pshrinkt,pshrinkq, &
    pshrinkpsi,pshrinkchi,bscalep,bscalet,bscaleq, &
    bscalepsi,bscalechi,bscalepred, &
    corlmin,corlmax, &
    delhour,grossp,grosspw,grossq,one_pres, &
    one_lat,one_lon,one_error,one_obs,one_theta, &
    one_sbot,one_stop,dlonc,dlatc, &
    userad,drift,verify,coldstart,cwm_adjust,diagvar, &
    jppf,jpch,jpchus,nangs_max,nbias_loop,nbias_loop1,mbias_loop, &
    tbias,tcbias,dt_assimilation,cbias_step_clen, &
    nsat,npred,nhaloc,rnormmax,saterr_inflate,saterr_runfact, &
    npass,no_interp,nsmooth,nsmooth_shapiro,ifilt_ord, &
    npassq,no_interpq,nsmoothq,nsmoothq_shapiro,ifilt_ordq, &
    linear_step,iuniterr,delpmin_fill,obs_space, &
    t_on,p_on,pw_on,w_on,q_on,bayes,binom,isoq,diagnostic,test_pieces,iout_rad,idiag_rad, &
    instats_qx,instats_qf,scale_stats,iter_restore,iter_smooth,erradar_inflate
  character(10) satfile(50),sattype(50)
  integer(4) isatid(50),isatthin(50),nelesat(50)
  namelist/satname/satfile,sattype,isatid,isatthin

  data tasks_per_node_available/-1/,tasks_per_node_desired/-1/
  data tbias/15./,tcbias/30./  ! 1/e times for running ave sat bias (in days)
  data dt_assimilation/3./     !  assimilation time interval (in hours)
  data nbias_loop/5/           ! number of iterations to spinup satbias
  data nbias_loop1/1/          ! number of iterations at end of spinup, with no error inflation
  data mbias_loop/5/           ! number of inner iterations for spinup of satbias
  data cbias_step_clen/2./     ! cor len between sat view angles (in steps)
  data nangs_max/90/           !  max number of satellite view angles
  data erradar_inflate/3./
  data jppf/128/                            ! max no. profiles
  data jpch/142/                            !    no. of channels*(number of sats)
  data jpchus/19/                           !    max. no. of channels used
  data satfile/50*' '/,sattype/50*' '/
  data isatid/50*0/,isatthin/50*1/
  data nelesat/50*1/

  data iout_rad/207/,idiag_rad/.true./,instats_qx/32/,instats_qf/33/,scale_stats/.62/
  data iter_restore/14/,iter_smooth/2/
  data corlmin/1./,corlmax/40./
  data  &
     bayes/.false./,binom/.true./,isoq/.true./,diagnostic/.false./,test_pieces/.false./, &
     t_on/.true./,p_on/.true./,pw_on/.true./,w_on/.true./,q_on/.true./,obs_space/.false./, &
     imetaglb/92/,jmetaglb/141/,ingesglob/16/, &
     lmeta/38/,itbeta/76/,jtbeta/134/,inbufr/30/,inradar/31/, &
     nvmodes/8/,maxouter/1/,maxinner/50/,maxinnerqc/17/,icondition/10/, &
     iorddata/3/,lhalfges/1/,iordges/1/,one_type/0/, &
     nsat/15/,npred/5/,nhaloc/2/,npass/2/,nsmooth/0/, &
     nsmooth_shapiro/0/,ifilt_ord/1/,no_interp/10000/, &
     npassq/3/,nsmoothq/0/,nsmoothq_shapiro/2/,ifilt_ordq/2/,no_interpq/600/

  data  &
     teststop/3.e-3/,grosst/15./, &
     grossw/35./,erlon0/0./,erlat0/0./,dlon0/.5/, &
     dlat0/.5/,pbot/1100./,ptop/50./,bshrinkp/1./,bshrinkt/1./ &
     bshrinkq/1./,bshrinkpsi/1./,bshrinkchi/1./,pshrinkt/1./,pshrinkq/1./ &
     pshrinkpsi/1./,pshrinkchi/1./,bscalep/.2/,bscalet/.2/,bscaleq/.1/ &
     bscalepsi/.5/,bscalechi/.2/,bscalepred/1./,delhour/1.5/, &
     grossp/15./,grosspw/12./,grossq/90./,one_pres/1000./ &
     one_lat/40./,one_lon/-90./,one_error/1./,one_obs/10./,one_theta/0./ &
     one_sbot/1./,one_stop/0./,rnormmax/20./,saterr_inflate/5./,saterr_runfact/1.15/,delpmin_fill/10./
  data dlonc/0./,dlatc/0./

  data &
     userad/.true./,drift/.true./,verify/.false./, &
     coldstart/.true./,cwm_adjust/.false./,diagvar/.false./

  integer(8) number_8,numberall_8
  data iuniterr /400/, nfout /97/, linear_step /.false./
  data inpes_out/-1/,jnpes_out/-1/
  real(4) vadlat(500),vadlon(500),vadqm(500,60)
  integer(8) model_id,ierlon_8,ierlat_8
  integer(4) comdup
        logical intercomm_yes_my,intercomm_yes_myworld

  call mpi_init(ierr)

! CALL W3LOG('$S98005.86','R3DVAR  ')
  CALL W3TAGB('R3DVAR  ',0098,0005,0086,'NP22   ')

          time1=timef()

!!!!!!!!!!!for the moment, set blambda_in=I, lambda_in=0, dlambda=.005
!!!!!!!!  some trial runs indicate dlamba=.005 with centered difference is ok
!!!!  so for one-sided difference, use dlambda=.01

  blambda_in=0.
  do i=1,nlambda
   blambda_in(i,i)=1.
  end do
  lambda_in=0.
  dlambda=.003

  call mpi_comm_size(mpi_comm_world,npes_all,ierr)
  call mpi_comm_rank(mpi_comm_world,mype,ierr)

!--------  read remaining input parameters in parallel (all pe's read same input cons)

  rewind 15
           if(mype.eq.0) write(0,*)' at 1 in r3dv_bayes'
  read(15,namr3dv)
  read(15,satname)

!      set up communicator my_comm

  npes=inpes*jnpes
  if(npes_all.lt.npes) then
      print *,' ******************************************************'
      print *,' ******************************************************'
      print *,' ***********MAJOR PROBLEM******************************'
      print *,' ***********MAJOR PROBLEM******************************'
      print *,' ***********MAJOR PROBLEM******************************'
      print *,' ***********MAJOR PROBLEM******************************'
      print *
         PRINT *, ' THERE ARE INSUFFICIENT MPI TASKS TO CONTINUE'
         PRINT *, ' YOU MUST SPECIFY AT LEAST ',npes,' TASKS'
         PRINT *, ' STOPPING NOW'
         PRINT *, ' HASTA LA VISTA BABY'
      print *
      print *,' ***********MAJOR PROBLEM******************************'
      print *,' ***********MAJOR PROBLEM******************************'
      print *,' ***********MAJOR PROBLEM******************************'
      print *,' ***********MAJOR PROBLEM******************************'
      print *,' ******************************************************'
      print *,' ******************************************************'
      call mpi_abort(mpi_comm_world,1,ierr)
  end if
  call assign_color(icolor,icolorpe, &
                    tasks_per_node_available,tasks_per_node_desired, &
                    mype,npes,npes_all)
  call mpi_comm_dup(mpi_comm_world,comdup,ierr)
  call mpi_comm_split(comdup,icolor,icolorpe,my_comm,ierr)
  icolor=0
  call mpi_comm_split(comdup,icolor,icolorpe,my_comm_world,ierr)
       call mpi_comm_test_inter(my_comm,intercomm_yes_my,ierr)
       call mpi_comm_test_inter(my_comm,intercomm_yes_myworld,ierr)
              print *,' mype,icolorpe,intercomm_yes_my,intercomm_yes_myworld=', &
                      mype,icolorpe,intercomm_yes_my,intercomm_yes_myworld
    call mpi_comm_size(mpi_comm_world,npes_world,ierr)
    call mpi_comm_rank(mpi_comm_world,mype_world,ierr)
    call mpi_comm_size(my_comm,npes_my,ierr)
call mpi_comm_rank(my_comm,mype_my,ierr)
    call mpi_comm_size(my_comm_world,npes_my_world,ierr)
    call mpi_comm_rank(my_comm_world,mype_my_world,ierr)
       print *,' mype_world,mype_my,mype_my_world,npes_world,npes_my,npes_my_world=', &
             mype_world,mype_my,mype_my_world,npes_world,npes_my,npes_my_world
mype=mype_my

if(mype_my_world.lt.npes) then
    call mpi_comm_rank(my_comm,mype_my,ierr)
      print *,' inside my_comm area now, mype,mype_my=',mype,mype_my
!        if(mype.gt.-1000) then
!            call mpi_finalize(ierr)
!            stop
!        end if

!  initialize events files (one for each processor)

  call open_events(mype)

!             call test_multio(mype,npes)

!        initialize lookup table for getting saturation vapor pressure

  call gpvs

! Read coefficients for satellites

  error_status = initialize_rtm()

!       set inpes_out = inpes, jnpes_out = jnpes, if inpes_out=-1, jnpes_out=-1 (default values)

  if(inpes_out.eq.-1) then
   inpes_out=inpes
   jnpes_out=jnpes
  end if

!    make sure nhaloc is large enough

  if(nsmooth.gt.0) nhaloc=max(1,nhaloc)
  if(nsmooth_shapiro.gt.0) nhaloc=max(3,nhaloc)

if(mype.eq.0)   write(0,namr3dv)
if(mype.eq.0)   write(0,satname)
           if(mype.eq.0) write(0,*)' at 2 in r3dv_bayes'
  close(15)

!-------- read second record of global sigma file to get resolution of global spectral model

  jcapglob=0 ; nsigglob=0 ; nlonglob=0 ; nlathglob=0
  if(userad) then
   allocate(all(250))
   rewind ingesglob
   read(ingesglob,err=100,end=100)
   read(ingesglob,err=100,end=100)all
   close(ingesglob)
   jcapglob=nint(all(207))
   nsigglob=nint(all(208))
   nlonglob=nint(all(213))
   nlathglob=nint(.5*all(214))
   if(mype.eq.0) then
    print *,' global model parms follow:'
    print *,'     jcapglob=',jcapglob
    print *,'     nsigglob=',nsigglob
    print *,'     nlonglob=',nlonglob
    print *,'     nlathglob=',nlathglob
   end if
   go to 110
   100 continue
     if(mype.eq.0) then
       print *,' NO GLOBAL MODEL AVAILABLE FOR ETA 3DVAR'
       print *,'   USE CLIMATOLOGICAL OZONE FOR PROCESSING RADIANCE DATA'
       print *,'  '
       print *,' NO GLOBAL MODEL AVAILABLE FOR ETA 3DVAR'
       print *,'   USE CLIMATOLOGICAL OZONE FOR PROCESSING RADIANCE DATA'
       print *,'  '
       print *,' NO GLOBAL MODEL AVAILABLE FOR ETA 3DVAR'
       print *,'   USE CLIMATOLOGICAL OZONE FOR PROCESSING RADIANCE DATA'
       print *,'  '
       write(0,*)' NO GLOBAL MODEL AVAILABLE FOR ETA 3DVAR'
       write(0,*)'   USE CLIMATOLOGICAL OZONE FOR PROCESSING RADIANCE DATA'
       write(0,*)'  '
       write(0,*)' NO GLOBAL MODEL AVAILABLE FOR ETA 3DVAR'
       write(0,*)'   USE CLIMATOLOGICAL OZONE FOR PROCESSING RADIANCE DATA'
       write(0,*)'  '
       write(0,*)' NO GLOBAL MODEL AVAILABLE FOR ETA 3DVAR'
       write(0,*)'   USE CLIMATOLOGICAL OZONE FOR PROCESSING RADIANCE DATA'
       write(0,*)'  '
     end if
   110 continue
   deallocate(all)
  end if
           if(mype.eq.0) write(0,*)' at 3 in r3dv_bayes'

!-------- run parts of eta model init routines to bring in eta model guess from restart file,
!--------  and also get various eta model specifications
!  NOTE:  modifications have been made to eta model routines so dynamic space can be used, while
!           still maintaining original common blocks.  This is done by replacing all common
!           arrays with pointer variables, which are then allocated before calling the eta
!           model routines.  Also, many fixed parameters are replaced by name-list inputs
!           and the parameters become variables in some newly created common blocks

!      first initialize common/parmeta/ and common/parmtbl/

  call initialize_parmeta(imetaglb,jmetaglb,lmeta, &
           inpes,jnpes,itbeta,jtbeta,itbqeta,jtbqeta)

!       create space needed for reading in eta restart file

  call alloc_etages
  call alloc_mppcoms(mype,npes)

!     now use eta routine to compute everything needed for distribution of model fields to the pe's

           if(mype.eq.0) write(0,*)' at 4 in r3dv_bayes'
  call MPPINIT

!    finally read in model fields and distribute to pe's (writing out again to mini-files
!                if input not already in mini-file form)

  rewind 12
  rewind 22
           if(mype.eq.0) write(0,*)' at 5 in r3dv_bayes, time=',(timef()-time1)*.001
  call INIT
           if(mype.eq.0) write(0,*)' at 6 in r3dv_bayes, time=',(timef()-time1)*.001
          if(test_pieces) then
            call out_restart_pieces_newijpe(inpes_out,jnpes_out)
            call mpi_finalize(ierr)
            stop
          end if
  call GOSSIP
         !   end if
         !     if(mype.gt.-1000) then
         !        call mpi_finalize(ierr)
         !        stop
         !     end if
         !   if(mype.lt.npes) then
           if(mype.eq.0) write(0,*)' at 7 in r3dv_bayes, time=',(timef()-time1)*.001

!-------- retrieve remaining eta model specifications from
!--------  the eta consts file (which has already been read in by eta model routine INIT)

  lmetaex=lmeta
  allocate(etaiex(lmetaex+1)) ; allocate(etamex(lmetaex))
  call getetacons(lmeta,imeta,jmeta,erlon0,erlat0,dlon0,dlat0,ptop, &
         wb8,sb8,wbglb,sbglb,istagh,istagv,etaiex,etamex)

!       create unique model id for satbias file

  ierlon_8=floor(modulo(erlon0+720.,360.))+2.
  ierlat_8=floor(erlat0+92.)
  model_id=jmetaglb+10000_8*lmeta+10000000_8*ierlat_8+10000000000_8*ierlon_8

!   note:  wb8,sb8 are coordinates of sw corner of local domain,
!               imeta,jmeta are dimensions of local domain, and
!                istagh,istagv define sw corner point
!        istagh=0, sw corner is h-point
!              =1, sw corner is v-point
!        istagv=0, sw corner is v-point
!              =1, sw corner is h-point


!   returned imeta,jmeta from above call to getetacons are local domain dimensions,
!     whereas, imetaglb,jmetaglb on input namelist/namr3dv/ are the full domain dimensions

!-------------------imeta,jmeta,lmeta are eta-model local grid dimensions 
!--------------------------------need on output

  if(mype.eq.0) then
   write(6,200)
   200 format(' calling r3dv with following input parameters:',//)
   print *,' imetaglb,jmetaglb,ingesglob=',imetaglb,jmetaglb,ingesglob
   print *,' lmeta,itbeta,jtbeta,itbqeta,jtbqeta=',lmeta,itbeta,jtbeta,itbqeta,jtbqeta
   print *,' inbufr,inpes,jnpes=',inbufr,inpes,jnpes
   print *,' inpes_out,jnpes_out=',inpes_out,jnpes_out
   print *,' maxouter,maxinner,maxinnerqc,icondition=', &
               maxouter,maxinner,maxinnerqc,icondition
   print *,' iorddata,lhalfges,iordges,one_type=',iorddata,lhalfges,iordges,one_type
   print *,' teststop,grosst=',teststop,grosst
   print *,' grossw,erlon0,erlat0,dlon0=',grossw,erlon0,erlat0,dlon0
   print *,' dlat0,pbot,ptop,bshrinkp,bshrinkt=',dlat0,pbot,ptop,bshrinkp,bshrinkt
   print *,' bshrinkq,bshrinkpsi,bshrinkchi,pshrinkt,pshrinkq=', &
                        bshrinkq,bshrinkpsi,bshrinkchi,pshrinkt,pshrinkq
   print *,' pshrinkpsi,pshrinkchi,bscalep,bscalet,bscaleq=',pshrinkpsi,pshrinkchi,bscalep,bscalet,bscaleq
   print *,' bscalepsi,bscalechi,bscalepred=', &
              bscalepsi,bscalechi,bscalepred
   print *,' delhour,grossp,grosspw,grossq=', &
               delhour,grossp,grosspw,grossq
   print *,' one_pres=',one_pres
   print *,' one_lat,one_lon,one_error,one_obs,one_theta=',one_lat,one_lon,one_error,one_obs,one_theta
   print *,' one_sbot,one_stop,dlonc,dlatc=',one_sbot,one_stop,dlonc,dlatc
   print *,' userad,drift,verify,coldstart,cwm_adjust=', &
               userad,drift,verify,coldstart,cwm_adjust
   print *,' nsat,npred,nhaloc,rnormmax,saterr_inflate,saterr_runfact,npass,nsmooth=', &
              nsat,npred,nhaloc,rnormmax,saterr_inflate,saterr_runfact,npass,nsmooth
   print *,' linear_step,iuniterr,delpmin_fill,obs_space=',linear_step,iuniterr,delpmin_fill,obs_space
   print *,' t_on,p_on,pw_on,w_on,q_on=',t_on,p_on,pw_on,w_on,q_on
   print *,' erradar_inflate=',erradar_inflate
  end if

  lbig2data=(iorddata*(iorddata+3)+2)/2
  lbig2ges=(iordges*(iordges+3)+2)/2
  lbig3ges=(iordges*(iordges*(iordges+6)+11)+6)/6
  if(lhalfges.eq.1) then
   lbig2ges=(iordges+1)**2
   lbig3ges=(iordges+1)**3
  end if
  if(mype.eq.0) then
   print *,' mype, order of interpolation, analysis grid to observations=',mype,iorddata
   print *,' mype, number of interpolating points for 2-d=',mype,lbig2data
  end if

!--------------assign internal grid dimensions to be
!-----------------the 1st odd number greater than the input

!-------- convert input correlation lengths to grid units

  dlon8=2._8*dlon0
  dlat8=2._8*dlat0

!--------  1.  unbufrize data, count and store on temporary files

  nbufdat=10000
  nbufrad=10000
  allocate(alldata(nbufdat))
  allocate(lev_val(nbufdat))
  allocate(lev_max(nbufdat))
  allocate(allrad0(47,nbufrad))
           if(mype.eq.0) write(0,*)' at rdtest in r3dv_bayes, time=',(timef()-time1)*.001
  mbufdat=0
  mbufrad=0

  ithin_goes=100000
  do ii=1,nsat
   if(sattype(ii).eq.'goes') then
    ithin_goes=max(1,min(ithin_goes,isatthin(ii)))
    nelesat(ii)=27
   end if
  end do
  call rdtest(inbufr,nbufdat,mbufdat,nbufrad,mbufrad, &
              iayear,iamonth,iaday,iahour,delhour,userad,drift, &
             erlon0,erlat0,wbglb,sbglb,dlon0,dlat0,imetaglb,jmetaglb, &
        one_type,one_pres,one_lat,one_lon,one_error,one_obs,one_theta,one_sbot,one_stop, &
           t_on,p_on,pw_on,w_on,q_on,bayes,ithin_goes, &
           vadlat,vadlon,vadqm,nvad,npes)
!        if(mype.eq.24) print *,' after rdtest, mype,mbufdat,itype,lon,lat of 28 =', &
!              mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat
  call expand120(nbufdat,mbufdat,mbufdat_out,delpmin_fill,mype)
  mbufdat=mbufdat_out

    idate5(1)=iayear
    idate5(2)=iamonth
    idate5(3)=iaday
    idate5(4)=iahour
    idate5(5)=0
    call w3fs21(idate5,nming)
    gsstm=float(nming)
  call mpi_barrier(my_comm,ierror)
  call rdradar(inradar,nbufdat,mbufdat,iayear,iamonth,iaday,iahour,delhour,gsstm, &
               erlon0,erlat0,wbglb,sbglb,dlon0,dlat0,imetaglb,jmetaglb, &
           vadlat,vadlon,vadqm,nvad,erradar_inflate,npes)
  deallocate(lev_val)
  deallocate(lev_max)
  call mpi_barrier(my_comm,ierror)
!        if(mype.eq.24) print *,' after rdradar, mype,mbufdat,itype,lon,lat of 28 =', &
!              mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat
    if(mype.eq.0) print *,' before setuprad,iayear,month,day,hour,gsstm=', &
             iayear,iamonth,iaday,iahour,gsstm
  if(userad) call read_nonbufsat(nbufrad,mbufrad,erlon0,erlat0, &
                 wbglb,sbglb,dlon0,dlat0,imetaglb,jmetaglb,nsat,mype,npes,delhour,gsstm, &
                 satfile,sattype,isatthin,nelesat,isatid)
  call mpi_barrier(my_comm,ierror)

  call mpi_allreduce(mbufdat,mbufdatall,1,mpi_integer,mpi_sum,my_comm,ierror)
  call mpi_allreduce(mbufrad,mbufradall,1,mpi_integer,mpi_sum,my_comm,ierror)
    if(mbufdat.gt.nbufdat.or.mbufrad.gt.nbufrad) then
      print *,' SOME DATA LOST--INCREASE nbufdat AND/OR nbufrad'
      print *,'  mype,mbufdat,nbufdat=',mype,mbufdat,nbufdat
      print *,'  mype,mbufrad,nbufrad=',mype,mbufrad,nbufrad
      call mpi_finalize(ierr)
      stop
    end if
      if(mype.eq.0) print *,' mbufdatall,mbufradall=',mbufdatall,mbufradall
             
  if(mbufdatall+mbufradall.eq.0) then
    print *,' NO PREPDA DATA AVAILABLE.  3DVAR ANALYSIS FAILS.'
    print *,' NO PREPDA DATA AVAILABLE.  3DVAR ANALYSIS FAILS.'
    print *,' NO PREPDA DATA AVAILABLE.  3DVAR ANALYSIS FAILS.'
    print *,' NO PREPDA DATA AVAILABLE.  3DVAR ANALYSIS FAILS.'
    write(0,*)' NO PREPDA DATA AVAILABLE.  3DVAR ANALYSIS FAILS.'
    write(0,*)' NO PREPDA DATA AVAILABLE.  3DVAR ANALYSIS FAILS.'
    write(0,*)' NO PREPDA DATA AVAILABLE.  3DVAR ANALYSIS FAILS.'
    write(0,*)' NO PREPDA DATA AVAILABLE.  3DVAR ANALYSIS FAILS.'
    stop 15
  end if

!--------  redistribute data on pe's, using information about eta model distribution

           if(mype.eq.0) write(0,*)' at redistribute in r3dv_bayes, time=',(timef()-time1)*.001
!        if(mype.eq.24) print *,' before redistribute_data, mype,mbufdat,itype,lon,lat of 28 =', &
!              mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat
  call mpi_barrier(my_comm,ierror)
  call redistribute_data(mbufdat)
  call mpi_barrier(my_comm,ierror)
  deallocate(alldata)
  nrad_dat=0
  call mpi_barrier(my_comm,ierror)
  if(userad) call redistribute_rad(mbufrad,nsat,nrad_dat)
  call mpi_barrier(my_comm,ierror)
  deallocate(allrad0)
  allocate(rad_dat(max(1,nrad_dat)))
  do i=1,max(1,nrad_dat)
   allocate(rad_dat(i)%pressure(lmetaex+10))
   allocate(rad_dat(i)%iwgts(lbig2ges))
   allocate(rad_dat(i)%wgts(lbig2ges))
   allocate(rad_dat(i)%icx(jpchus))
   allocate(rad_dat(i)%var(jpchus))
   allocate(rad_dat(i)%pred(npred+jpchus-1))
   allocate(rad_dat(i)%obsbt(jpchus))
   allocate(rad_dat(i)%gesbt(jpchus))
   allocate(rad_dat(i)%htlto(3*(lmetaex+10)+1,jpchus))
  end do

!--------  2.  get residuals, sort and thin data, and save again

  allocate(lmh(imeta*jmeta))
  allocate(tetaanl(imeta*jmeta,lmetaex))
  allocate(qetaanl(imeta*jmeta,lmetaex))
  allocate(qsatetaanl(imeta*jmeta,lmetaex))
  allocate(q2etaanl(imeta*jmeta,lmetaex))
  allocate(cwmetaanl(imeta*jmeta,lmetaex))
  allocate(wetaanl(imeta*jmeta,lmetaex,2))    !   1--u, 2--v
  allocate(pdr01etaanl(imeta*jmeta))
  allocate(tetages(imeta*jmeta,lmeta))
  allocate(qetages(imeta*jmeta,lmeta))
  allocate(q2etages(imeta*jmeta,lmeta))
  allocate(cwmetages(imeta*jmeta,lmeta))
  allocate(wetages(imeta*jmeta,lmeta,2))    !   1--u, 2--v
  allocate(pdr01etages(imeta*jmeta))
           if(mype.eq.0) write(0,*)' at get_xbarb in r3dv_bayes, time=',(timef()-time1)*.001
  allocate(allbias_old(jpch))
  allocate(allbias_new(jpch))
  call get_xbarb(imeta,jmeta,imetaglb,jmetaglb,lmeta,lmh,etaiex,etamex, &
       tetaanl,qetaanl,qsatetaanl,q2etaanl,cwmetaanl,wetaanl(1,1,1),wetaanl(1,1,2),pdr01etaanl, &
       tetages,qetages,q2etages,cwmetages,wetages(1,1,1),wetages(1,1,2),pdr01etages, &
       myis2,myie2,myjs2,myje2, &
       dlon0,dlat0,wbglb,sbglb,istagh,istagv,ptop,pbot,lmetaex,erlat0,erlon0, &
       iayear,iamonth,iaday,iahour,delhour,grosst,grossw,grossp,grosspw,grossq, &
       ingesglob,jcapglob,nsigglob,nlonglob,nlathglob,userad, &
       iordges,lbig2ges,lbig3ges,lhalfges, &
       rad_dat,nrad_dat,mrad_dat,npred,nsat,dlon8,dlat8,wb8,sb8, &
       saterr_inflate,saterr_runfact,iuniterr,mpwdata, &
       idiag_rad,iout_rad,sattype,isatid,nelesat,jppf,jpch,jpchus, &
       nangs_max,nbias_loop,nbias_loop1,mbias_loop,model_id, &
       tbias,tcbias,dt_assimilation,cbias_step_clen,allbias_old,allbias_new,npes)


!--------  3.  get analysis increment and add to guess


           if(mype.eq.0) write(0,*)' at r3dvex in r3dv_bayes, time=',(timef()-time1)*.001
  call r3dvex(inpes_out,jnpes_out,verify,lmetaex,erlon0,erlat0,lmh,etaiex,etamex, &
       bshrinkp,bshrinkt,bshrinkq,bshrinkpsi,bshrinkchi, &
       pshrinkt,pshrinkq,pshrinkpsi,pshrinkchi, &
       bscalep,bscalet,bscaleq,bscalepsi,bscalechi,bscalepred,nvmodes,ptop, &
       imeta,jmeta,imetaglb,jmetaglb,lmeta,maxouter,maxinner,maxinnerqc,teststop, &
       tetaanl,qetaanl,qsatetaanl,q2etaanl,cwmetaanl,wetaanl,pdr01etaanl, &
       tetages,qetages,q2etages,cwmetages,wetages,pdr01etages, &
       myis2,myie2,myjs2,myje2, &
       icondition,iayear,iamonth,iaday,iahour, &
       iorddata,lbig2data,iordges,lhalfges,lbig2ges,lbig3ges, &
       wbglb,sbglb,dlon0,dlat0, &
       userad,rad_dat,mrad_dat,npred,nsat,nhaloc, &
       coldstart,cwm_adjust,npass,no_interp,binom,nsmooth,nsmooth_shapiro,ifilt_ord,iuniterr, &
       npassq,no_interpq,nsmoothq,nsmoothq_shapiro,ifilt_ordq,isoq,diagnostic, &
       linear_step,obs_space,dlonc,dlatc,mpwdata, &
           bayes,blambda_in,blambda_out,lambda_in,lambda_out,dlambda,nlambda,diagvar, &
       instats_qx,instats_qf,scale_stats,corlmin,corlmax, &
       iter_restore,iter_smooth,time1,jpch,jpchus,allbias_new,npes)


  deallocate(rad_dat,stat=ierr)
  deallocate(etaiex,stat=ierr) ; deallocate(etamex,stat=ierr)

! if(verify) then
!  call mpi_finalize(ierror)
!  stop
! end if

!--------
!--------  write events to prepda
!--------
!!!   write(0,*)' r3dv--before call errorev, iuniterr: ',iuniterr
!!!   call errorev(inbufr,nfout,iuniterr,mype,npes)

!
!     CALL W3LOG('$E')
      CALL W3TAGE('R3DVAR  ')
!

end if
         if(mype_world.eq.0) &
           write(0,*)' at mpi_finalize in r3dv_bayes, time=',(timef()-time1)*.001
  call mpi_finalize(ierror)

stop
end program r3dv
subroutine test_multio(mype,npes)

!     try out reading from same file independently from different processors

  include 'mpif.h'
         include "my_comm.h"

!   call setrteopts("multconn=yes")
  call mpi_barrier(my_comm,ierr)
  if(mype.eq.0) then
   iwrite=9000+mype
   open(iwrite,file='testfile',form='unformatted',action='write')
   rewind(iwrite)
   do i=0,npes-1
    write(iwrite)i
   end do
   close(iwrite)
  end if
  call mpi_barrier(my_comm,ierr)

  do i=0,npes-1
   iwrite=9000+mype
   if(i.eq.mype) then
    open(iwrite,file='testfile',form='unformatted',action='read')
    rewind(iwrite)
    if(mype.gt.0) then
     do j=0,mype-1
      read(iwrite)
     end do
    end if
    read(iwrite)k
    print *,' mype,k=',mype,k
    close(iwrite)
   end if
  end do
  call mpi_barrier(my_comm,ierr)
  
!   call setrteopts("multconn=no")
! if(mype.gt.-1000) then
!  call mpi_finalize(ierr)
!  stop
! end if

return
end subroutine test_multio
subroutine assign_color(icolor,icolorpe,tasks_per_node_available,tasks_per_node_desired, &
                        mype,npes,npes_all)

!  assign color for sub-communicator so that we get desired number of tasks per node

!  if tasks_per_node_desired < 0, then assign icolor so
!    as to minimize number of tasks per node ( try every 4, then every 3, then every 2, and
!            finally every 1, until get number which fits in total number of processors

  include 'mpif.h'

  implicit none

  integer(4) icolor,icolorpe
  integer(4) tasks_per_node_available,tasks_per_node_desired,mype,npes,npes_all

  integer(4) icolor0(0:npes_all-1),icolorpe0(0:npes_all-1)
  integer(4) i,icount,icount0,ierr,n_per_node


!  first figure out on processor zero

if(mype.eq.0) then
 icount0=0
 if(tasks_per_node_desired.gt.0) then
  icount=0
  do i=0,npes_all-1
   icolor0(i)=1
   if(mod(i,tasks_per_node_available).lt.tasks_per_node_desired) then
    icount=icount+1
    if(icount.le.npes) then
     icolor0(i)=0
     icount0=icount0+1
    end if
   end if
  end do
 end if
 if(icount0.gt.0.and.icount0.ne.npes) then
   print *,' tasks_per_node_desired cannot be achieved out of total nodes available'
   print *,'  will assign mimimum allowable tasks per node'
 end if
 if(icount.eq.0) then
   print *,' assign minumum allowable tasks per node'
 end if
 if(icount.eq.0.or.icount0.ne.npes) then
  do n_per_node=1,tasks_per_node_available
   icount=0
   icount0=0
   do i=0,npes_all-1
    icolor0(i)=1
    if(mod(i,tasks_per_node_available).lt.n_per_node) then
     icount=icount+1
     if(icount.le.npes) then
      icolor0(i)=0
      icount0=icount0+1
     end if
    end if
   end do
   if(icount0.eq.npes) then
    print *,' minimum allowable tasks per node = ',n_per_node
    exit
   end if
  end do
 end if
 icount=-1
 do i=0,npes_all-1
  if(icolor0(i).eq.0) then
   icount=icount+1
   icolorpe0(i)=icount
  end if
 end do
 do i=0,npes_all-1
  if(icolor0(i).eq.1) then
   icount=icount+1
   icolorpe0(i)=icount
  end if
 end do
 do i=0,npes_all-1
  print *,' icolor0,icolorpe0(',i,')=',icolor0(i),icolorpe0(i)
 end do
end if
call mpi_bcast(icolor0,npes_all,mpi_integer4,0,mpi_comm_world,ierr)
call mpi_bcast(icolorpe0,npes_all,mpi_integer4,0,mpi_comm_world,ierr)
icolor=icolor0(mype)
icolorpe=icolorpe0(mype)
!if(icolor.eq.1) icolor=mpi_undefined
print *,' mype,icolorpe,icolor=',mype,icolorpe,icolor

return
end subroutine assign_color
