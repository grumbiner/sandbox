subroutine r3dvex(inpes_out,jnpes_out,verify,lmetaex,erlon0,erlat0,lmh,etaiex,etamex, &
    bshrinkp,bshrinkt,bshrinkq,bshrinkpsi,bshrinkchi, &
    pshrinkt,pshrinkq,pshrinkpsi,pshrinkchi, &
    bscalep,bscalet,bscaleq,bscalepsi,bscalechi,bscalepred,nvmodes,ptop, &
    imeta,jmeta,imetaglb,jmetaglb,lmeta,maxouter,maxinner,maxinnerqc,teststop, &
    tetaanl,qetaanl,qsatetaanl,q2etaanl,cwmetaanl,wetaanl,petaanl, &
    tetages,qetages,q2etages,cwmetages,wetages,petages, &
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

!????????????????????????????/
!???look carefully at????????/
!?????where to have virtual/sensible temperature??????????
!??????????everywhere-- data is virtual T, to get qsat, want to use sensible T
!????????????//   in eta model, always sensible T
!    ???????/  virtual T only appears in hydrostatic integration for height from T, Zsfc and PD
!??????????????????
!????? the way i will handle it is to let Tv be the analysis control variable, and always
!?????  convert back to T when it is time to do model time steps for the div tend stuff

  include "mpif.h"
      include "my_comm.h"
  include 'types.h'
  include 'filtertype.h'
  include 'satbias_type.h'

  type(satbias_stuff) allbias_new(jpch)

  logical verify,bayes,diagvar,binom,isoq,diagnostic

  real(4) blambda_in(nlambda,nlambda),blambda_out(nlambda,nlambda),lambda_in(nlambda)
  real(4) dlambda(nlambda),lambda_out(nlambda)

! on input to r3dvex, tetaanl, etc contain the vertically extended
!    eta ges with holes filled under the steps

  integer(4) lmh(imeta,jmeta)
  real(4) tetaanl(imeta,jmeta,lmetaex)
  real(4) qetaanl(imeta,jmeta,lmetaex)
  real(4) qsatetaanl(imeta,jmeta,lmetaex)
  real(4) q2etaanl(imeta,jmeta,lmetaex)
  real(4) cwmetaanl(imeta,jmeta,lmetaex)
  real(4) wetaanl(imeta,jmeta,lmetaex,2)
  real(4) petaanl(imeta,jmeta)
  real(4) tetages(imeta,jmeta,lmeta)
  real(4) qetages(imeta,jmeta,lmeta)
  real(4) q2etages(imeta,jmeta,lmeta)
  real(4) cwmetages(imeta,jmeta,lmeta)
  real(4) wetages(imeta,jmeta,lmeta,2)
  real(4) petages(imeta,jmeta)

  logical obs_space
  logical coldstart,userad,cwm_adjust,linear_step
  character(4) on85dt(8)
  real(4) etaiex(lmetaex+1),etamex(lmetaex)
  type(rad_obs) rad_dat(max(1,mrad_dat))
  
!--------
!---------  yo:  (normalized observations)
!--------
  real(4),allocatable::yot(:),yow(:),yowr(:),yoq(:)
  real(4),allocatable::yopw(:),yop(:),yorad(:)
!--------
!---------  xbarb:  (normalized background at observations)
!--------
  real(4),allocatable::xbarbt(:),xbarbw(:),xbarbwr(:,:),xbarbq(:)
  real(4),allocatable::xbarbpw(:),xbarbp(:),xbarbrad(:),qsatges(:)
!--------
!--------  Eeta:  (interpolation from coarse grid to full eta grid (horizontal only)
!--------
  real(4),allocatable::bigeetah(:,:),bigeetav(:,:,:)
  integer(4),allocatable::ibigeetah(:,:),ibigeetav(:,:)
!--------
!--------  Heta:  (interpolation from full eta grid to coarse grid (horizontal only)
!--------
  real(4),allocatable::bighetah(:,:),bighetav(:,:)
  integer(4),allocatable::ibighetah(:,:),ibighetav(:,:)
!--------
!--------  Ht_psi, Hpdr01_psi: psi is balanced variable. These define balanced part of T, pdr01
!--------
  real(4),allocatable::bight_psi(:,:,:),bighp_psi(:,:)
  real(4),allocatable::wgts_bightp(:,:)
  integer(4),allocatable::iwgts_bightp(:,:)
!--------
!--------  H:  (linear forward models)
!--------
  real(4),allocatable::bight(:,:)
  integer(4),allocatable::ibight(:,:)

  real(4),allocatable::bighw(:,:,:)
  integer(4),allocatable::ibighw(:,:)

  real(4),allocatable::bighwr(:,:,:)
  integer(4),allocatable::ibighwr(:,:)

  real(4),allocatable::bighq(:,:)
  integer(4),allocatable::ibighq(:,:)

  real(4),allocatable::bighpw(:,:,:)
  integer(4),allocatable::ibighpw(:,:)

  real(4),allocatable::bighp(:,:)
  integer(4),allocatable::ibighp(:,:)

  real(4),allocatable::bighradh(:,:),bighradv(:,:)
  integer(4),allocatable::ibighradh(:,:)
  integer(4),allocatable::icxrad(:,:)
  real(4),allocatable::predrad(:,:),varpred(:,:)

!--------
!--------   observation errors
!--------
  real(4),allocatable::eyot0(:),eyow0(:),eyowr0(:),eyoq0(:)
  real(4),allocatable::eyopw0(:),eyop0(:),eyorad0(:)

!--------  where to evaluate background variance

  real(4) varlats(5),varpres(5)
  integer(4) ivart(5,5),ivaru(5,5),ivarv(5,5),ivarq(5,5),ivarpw(5,5),ivarp(5),ivarrad(5,jpch)
  data varlats/15.,30.,45.,60.,75./
  data varpres/900.,700.,500.,300.,100./
  data nvarlats/5/,nvarpres/5/

!-------- background error structures

  type(filter_cons) isofilter_w(14,2)                    !  1--psi, 2--chi
  type(filter_cons) isofilter_t(14),isofilter_q(14)
  type(filter_cons) isofilter_p(14)

!   1--h, 2--t, 3--q, 4--psi, 5--chi

  real(4),allocatable::predinc(:,:)

!--------
!-------- adjusted background errors
!--------
  real(4),allocatable::epsi(:,:),echi(:,:),et(:,:),eq(:,:),ep(:)
  real(4),allocatable::rlenxyt(:),rlenxyq(:),rlenxypsi(:)
  real(4),allocatable::rlenxychi(:),rlenpt(:),rlenpq(:)
  real(4),allocatable::rlenppsi(:),rlenpchi(:)
  real(4),allocatable::rxcglb(:),rycglb(:)
  real(4),allocatable::rxc(:),ryc(:)
  real(8) r1_8
  integer(4),allocatable::pe_of_injn(:,:),in_of_i(:),jn_of_j(:)
!--------
!-------- quality markers (=1 for bad data); 'analysis weights'
!--------
  integer, allocatable::iq(:),it(:),iw(:),ip(:),ipw(:)
  real(4), allocatable::wtq(:,:),wtt(:,:),wtw(:,:),wtp(:,:),wtpw(:,:)
!________
!-------- basic observation data
!--------
  real(4),allocatable::psdata(:,:)
  real(4),allocatable::pwdata(:,:)
  real(4),allocatable::qdata(:,:)
  real(4),allocatable::tdata(:,:)
  real(4),allocatable::wdata(:,:)
  logical,allocatable::psfw(:)
  logical,allocatable::pwfw(:)
  logical,allocatable::qfw(:)
  logical,allocatable::tfw(:)
  logical,allocatable::radfw(:)
  logical,allocatable::wfw(:)
  character(8),allocatable::pstaid(:)
  character(8),allocatable::pwstaid(:)
  character(8),allocatable::qstaid(:)
  character(8),allocatable::tstaid(:)
  character(8),allocatable::wstaid(:)
  real(4),allocatable::xeta(:,:),xetam(:,:)
  real(4),allocatable::yeta(:,:),yetam(:,:)

!-------- aspect tensors

  real(4),allocatable::aspectq(:,:,:,:),ampq(:,:,:)

!-- temporary storage for coarse grid smoothed background, used to construct anisotropic aspect tensor

   real(4),allocatable::tg(:,:,:),ug(:,:,:),vg(:,:,:),qg(:,:,:),pg(:,:)
   integer(4),allocatable::ksfc(:,:)

   integer(4) iplot(20,20),jplot(20,20),kplot(20,20)

!--------

  call mpi_comm_rank(my_comm,mype,ierr)

  ifh=1 ; ift=2 ; ifq=3 ; ifpsi=4 ; ifchi=5


!--------
!-------- get grid stuff
!--------
!---------------- eta grid first
  allocate(xeta(imeta,jmeta+1)) ; allocate(yeta(imeta,jmeta+1))
  allocate(xetam(imeta,jmeta+1)) ; allocate(yetam(imeta,jmeta+1))
  call getetagrid(xeta,yeta,xetam,yetam,imeta,jmeta,lmeta)
  xetamax=maxval(xeta) ; xetamin=minval(xeta)
  yetamax=maxval(yeta) ; yetamin=minval(yeta)
  call mpi_allreduce(xetamax,xetaglbmax,1,mpi_real4,mpi_max,my_comm,ierr)
  call mpi_allreduce(xetamin,xetaglbmin,1,mpi_real4,mpi_min,my_comm,ierr)
  call mpi_allreduce(yetamax,yetaglbmax,1,mpi_real4,mpi_max,my_comm,ierr)
  call mpi_allreduce(yetamin,yetaglbmin,1,mpi_real4,mpi_min,my_comm,ierr)
!-------------- now coarse grid
  nxcglb=1+iorddata+ceiling((xetaglbmax-xetaglbmin)/dlonc)
  nycglb=1+iorddata+ceiling((yetaglbmax-yetaglbmin)/dlatc)
  allocate(rxcglb(nxcglb))
  do i=1,nxcglb
   rxcglb(i)=xetaglbmin+dlonc*(i-1.)
  end do
  shift=.5*(rxcglb(nxcglb)-xetaglbmax)
  rxcglb=rxcglb-shift
  if(mype.eq.0) print *,' xetaglbmin-rxcglb(1)=',xetaglbmin-rxcglb(1)
  if(mype.eq.0) print *,' rxcglb(nxcglb)-xetaglbmax=',rxcglb(nxcglb)-xetaglbmax
  allocate(rycglb(nycglb))
  do i=1,nycglb
   rycglb(i)=yetaglbmin+dlatc*(i-1.)
  end do
  shift=.5*(rycglb(nycglb)-yetaglbmax)
  rycglb=rycglb-shift
  if(mype.eq.0) print *,' yetaglbmin-rycglb(1)=',yetaglbmin-rycglb(1)
  if(mype.eq.0) print *,' rycglb(nycglb)-yetaglbmax=',rycglb(nycglb)-yetaglbmax
        if(mype.eq.0) print *,' xetaglbmin=',xetaglbmin
        if(mype.eq.0) print *,' xetaglbmax=',xetaglbmax
        if(mype.eq.0) print *,' yetaglbmin=',yetaglbmin
        if(mype.eq.0) print *,' yetaglbmax=',yetaglbmax
        if(mype.eq.0) print *,' nxcglb,nycglb=',nxcglb,nycglb
  halo_widthc=nhaloc*dlatc
            if(mype.eq.0) write(0,*)' at 1 in r3dvex, time=',(timef()-time1)*.001
  call mpp_compgrid_init(rxcglb,rycglb,halo_widthc,nxcglb,nycglb,nxc,nyc,2)
  allocate(rxc(nxc)) ; allocate(ryc(nyc))
            if(mype.eq.0) write(0,*)' at 2 in r3dvex, time=',(timef()-time1)*.001
  call mpp_compgrid_retrieve(rxc,ryc,nxc,nyc,2)
            if(mype.eq.0) write(0,*)' at 3 in r3dvex, time=',(timef()-time1)*.001
  call get_gridlims(myxsc,myxec,myysc,myyec,myxsc_glb,myxec_glb,myysc_glb,myyec_glb,2)

!   compute the interpolation operator bigeeta, which is used to go back and forth
!     between coarse grid and full eta grid

            if(mype.eq.0) write(0,*)' at 4 in r3dvex, time=',(timef()-time1)*.001
  allocate(bigeetah(imeta*jmeta,lbig2data))
  allocate(bigeetav(imeta*jmeta,lbig2data,2))      !   1--x, 2--y
  allocate(ibigeetah(imeta*jmeta,lbig2data))
  allocate(ibigeetav(imeta*jmeta,lbig2data))
            if(mype.eq.0) write(0,*)' at 5 in r3dvex, time=',(timef()-time1)*.001
  call getbigeeta(bigeetah,bigeetav(1,1,1),bigeetav(1,1,2),ibigeetah,ibigeetav, &
                  xeta,yeta,xetam,yetam,imeta,jmeta,iorddata,lbig2data, &
                  rxcglb,rycglb,nxcglb,nycglb,nxc,nyc,myis2,myie2,myjs2,myje2,mype)

!   compute the interpolation operator bigheta, which is used to go form full eta grid
!      to coarse grid

            if(mype.eq.0) write(0,*)' at 5.1 in r3dvex, time=',(timef()-time1)*.001
  allocate(bighetah(nxc*nyc,lbig2ges))
  allocate(bighetav(nxc*nyc,lbig2ges))      !   1--x, 2--y
  allocate(ibighetah(nxc*nyc,lbig2ges))
  allocate(ibighetav(nxc*nyc,lbig2ges))
            if(mype.eq.0) write(0,*)' at 5.2 in r3dvex, time=',(timef()-time1)*.001
  call getbigheta(bighetah,bighetav,ibighetah,ibighetav,rxc,ryc,nxc,nyc, &
                  iordges,lhalfges,lbig2ges,wbglb,dlon0,sbglb,dlat0,imeta,jmeta,imetaglb,jmetaglb,mype)

!    bring in background error info and get corlens,variances, and slow variable operators

  rewind 9
  read(9,err=100,end=100)nlath,mmeta,lat1,lat2
  go to 110
100 continue
    write(0,*)' BAD BACKGROUND ERROR FILE--eta 3dvar fails'
    stop
110 continue
  if(mmeta.ne.lmeta) then
    write(0,*)' BAD OR WRONG BACKGROUND ERROR FILE--eta 3dvar fails'
    stop
  end if
     if(mype.eq.0) print *,' from background err stats file, nlath,lmeta,lat1,lat2=',nlath,mmeta,lat1,lat2
     if(mype.eq.0) print *,'  lmeta,lmetaex=',lmeta,lmetaex,' --- should be equal'
  allocate(bight_psi(lmeta,lmeta,lat1:lat2))
  allocate(bighp_psi(lmeta,lat1:lat2))
  allocate(wgts_bightp(2,nxc*nyc))
  allocate(iwgts_bightp(2,nxc*nyc))
  allocate(et(lmeta,lat1:lat2))
  allocate(eq(lmeta,lat1:lat2))
  allocate(epsi(lmeta,lat1:lat2))
  allocate(echi(lmeta,lat1:lat2))
  allocate(ep(lat1:lat2))
  allocate(rlenxyt(lmeta))
  allocate(rlenxyq(lmeta))
  allocate(rlenxypsi(lmeta))
  allocate(rlenxychi(lmeta))
  allocate(rlenpt(lmeta))
  allocate(rlenpq(lmeta))
  allocate(rlenppsi(lmeta))
  allocate(rlenpchi(lmeta))
  call getbkgerr(bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
                 et,eq,epsi,echi,ep, &
                 rlenxyt,rlenxyq,rlenxypsi,rlenxychi,rlenxyp, &
                 rlenpt,rlenpq,rlenppsi,rlenpchi, &
                 bscalep,bscalet,bscaleq,bscalepsi,bscalechi, &
                 nlath,lat1,lat2,rxc,ryc,erlat0,nxc,nyc,lmetaex,dlatc,ptop,etaiex,etamex,mype)

            if(mype.eq.0) write(0,*)' at 6 in r3dvex, time=',(timef()-time1)*.001

!--------  obtain WRF-style grid index constants (for now need only for RAF package)

  call get_wrf_cons_0(myxsc,myxec,myysc,myyec, &
           myxsc_glb,myxec_glb,myysc_glb,myyec_glb,nxc,nyc,lmetaex,nxcglb,nycglb, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes )                               ! processor info

  allocate(pe_of_injn(inpes,jnpes))
  allocate(in_of_i(ids:ide))
  allocate(jn_of_j(jds:jde))
            if(mype.eq.0) write(0,*)' at 9 in r3dvex, time=',(timef()-time1)*.001
  call get_wrf_cons_1( &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info


!  start final data preparation

  call count_qs(mqdata)
  call count_sfcps(mpdata)
  call count_temps(mtdata)
  call count_windcr(mwdata,mwrdata)
  mraddata=0
  if(userad) call count2_rads(rad_dat,mrad_dat,mraddata)
  call mpi_allreduce(mqdata,mqdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
  call mpi_allreduce(mpdata,mpdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
  call mpi_allreduce(mpwdata,mpwdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
  call mpi_allreduce(mtdata,mtdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
  call mpi_allreduce(mwdata,mwdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
  call mpi_allreduce(mwrdata,mwrdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
  call mpi_allreduce(mraddata,mraddataall,1,mpi_integer,mpi_sum,my_comm,ierr)

  if(mype.eq.0) write(0,*)' r3dvex--mpdata,mpwdata,mqdata,mtdata,mwdata,mwrdata: ', &
                 mpdataall,mpwdataall,mqdataall,mtdataall,mwdataall,mwrdataall
!--------
!-------- set up yo, xbarb, H
!--------
  allocate(varpred(jpch,npred))
  varpred=sqrt(10.)*bscalepred
  allocate(yorad(max(1,mraddata)))
  allocate(xbarbrad(max(1,mraddata)))
  allocate(bighradh(lbig2ges,max(1,mraddata)))
  allocate(bighradv(2*lmetaex,max(1,mraddata)))
  allocate(ibighradh(lbig2ges,max(1,mraddata)))
  allocate(icxrad(2,max(1,mraddata)))
  allocate(predrad(npred+jpchus-1,max(1,mraddata)))
  allocate(eyorad0(max(1,mraddata)))

       raderrmax=-1.e20
       raderrmin=1.e20
            if(mype.eq.0) write(0,*)' before lastgetrads in r3dvex, time=',(timef()-time1)*.001
  ivarrad=0
  if(userad.and.mraddataall.gt.0) then
   call lastgetrads(yorad,eyorad0,xbarbrad,bighradh,bighradv,ibighradh,mraddata, &
         icxrad,predrad,npred,lbig2ges,lmetaex,rad_dat,mrad_dat,mype,ivarrad,varlats, &
                 nvarlats,jpch,jpchus,npes)
      if(mraddata.gt.0) then
       do i=1,mraddata
        raderrmax=max(eyorad0(i),raderrmax)
        raderrmin=min(eyorad0(i),raderrmin)
       end do
      end if
           call dot_prod8(r1_8,yorad,yorad,mraddata)
           if(mype.eq.0) write(6,*)' after lastgetrads, yoraddotyorad=',r1_8
           call dot_prod8(r1_8,xbarbrad,xbarbrad,mraddata)
           if(mype.eq.0) write(6,*)' after lastgetrads, xbarbraddotxbarbrad=',r1_8
     call mpi_reduce(raderrmax,raderrmaxall,1,mpi_real,mpi_max,0,my_comm,ier)
     call mpi_reduce(raderrmin,raderrminall,1,mpi_real,mpi_min,0,my_comm,ier)
     if(mype.eq.0) &
       print *,' after lastgetrads in r3dvex, mraddata,raderrmax,min=', &
                mraddataall,raderrmaxall,raderrminall
  end if

            if(mype.eq.0) write(0,*)' at 15 in r3dvex, time=',(timef()-time1)*.001
!     print *,' mype,mtdata=',mype,mtdata
  allocate(tdata(max(1,mtdata),6))
  allocate(tstaid(max(1,mtdata)))
  allocate(yot(max(1,mtdata)))
  allocate(xbarbt(max(1,mtdata)))
  allocate(bight(lbig3ges,max(1,mtdata)))
  allocate(ibight(lbig3ges,max(1,mtdata)))
  allocate(eyot0(max(1,mtdata)))
  ivart=0
  if(mtdataall.gt.0) &
   call lastgettemps(yot,eyot0,xbarbt,bight,ibight,mtdata,lbig3ges,tdata,tstaid,ivart, &
                     varlats,nvarlats,varpres,nvarpres,npes)

            if(mype.eq.0) write(0,*)' at 16 in r3dvex, time=',(timef()-time1)*.001
  allocate(psdata(max(1,mpdata),6))
  allocate(pstaid(max(1,mpdata)))
  allocate(yop(max(1,mpdata)))
  allocate(xbarbp(max(1,mpdata)))
  allocate(bighp(lbig2ges,max(1,mpdata)))
  allocate(ibighp(lbig2ges,max(1,mpdata)))
  allocate(eyop0(max(1,mpdata)))
  ivarp=0
  if(mpdataall.gt.0) &
    call lastgetps(yop,eyop0,xbarbp,bighp,ibighp,mpdata,lbig2ges,psdata,pstaid,ivarp, &
                      varlats,nvarlats,npes)

            if(mype.eq.0) write(0,*)' at 17 in r3dvex, time=',(timef()-time1)*.001

  allocate(wdata(max(1,mwdata),6))
  allocate(wstaid(max(1,mwdata)))
  allocate(yow(max(1,mwdata)))
  allocate(xbarbw(max(1,mwdata)))
  allocate(bighw(lbig3ges,2,max(1,mwdata)))
  allocate(ibighw(lbig3ges,max(1,mwdata)))
  allocate(eyow0(max(1,mwdata)))
  ivaru=0
  ivarv=0
  if(mwdataall.gt.0) &
    call lastgetwinds(yow,eyow0,xbarbw,bighw,ibighw,mwdata,lbig3ges,wdata,wstaid,ivaru,ivarv, &
                      varlats,nvarlats,varpres,nvarpres)

  allocate(yowr(max(1,mwrdata)))
  allocate(xbarbwr(lmetaex,max(1,mwrdata)))
  allocate(bighwr(lbig2ges,2,max(1,mwrdata)))
  allocate(ibighwr(lbig2ges+2,max(1,mwrdata)))
  allocate(eyowr0(max(1,mwrdata)))
  if(mwrdataall.gt.0) &
    call lastget_radar_winds(yowr,eyowr0,xbarbwr,bighwr,ibighwr,mwrdata, &
                             lmetaex,lbig2ges,wetages,imeta,jmeta)
if(mype.eq.0) write(0,*)' at 18 in r3dvex, time=',(timef()-time1)*.001
              qerrmax=-1.e20
              qerrmin=1.e20
  allocate(qdata(max(1,mqdata),6))
  allocate(qstaid(max(1,mqdata)))
  allocate(yoq(max(1,mqdata)))
  allocate(xbarbq(max(1,mqdata)))
  allocate(qsatges(max(1,mqdata)))
  allocate(bighq(lbig3ges,max(1,mqdata)))
  allocate(ibighq(lbig3ges,max(1,mqdata)))
  allocate(eyoq0(max(1,mqdata)))
  ivarq=0
  if(mqdataall.gt.0) &
    call lastgetqs(yoq,eyoq0,xbarbq,qsatges,bighq,ibighq,mqdata,lbig3ges,qdata,qstaid,ivarq, &
                      varlats,nvarlats,varpres,nvarpres,npes)
          if(mqdata.gt.0) then
           do i=1,mqdata
            qerrmax=max(eyoq0(i),qerrmax)
            qerrmin=min(eyoq0(i),qerrmin)
           end do
          end if
     call mpi_reduce(qerrmax,qerrmaxall,1,mpi_real,mpi_max,0,my_comm,ier)
     call mpi_reduce(qerrmin,qerrminall,1,mpi_real,mpi_min,0,my_comm,ier)
     if(mype.eq.0) then
      if(mqdataall.gt.0) &
       print *,' after lastgetqs in r3dvex, qerrmax,min=',qerrmaxall,qerrminall
     end if


            if(mype.eq.0) write(0,*)' at 19 in r3dvex, time=',(timef()-time1)*.001
       pwerrmax=-1.e20
       pwerrmin=1.e20
  allocate(pwdata(max(1,mpwdata),6))
  allocate(pwstaid(max(1,mpwdata)))
  allocate(yopw(max(1,mpwdata)))
  allocate(xbarbpw(max(1,mpwdata)))
  allocate(bighpw(lmetaex,lbig2ges,max(1,mpwdata)))
  allocate(ibighpw(lbig2ges,max(1,mpwdata)))
  allocate(eyopw0(max(1,mpwdata)))
  if(mpwdataall.gt.0) &
    call lastgetpws(yopw,eyopw0,xbarbpw,bighpw,ibighpw,mpwdata,lbig2ges,lmetaex,pwdata,pwstaid)
 !   if(mype.eq.0) then
 !    print *,' after lastgetpws, mype,max,min(yopw)=',mype,maxval(yopw),minval(yopw)
 !    print *,' after lastgetpws, mype,max,min(eyopw0)=',mype,maxval(eyopw0),minval(eyopw0)
 !    print *,' after lastgetpws, mype,max,min(xbarbpw)=',mype,maxval(xbarbpw),minval(xbarbpw)
 !    print *,' after lastgetpws, mype,max,min(bighpw)=',mype,maxval(bighpw),minval(bighpw)
 !    print *,' after lastgetpws, mype,ijmeta,max,min(ibighpw)=', &
 !                          mype,imeta*jmeta,maxval(ibighpw),minval(ibighpw)
 !   end if
     call mpi_reduce(pwerrmax,pwerrmaxall,1,mpi_real,mpi_max,0,my_comm,ier)
     call mpi_reduce(pwerrmin,pwerrminall,1,mpi_real,mpi_min,0,my_comm,ier)
     if(mype.eq.0) then
      if(mpwdataall.gt.0) &
       print *,' after lastgetpws in r3dvex, pwerrmax,min=',pwerrmaxall,pwerrminall
     end if


!--------
!-------- normalize everything by obs errors
!--------

            if(mype.eq.0) write(0,*)' at 20 in r3dvex, time=',(timef()-time1)*.001
  if(userad) then
           call dot_prod8(r1_8,yorad,yorad,mraddata)
           if(mype.eq.0) write(6,*)' before reducesrad, yoraddotyorad=',r1_8
           call dot_prod8(r1_8,xbarbrad,xbarbrad,mraddata)
           if(mype.eq.0) write(6,*)' before reducesrad, xbarbraddotxbarbrad=',r1_8
    call obscorr2t(eyorad0,mraddata,bighradh,yorad,xbarbrad,lbig2ges)
           call dot_prod8(r1_8,yorad,yorad,mraddata)
           if(mype.eq.0) write(6,*)' after reducesrad, yoraddotyorad=',r1_8
           call dot_prod8(r1_8,xbarbrad,xbarbrad,mraddata)
           if(mype.eq.0) write(6,*)' after reducesrad, xbarbraddotxbarbrad=',r1_8
  end if



   call obscorr2t(eyot0,mtdata,bight,yot,xbarbt,lbig3ges)

   call obscorr2t(eyop0,mpdata,bighp,yop,xbarbp,lbig2ges)

   call obscorr2t(eyopw0,mpwdata,bighpw,yopw,xbarbpw,lmetaex*lbig2ges)

   call obscorr2t(eyow0,mwdata,bighw,yow,xbarbw,2*lbig3ges)

   call obscorr2wr(eyowr0,mwrdata,bighwr,yowr,xbarbwr,lbig2ges,lmetaex)

            if(mype.eq.0) write(0,*)' at 20.1 in r3dvex, time=',(timef()-time1)*.001
!----------
!---------- convert moisture obs errors from rel hum to sp hum
!----------  (bound below by .1 g/kg)
!----------
     qsatgesmax=-1.e20 ; qsatgesmin=1.e20
     eyoq0max_b=-1.e20 ; eyoq0min_b=1.e20
     eyoq0max_a=-1.e20 ; eyoq0min_a=1.e20
     yoqmax_b=-1.e20 ; yoqmin_b=1.e20
     yoqmax_a=-1.e20 ; yoqmin_a=1.e20
     xbarbqmax_b=-1.e20 ; xbarbqmin_b=1.e20
     xbarbqmax_a=-1.e20 ; xbarbqmin_a=1.e20
  if(mqdata.gt.0) then
      qsatgesmax=maxval(qsatges) ; qsatgesmin=minval(qsatges)
      eyoq0max_b=maxval(eyoq0) ; eyoq0min_b=minval(eyoq0)
   do i=1,mqdata
    eyoq0(i)=max(eyoq0(i)*qsatges(i),1.e-10)
   end do
      eyoq0max_a=maxval(eyoq0) ; eyoq0min_a=minval(eyoq0)
      yoqmax_b=maxval(yoq) ; yoqmin_b=minval(yoq)
      xbarbqmax_b=maxval(xbarbq) ; xbarbqmin_b=minval(xbarbq)
  end if
  deallocate(qsatges)
   call obscorr2t(eyoq0,mqdata,bighq,yoq,xbarbq,lbig3ges)
      if(mqdata.gt.0) then
       yoqmax_a=maxval(yoq) ; yoqmin_a=minval(yoq)
       xbarbqmax_a=maxval(xbarbq) ; xbarbqmin_a=minval(xbarbq)
      end if
      if(mqdataall.gt.0) then
       call mpi_reduce(qsatgesmax,qsatgesmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
       call mpi_reduce(qsatgesmin,qsatgesminall,1,mpi_real,mpi_min,0,my_comm,ierror)
       call mpi_reduce(eyoq0max_b,eyoq0max_ball,1,mpi_real,mpi_max,0,my_comm,ierror)
       call mpi_reduce(eyoq0min_b,eyoq0min_ball,1,mpi_real,mpi_min,0,my_comm,ierror)
       call mpi_reduce(eyoq0max_a,eyoq0max_aall,1,mpi_real,mpi_max,0,my_comm,ierror)
       call mpi_reduce(eyoq0min_a,eyoq0min_aall,1,mpi_real,mpi_min,0,my_comm,ierror)
       call mpi_reduce(yoqmax_b,yoqmax_ball,1,mpi_real,mpi_max,0,my_comm,ierror)
       call mpi_reduce(yoqmin_b,yoqmin_ball,1,mpi_real,mpi_min,0,my_comm,ierror)
       call mpi_reduce(yoqmax_a,yoqmax_aall,1,mpi_real,mpi_max,0,my_comm,ierror)
       call mpi_reduce(yoqmin_a,yoqmin_aall,1,mpi_real,mpi_min,0,my_comm,ierror)
       call mpi_reduce(xbarbqmax_b,xbarbqmax_ball,1,mpi_real,mpi_max,0,my_comm,ierror)
       call mpi_reduce(xbarbqmin_b,xbarbqmin_ball,1,mpi_real,mpi_min,0,my_comm,ierror)
       call mpi_reduce(xbarbqmax_a,xbarbqmax_aall,1,mpi_real,mpi_max,0,my_comm,ierror)
       call mpi_reduce(xbarbqmin_a,xbarbqmin_aall,1,mpi_real,mpi_min,0,my_comm,ierror)
       if(mype.eq.0) then
        print'('' max,min qsatges= '',2g13.5)',qsatgesmaxall,qsatgesminall
        print'('' max,min eyoq0 before coversion= '',2g13.5)',eyoq0max_ball,eyoq0min_ball
        print'('' max,min eyoq0 after coversion= '',2g13.5)',eyoq0max_aall,eyoq0min_aall
        print'('' before reduces, qobs max,min= '',2g13.5)',yoqmax_ball,yoqmin_ball
        print'('' before reduces, qges max,min= '',2g13.5)',xbarbqmax_ball,xbarbqmin_ball
        print'('' after reduces, qobs max,min= '',2g13.5)',yoqmax_aall,yoqmin_aall
        print'('' after reduces, qges max,min= '',2g13.5)',xbarbqmax_aall,xbarbqmin_aall
       end if
      end if
            if(mype.eq.0) write(0,*)' at 20.2 in r3dvex, time=',(timef()-time1)*.001

!     do we want to do bayesian maximum-likelihood parameter estimation?

! if(bayes) &
!  call maxl_parameter_update_bayes(blambda_in,blambda_out,lambda_in,lambda_out,dlambda,nlambda, &
!              rlenxyp,rlenxyt,rlenxyq,rlenxypsi,rlenxychi, &         !   1-5
!              rlenpt,rlenpq,rlenppsi,rlenpchi, &                     !   6-9
!              ep,et,eq,epsi,echi, &                                  !  10-14
!       tetaanl,qetaanl,q2etaanl,cwmetaanl,wetaanl,petaanl, &
!       tetages,qetages,q2etages,cwmetages,wetages,petages, &
!       iayear,iamonth,iaday,iahour,coldstart,cwm_adjust, &
!       maxinner,maxouter,teststop,npass,nsmooth, &
!       yoq,xbarbq,mqdata,mqdataall,ibighq,bighq, &
!       yopw,xbarbpw,mpwdata,mpwdataall,ibighpw,bighpw, &
!       yow,xbarbw,mwdata,mwdataall,ibighw,bighw, &
!       yop,xbarbp,mpdata,mpdataall,ibighp,bighp, &
!       yot,xbarbt,mtdata,mtdataall,ibight,bight, &
!       yorad,xbarbrad,mraddata,mraddataall,ibighradh,bighradh,bighradv, &
!               predrad,icxrad,varpred,npred,userad, &
!       lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
!       lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
!       nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec, &
!       myxsc_glb,myysc_glb,nxcglb,nycglb,erlat0,rxc,ryc,jpch,jpchus, &
!            ids, ide, jds, jde, kds, kde, &                          ! domain indices
!            ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
!            ims, ime, jms, jme, kms, kme, &                          ! memory indices
!            inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

! call update_bkgerr_bayes(rlenxyp,rlenxyt,rlenxyq,rlenxypsi,rlenxychi, &
!                    rlenpt,rlenpq,rlenppsi,rlenpchi, &
!                    ep,et,eq,epsi,echi, &
!                    lmetaex,lambda_in,nlambda,mype)


!    now define all filter constants 

  kh0=1 ; kt0=2 ; kq0=kt0+lmetaex
  kpsi0=kq0+lmetaex ; kchi0=kpsi0+lmetaex
  nvars=4*lmetaex+1
          if(mype.eq.0) print *,' before get3berr for chi'
  call get3berr(echi,lat1,lat2,wgts_bightp,iwgts_bightp,rlenxychi,rlenpchi,isofilter_w(1,2), &
                nhaloc,npass,no_interp,binom,nsmooth,nsmooth_shapiro,ifilt_ord, &
                nxc,nyc,lmetaex,myxsc,myysc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
  deallocate(echi) ; deallocate(rlenxychi) ; deallocate(rlenpchi)
  if(diagnostic) call outcovarchi(isofilter_w(1,2), &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

            if(mype.eq.0) write(0,*)' at 11 in r3dvex, time=',(timef()-time1)*.001

          if(mype.eq.0) print *,' before get3berr for t'
  call get3berr(et,lat1,lat2,wgts_bightp,iwgts_bightp,rlenxyt,rlenpt,isofilter_t, &
                nhaloc,npass,no_interp,binom,nsmooth,nsmooth_shapiro,ifilt_ord, &
                nxc,nyc,lmetaex,myxsc,myysc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
  deallocate(et) ; deallocate(rlenxyt) ; deallocate(rlenpt)
  if(diagnostic) call outcovart(isofilter_t, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info


            if(mype.eq.0) write(0,*)' at 11 in r3dvex, time=',(timef()-time1)*.001
          if(mype.eq.0) print *,' before get3berr for q'
  if(isoq) then
   call get3berr(eq,lat1,lat2,wgts_bightp,iwgts_bightp,rlenxyq,rlenpq,isofilter_q, &
                nhaloc,npass,no_interp,binom,nsmooth,nsmooth_shapiro,ifilt_ord, &
                nxc,nyc,lmetaex,myxsc,myysc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
  else
   allocate(tg(ims:ime,jms:jme,kms:kme))
   allocate(ug(ims:ime,jms:jme,kms:kme))
   allocate(vg(ims:ime,jms:jme,kms:kme))
   allocate(qg(ims:ime,jms:jme,kms:kme))
   allocate(pg(ims:ime,jms:jme))
   allocate(ksfc(ips:ipe,jps:jpe))
   call get_coarse_background(tetaanl,wetaanl(1,1,1,1),wetaanl(1,1,1,2),qetaanl,qsatetaanl,petaanl, &
                 tg,ug,vg,qg,pg,ksfc,lmh,etamex,ptop,nxc,nyc, &
                 bighetah,bighetav,ibighetah,ibighetav,lbig2ges,imeta,jmeta,lmetaex, &
                 iter_restore,iter_smooth, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
   allocate(aspectq(7,ips:ipe,jps:jpe,kps:kpe))
   allocate(ampq(ips:ipe,jps:jpe,kps:kpe))
   call get_aspect(iplot,jplot,kplot,aspectq,ampq,instats_qx,instats_qf,scale_stats,tg,ug,vg,qg,pg,ksfc, &
                  etamex,ptop,dlonc,dlatc,nxc,nyc,lmetaex,corlmin,corlmax, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
   deallocate(tg)
   deallocate(ug)
   deallocate(vg)
   deallocate(pg)
   deallocate(ksfc)
   call get3berrq(qg,iplot,jplot,kplot,aspectq,ampq,isofilter_q, &
                nhaloc,npassq,no_interpq,binom,nsmoothq,nsmoothq_shapiro,ifilt_ordq, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
   deallocate(qg)

   deallocate(aspectq)
   deallocate(ampq)
  end if
  deallocate(eq) ; deallocate(rlenxyq) ; deallocate(rlenpq)
  if(diagnostic) call outcovarq(isofilter_q, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info


            if(mype.eq.0) write(0,*)' at 13 in r3dvex, time=',(timef()-time1)*.001
          if(mype.eq.0) print *,' before get2berrs for p'
  call get2berrs(ep,lat1,lat2,wgts_bightp,iwgts_bightp,rlenxyp,isofilter_p, &
                 nhaloc,npass,no_interp,binom,nsmooth,nsmooth_shapiro,ifilt_ord, &
                 nxc,nyc,myxsc,myysc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
  deallocate(ep)
  if(diagnostic) call outcovarp(isofilter_p, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info


            if(mype.eq.0) write(0,*)' at 14 in r3dvex, time=',(timef()-time1)*.001
          if(mype.eq.0) print *,' before get3berr for psi'
  call get3berr(epsi,lat1,lat2,wgts_bightp,iwgts_bightp,rlenxypsi,rlenppsi,isofilter_w(1,1), &
                nhaloc,npass,no_interp,binom,nsmooth,nsmooth_shapiro,ifilt_ord, &
                nxc,nyc,lmetaex,myxsc,myysc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  deallocate(epsi) ; deallocate(rlenxypsi) ; deallocate(rlenppsi)
  if(diagnostic) call outcovarpsi(isofilter_w(1,1), &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!--------
!--------  now simple iterative solution for analysis increment 
!--------
!        begin=second()

!--------  set up psfw,pwfw,qfw,tfw,wfw
!--------  if .true. then full weight will be given to these data
!--------  conditions test for syndata, dropsondes and 'keeps'

  allocate(psfw(max(1,mpdata)))
  allocate(pwfw(max(1,mpwdata)))
  allocate(qfw(max(1,mqdata)))
  allocate(tfw(max(1,mtdata)))
  allocate(radfw(max(1,mraddata)))
  allocate(wfw(max(1,mwdata)))
  psfw = .false.
  pwfw = .false.
  qfw  = .false.
  tfw  = .false.
  radfw= .false.
  wfw  = .false.
  if(linear_step) then
   psfw = .true.
   pwfw = .true.
   qfw  = .true.
   tfw  = .true.
   radfw= .true.
   wfw  = .true.
  end if

  do i=1,mpdata
    if(psdata(i,5).eq.210. .or. psdata(i,5).eq.132. .or.  &
       psdata(i,5).eq.232. .or. psdata(i,6).eq.0.) psfw(i) = .true.
  enddo
  do i=1,mpwdata
    if(pwdata(i,5).eq.210. .or. pwdata(i,5).eq.132. .or.  &
       pwdata(i,5).eq.232. .or. pwdata(i,6).eq.0.) pwfw(i) = .true.
  enddo
  do i=1,mqdata
    if(qdata(i,5).eq.210. .or. qdata(i,5).eq.132. .or.  &
       qdata(i,5).eq.232. .or. qdata(i,6).eq.0.) qfw(i) = .true.
  enddo
  do i=1,mtdata
    if(tdata(i,5).eq.210. .or. tdata(i,5).eq.132. .or.  &
       tdata(i,5).eq.232. .or. tdata(i,6).eq.0.) tfw(i) = .true.
  enddo
  do i=1,mwdata
    if(wdata(i,5).eq.210. .or. wdata(i,5).eq.132. .or.  &
       wdata(i,5).eq.232. .or. wdata(i,6).eq.0.) wfw(i) = .true.
  enddo

! Preset iq,it,iw,ip,ipw with value of 2 for stations to be printed

  allocate(iq(max(1,mqdata)))
  allocate(it(max(1,mtdata)))
  allocate(iw(max(1,mwdata)))
  allocate(ip(max(1,mpdata)))
  allocate(ipw(max(1,mpwdata)))
  iq=0
  it=0
  iw=0
  ip=0
  ipw=0
!  call setprint(iq,it,iw,ip,ipw,mqdata,mtdata,mwdata,mpdata,mpwdata, &
!                qstaid,tstaid,wstaid,pstaid,pwstaid)

! Solve for penalty function minimum

  allocate(wtq(4,max(1,mqdata)))
  allocate(wtt(4,max(1,mtdata)))
  allocate(wtw(4,max(1,mwdata)))
  allocate(wtp(4,max(1,mpdata)))
  allocate(wtpw(4,max(1,mpwdata)))
  allocate(predinc(jpch,npred))

!   do we want to evaluate background variance at selected lats and pressures?

            if(mype.eq.0) write(0,*)' at 20.3 in r3dvex, time=',(timef()-time1)*.001
  call mdescent(inpes_out,jnpes_out, &
           diagnostic,diagvar,eyot0,eyoq0,eyop0,eyorad0,eyow0,ivart,ivarq,ivarp,ivarrad,ivaru,ivarv, &
                varlats,varpres,nvarlats,nvarpres, &
                verify,maxinner,maxinnerqc,teststop,icondition, &
                iayear,iamonth,iaday,iahour,coldstart,cwm_adjust, &
           tetages,qetages,q2etages,cwmetages,wetages,petages, &
                   q2etaanl,cwmetaanl, &
           qetaanl,qsatetaanl,yoq,xbarbq,mqdata,ibighq,bighq,isofilter_q,iq,wtq,qfw, &
                   yopw,xbarbpw,mpwdata,ibighpw,bighpw,ipw,wtpw,pwfw, &
           wetaanl,yow,xbarbw,mwdata,ibighw,bighw,isofilter_w,iw,wtw,wfw, &
                   yowr,xbarbwr,mwrdata,ibighwr,bighwr, &
           petaanl,yop,xbarbp,mpdata,ibighp,bighp,isofilter_p,ip,wtp,psfw, &
           tetaanl,yot,xbarbt,mtdata,ibight,bight,isofilter_t,it,wtt,tfw, &
        predinc,yorad,xbarbrad,mraddata,ibighradh,bighradh,bighradv,radfw, &
                predrad,icxrad,varpred,npred,userad, &
      lbig2data,lbig2ges,lbig3ges,bigeetah,ibigeetah,bigeetav,ibigeetav, &
        lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
      nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ,time1) ! processor info

! call dealloc_filter(isofilter_t)
! call dealloc_filter(isofilter_w(1,1))        !  for some unknown reason, these calls result 
! call dealloc_filter(isofilter_w(1,2))        !  in a seg-fault at mpi_finalize
! call dealloc_filter(isofilter_q)
! call dealloc_filter(isofilter_p)
  deallocate(pe_of_injn)
  deallocate(in_of_i)
  deallocate(jn_of_j)
  deallocate(varpred)
  deallocate(rxc)
  deallocate(ryc)
  deallocate(yot) ; deallocate(yow) ; deallocate(yowr)
  deallocate(yoq) ; deallocate(yopw)
  deallocate(yop) ; deallocate(yorad)
  deallocate(xbarbt) ; deallocate(xbarbw) ; deallocate(xbarbwr)
  deallocate(xbarbq) ; deallocate(xbarbpw)
  deallocate(xbarbp)
  deallocate(xbarbrad)
  deallocate(bight) ; deallocate(ibight)
  deallocate(bighw) ; deallocate(ibighw)
  deallocate(bighwr) ; deallocate(ibighwr)
  deallocate(bighq) ; deallocate(ibighq)
  deallocate(bighpw) ; deallocate(ibighpw)
  deallocate(bighp) ; deallocate(ibighp)
  deallocate(bighradh) ; deallocate(ibighradh)
  deallocate(icxrad) ; deallocate(predrad)
  deallocate(bighradv)

! Print quality control related information

! call printqc(psdata,pstaid,ip,wtp,mpdata,eyop0,pwdata,pwstaid,ipw,wtpw,mpwdata,eyopw0, &
!       qdata,qstaid,iq,wtq,mqdata,eyoq0,tdata,tstaid,it,wtt,mtdata,eyot0, &
!       wdata,wstaid,iw,wtw,mwdata,eyow0,iuniterr)
            if(mype.eq.0) write(0,*)' at 21 in r3dvex, time=',(timef()-time1)*.001

  deallocate(eyorad0)
  deallocate(eyop0)
  deallocate(eyot0)
  deallocate(eyopw0)
  deallocate(eyow0)
  deallocate(eyowr0)
  deallocate(eyoq0)
  deallocate(psdata) ; deallocate(pwdata)  ; deallocate(qdata) ; deallocate(tdata)  ; deallocate(wdata)
  deallocate(pstaid) ; deallocate(pwstaid) ; deallocate(qstaid); deallocate(tstaid) ; deallocate(wstaid)
  deallocate(iq)     ; deallocate(it)      ; deallocate(iw)    ; deallocate(ip)     ; deallocate(ipw)
  deallocate(wtq)    ; deallocate(wtt)     ; deallocate(wtw)   ; deallocate(wtp)    ; deallocate(wtpw)
  deallocate(psfw)
  deallocate(pwfw)
  deallocate(qfw)
  deallocate(tfw)
  deallocate(wfw)


  
  deallocate(rxcglb,stat=ier)
  deallocate(rycglb,stat=ier)


!     update bias corrector coefficients and write out
  mype_rad=min(npes-1,12)
! mype_rad=npes+10
  if(userad.and.mype.eq.mype_rad) &
       call write_satbias(allbias_new,predinc,jpch,npred)

  deallocate(predinc,stat=ier)
  deallocate(bigeetah)
  deallocate(bigeetav)
  deallocate(ibigeetah)
  deallocate(ibigeetav)

       return
       end
