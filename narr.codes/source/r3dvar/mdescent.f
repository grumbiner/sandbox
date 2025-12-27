subroutine mdescent(inpes_out,jnpes_out,diagnostic,diagvar,eyot0,eyoq0,eyop0, &
                      eyorad0,eyow0,ivart,ivarq,ivarp,ivarrad,ivaru,ivarv, &
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
        xpred,yorad,xbarbrad,mraddata,ibighradh,bighradh,bighradv,radfw, &
                predrad,icxrad,varpred,npred,userad, &
       lbig2data,lbig2ges,lbig3ges,bigeetah,ibigeetah,bigeetav,ibigeetav, &
       lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
   nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ,time1) ! processor info

!-------- this is a model space conjugate gradient descent algorithm, which will eventually be
!-------- used to include some non-linear forward operators, using the global 3dvar procedure.

  include 'mpif.h'
      include "my_comm.h"
  include 'filtertype.h'
  include 'qcparam.h'

  logical diagvar,verify,diagnostic
  logical doqc
  INTEGER(4), INTENT(IN) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)
  logical coldstart,cwm_adjust

  integer(4) ivart(nvarlats,nvarpres)
  integer(4) ivarq(nvarlats,nvarpres)
  integer(4) ivarp(nvarlats)
  integer(4) ivarrad(nvarlats,jpch)
  integer(4) ivaru(nvarlats,nvarpres)
  integer(4) ivarv(nvarlats,nvarpres)
  real(4) varlats(nvarlats),varpres(nvarpres)
  real(4) tetages(imeta,jmeta,lmeta)
  real(4) qetages(imeta,jmeta,lmeta)
  real(4) q2etages(imeta,jmeta,lmeta)
  real(4) cwmetages(imeta,jmeta,lmeta)
  real(4) wetages(imeta,jmeta,lmeta,2)
  real(4) petages(imeta,jmeta)

!--------  these arrays contain the eta guess on extended vertical coordinate on input
!--------  and are continually updated during the analysis.
!--------  at the end, they contain the analysis on the extended eta grid, and it is a 
!--------  simple matter to update the eta common variables

  real(4) q2etaanl(imeta,jmeta,lmetaex)
  real(4) cwmetaanl(imeta,jmeta,lmetaex)

!    moisture stuff:

  real(4) qetaanl(imeta,jmeta,lmetaex)        ! input: eta guess,  output: eta analysis
  real(4) qsatetaanl(imeta,jmeta,lmetaex)     ! input: eta guess sat specific hum
  real(4) yoq(max(1,mqdata))                  ! observations
  real(4) eyoq0(max(1,mqdata))                ! observation errors
  real(4) xbarbq(max(1,mqdata))               ! background (obs units)
  integer(4) ibighq(lbig3ges,max(1,mqdata))   ! H operator
  real(4) bighq(lbig3ges,max(1,mqdata))       !   (eta to obs) 
  type(filter_cons) isofilter_q(14)           ! coarse background error Ctilde
  integer(4) iq(max(1,mqdata))                ! various stuff
  real(4)    wtq(4,max(1,mqdata))             !  for keeping track of
  logical qfw(max(1,mqdata))                  !   quality control

  real(4),allocatable::xq(:,:,:)                  !  analysis increment
  real(4),allocatable::xbarq(:)                   !  analysis increment (obs units)
  real(4),allocatable::pq(:,:,:)                  !  descent direction
  real(4),allocatable::pbarq(:)                   !  descent direction (obs units)
  real(4),allocatable::rq(:,:,:)                  !  gradient of objective function
  real(4),allocatable::qetadir(:,:,:)             !  intermediate descent direction

!    pw stuff:

  real(4) yopw(max(1,mpwdata))                     ! observations
  real(4) xbarbpw(max(1,mpwdata))                  ! background (obs units)
  integer(4) ibighpw(lbig2ges,max(1,mpwdata))      ! H operator
  real(4) bighpw(lmetaex,lbig2ges,max(1,mpwdata))  !   (eta to obs) 
  integer(4) ipw(max(1,mpwdata))                   ! various stuff
  real(4)    wtpw(4,max(1,mpwdata))                !  for keeping track of
  logical pwfw(max(1,mpwdata))                     !   quality control

  real(4),allocatable::xbarpw(:)                   !  analysis increment (obs units)
  real(4),allocatable::pbarpw(:)                   !  descent direction (obs units)
  
!    wind stuff:

  real(4) wetaanl(imeta,jmeta,lmetaex,2)      ! input: eta guess     (1=u,2=v)
  real(4) yow(max(1,mwdata))                  ! observations
  real(4) eyow0(max(1,mwdata))                ! observation errors
  real(4) xbarbw(max(1,mwdata))               ! background (obs units)
  integer(4) ibighw(lbig3ges,max(1,mwdata))   ! H operator
  real(4) bighw(lbig3ges,2,max(1,mwdata))     !   (eta to obs) 
  type(filter_cons) isofilter_w(14,2)         ! coarse background error Ctilde  (1--psi, 2--chi)
  integer(4) iw(max(1,mwdata))                ! various stuff
  real(4)    wtw(4,max(1,mwdata))             !  for keeping track of
  logical wfw(max(1,mwdata))                  !   quality control

  real(4),allocatable::xw(:,:,:,:)                !  analysis increment
  real(4),allocatable::xbarw(:)                   !  analysis increment (obs units)
  real(4),allocatable::pw(:,:,:,:)                !  descent direction
  real(4),allocatable::pbarw(:)                   !  descent direction (obs units)
  real(4),allocatable::rw(:,:,:,:)                !  gradient of objective function
  real(4),allocatable::wetadir(:,:,:,:)           !  intermediate descent direction
  
!    radar wind stuff:

  real(4) yowr(max(1,mwrdata))                  ! observations
  real(4) eyowr0(max(1,mwrdata))                ! observation errors
  real(4) xbarbwr(lmetaex,max(1,mwrdata))       ! background (obs units)
  integer(4) ibighwr(lbig2ges+2,max(1,mwrdata)) ! H operator
  real(4) bighwr(lbig2ges,2,max(1,mwrdata))     !   (eta to obs) 

  real(4),allocatable::xbarwr(:,:)                  !  analysis increment (obs units)
  real(4),allocatable::pbarwr(:,:)                  !  descent direction (obs units)
  
!    h stuff:

  real(4) petaanl(imeta,jmeta)                ! input: eta guess
  real(4) yop(max(1,mpdata))                  ! observations
  real(4) eyop0(max(1,mpdata))                ! observation errors
  real(4) xbarbp(max(1,mpdata))               ! background temperature (1-lmetaex) and pdr (lmetaex+1)
  integer(4) ibighp(lbig2ges,max(1,mpdata))   ! Hh operator
  real(4) bighp(lbig2ges,max(1,mpdata))       !   (eta to obs, horizontal part) 
  type(filter_cons) isofilter_p(14)           ! coarse background error Ctilde
  integer(4) ip(max(1,mpdata))                ! various stuff
  real(4)    wtp(4,max(1,mpdata))             !  for keeping track of
  logical psfw(max(1,mpdata))                 !   quality control

  real(4),allocatable::xp(:,:)                    !  analysis increment
  real(4),allocatable::xbarp(:)                   !  analysis increment (obs units)
  real(4),allocatable::pp(:,:)                    !  descent direction
  real(4),allocatable::pbarp(:)                   !  descent direction (obs units)
  real(4),allocatable::rp(:,:)                    !  gradient of objective function
  real(4),allocatable::petadir(:,:)               !  intermediate descent direction

!    t stuff:

  real(4) tetaanl(imeta,jmeta,lmetaex)        ! input: eta guess,  output: eta analysis
  real(4) yot(max(1,mtdata))                  ! observations
  real(4) eyot0(max(1,mtdata))                ! observation errors
  real(4) xbarbt(max(1,mtdata))               ! background (obs units)
  integer(4) ibight(lbig3ges,max(1,mtdata))   ! H operator
  real(4) bight(lbig3ges,max(1,mtdata))       !   (eta to obs) 
  type(filter_cons) isofilter_t(14)           ! coarse background error Ctilde
  integer(4) it(max(1,mtdata))                ! various stuff
  real(4)    wtt(4,max(1,mtdata))             !  for keeping track of
  logical tfw(max(1,mtdata))                  !   quality control

  real(4),allocatable::xt(:,:,:)                  !  analysis increment
  real(4),allocatable::xbart(:)                   !  analysis increment (obs units)
  real(4),allocatable::pt(:,:,:)                  !  descent direction
  real(4),allocatable::pbart(:)                   !  descent direction (obs units)
  real(4),allocatable::rt(:,:,:)                  !  gradient of objective function
  real(4),allocatable::tetadir(:,:,:)             !  intermediate descent direction

!    rad stuff:

  real(4) xpred(jpch,npred)
  real(4) yorad(max(1,mraddata))
  real(4) eyorad0(max(1,mraddata))                ! observation errors
  real(4) xbarbrad(max(1,mraddata))
  integer(4) ibighradh(lbig2ges,max(1,mraddata))
  real(4) bighradh(lbig2ges,max(1,mraddata))
  real(4) bighradv(2*lmetaex,max(1,mraddata))
  real(4) predrad(npred+jpchus-1,max(1,mraddata))
  integer(4) icxrad(2,max(1,mraddata))
  real(4) varpred(jpch,npred)
  logical userad
  logical radfw(max(1,mraddata))                    !   quality control

  real(4),allocatable::xbarrad(:)
  real(4),allocatable::ppred(:,:)                 ! descent direction
  real(4),allocatable::pbarrad(:)                 ! descent direction (obs units)
  real(4),allocatable::rpred(:,:)                 ! gradient of objective function
  
!-------- Eeta:  interpolation constants for coarse grid to eta grid

  real(4) bigeetah(imeta,jmeta,lbig2data)
  integer(4) ibigeetah(imeta,jmeta,lbig2data)
  real(4) bigeetav(imeta,jmeta,lbig2data,2)
  integer(4) ibigeetav(imeta,jmeta,lbig2data)

!--------  Ht_psi, Hp_psi: psi is balanced variable. These define balanced part of T, pdr

  real(4) bight_psi(lmetaex,lmetaex,lat1:lat2),bighp_psi(lmetaex,lat1:lat2)
  real(4) wgts_bightp(2,nxc*nyc)
  integer(4) iwgts_bightp(2,nxc*nyc)

  real(8) alphat(maxinner+1)
  real(8) betat(maxinner+1)
  real(8) rnormold8t,rnorm8t,alpha8t,beta8t
  real(8) condnum
  real(8) prodtt_8
  real(4) bigj(maxinner)

  data bmiss /10.e10/

        time0=timef()
             print *,' at 0.0 in mdescent, mype,mpdata=',mype,mpdata
  call mpi_comm_rank(my_comm,mype,ierror)
             print *,' at 0.1 in mdescent, mype,mpdata=',mype,mpdata
      
!-------------allocate moisture stuff:

           if(mype.eq.0) write(0,*)' at 1 in mdescent, time=',(timef()-time1)*.001
  allocate(xq(nxc,nyc,lmetaex))
  allocate(xbarq(max(1,mqdata)))
  allocate(pq(nxc,nyc,lmetaex))
  allocate(pbarq(max(1,mqdata)))
  allocate(rq(nxc,nyc,lmetaex))
  allocate(qetadir(imeta,jmeta,lmetaex))
      
!-------------allocate pw stuff:

  allocate(xbarpw(max(1,mpwdata)))
  allocate(pbarpw(max(1,mpwdata)))
      
!-------------allocate wind stuff:

  allocate(xw(nxc,nyc,lmetaex,2))
  allocate(xbarw(max(1,mwdata)))
  allocate(pw(nxc,nyc,lmetaex,2))
  allocate(pbarw(max(1,mwdata)))
  allocate(rw(nxc,nyc,lmetaex,2))
  allocate(wetadir(imeta,jmeta,lmetaex,2))
      
!-------------allocate radar wind stuff:

  allocate(xbarwr(lmetaex,max(1,mwrdata)))
  allocate(pbarwr(lmetaex,max(1,mwrdata)))
      
!-------------allocate h stuff:

  allocate(xp(nxc,nyc))
  allocate(xbarp(max(1,mpdata)))
  allocate(pp(nxc,nyc))
  allocate(pbarp(max(1,mpdata)))
  allocate(rp(nxc,nyc))
  allocate(petadir(imeta,jmeta))
      
!-------------allocate temp stuff:

  allocate(xt(nxc,nyc,lmetaex))
  allocate(xbart(max(1,mtdata)))
  allocate(pt(nxc,nyc,lmetaex))
  allocate(pbart(max(1,mtdata)))
  allocate(rt(nxc,nyc,lmetaex))
  allocate(tetadir(imeta,jmeta,lmetaex))

!-------------allocate rad stuff:

  allocate(xbarrad(max(1,mraddata)))
  allocate(ppred(jpch,npred))
  allocate(pbarrad(max(1,mraddata)))
  allocate(rpred(jpch,npred))

           if(mype.eq.0) write(0,*)' at 4 in mdescent, time=',(timef()-time1)*.001
!  set initial analysis increment to zero

  xq=0. ; xbarq=0.
          xbarpw=0.
  xw=0. ; xbarw=0.
          xbarwr=0.
  xp=0. ; xbarp=0.
  xt=0. ; xbart=0.
  xpred=0. ;       xbarrad=0.

!  set initial discent direction to zero

  pq=0.
  pw=0.
  pp=0.
  pt=0.
  ppred=0.
  beta4t=0.

!       do we want to look at selected variances?

! if(diagvar) then
!  deallocate(xq)
!  deallocate(pq)
!  deallocate(pbarq)
!  deallocate(qetadir)
!  deallocate(pbarpw)
!  deallocate(xw)
!  deallocate(pw)
!  deallocate(pbarw)
!  deallocate(wetadir)
!  deallocate(xp)
!  deallocate(pp)
!  deallocate(pbarp)
!  deallocate(petadir)
!  deallocate(xt)
!  deallocate(pt)
!  deallocate(pbart)
!  deallocate(tetadir)
!  deallocate(ppred)
!  deallocate(pbarrad)
!  call diagnostic_bvar(eyot0,eyoq0,eyop0,eyorad0,eyow0,ivart,ivarq,ivarp,ivarrad,ivaru,ivarv, &
!             varlats,varpres,nvarlats,nvarpres, &
!             xbarq,rq,mqdata,ibighq,bighq,isofilter_q, &
!             xbarpw,mpwdata,ibighpw,bighpw, &
!             xbarw,rw,mwdata,ibighw,bighw,isofilter_w(1,1),isofilter_w(1,2), &
!             xbarp,rp,mpdata,ibighp,bighp,isofilter_p, &
!             xbart,xbarbt,yot,rt,mtdata,ibight,bight,isofilter_t, &
!             xbarrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
!               predrad,icxrad,varpred,npred,userad, &
!        lbig2data,lbig2ges,lbig3ges,bigeetah,ibigeetah,bigeetav,ibigeetav, &
!        lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
!      nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
!               ids, ide, jds, jde, kds, kde, &                          ! domain indices
!               ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
!               ims, ime, jms, jme, kms, kme, &                          ! memory indices
!               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
! end if

!  set last gradient norm to zero

  rnormold8t=0._8

!  start iterative descent

  iterused=0
  doqc=.false.
  pseudogradmax=0.

  do iter=1,maxinner

   if(iter.gt.maxinnerqc) then
    doqc=.true.
    beta4t=0.            !  when turning on qc, restart with steepest descent
    rnormold8t=0._8
   end if

!       do diagnostic computation of model div, p tendencies

!  if(iter.eq.1.or.iter.eq.maxinner) then
!   call get_dt_gt(tetaanl,qetaanl,q2etaanl,cwmetaanl, &
!                 wetaanl(1,1,1,1),wetaanl(1,1,1,2),petaanl, &
!           imeta,jmeta,lmeta,nxc,nyc,myis2,myie2,myjs2,myje2)
               if(verify) then
                 call mpi_finalize(ierr)
                 stop
               end if
!  end if
   if(iter.eq.maxinner) exit

!   obtain gradient of objective function

           if(mype.eq.0) write(0,*)' at 5 in mdescent, time=',(timef()-time1)*.001
   call gradj( &
              xq,xbarq,rq,yoq,xbarbq,mqdata,ibighq,bighq,isofilter_q,qfw, &
                  xbarpw,yopw,xbarbpw,mpwdata,ibighpw,bighpw,pwfw, &
              xw,xbarw,rw,yow,xbarbw,mwdata,ibighw,bighw,isofilter_w,wfw, &
                  xbarwr,yowr,xbarbwr,mwrdata,ibighwr,bighwr, &
              xp,xbarp,rp,yop,xbarbp,mpdata,ibighp,bighp,isofilter_p,psfw, &
              xt,xbart,rt,yot,xbarbt,mtdata,ibight,bight,isofilter_t,tfw, &
        xpred,xbarrad,rpred,yorad,xbarbrad,mraddata,ibighradh,bighradh,bighradv,radfw, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
       nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,doqc,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!   compute norm of gradient  (real8 arithmetic may be important for these long sums)

   rnorm8t=0._8
   if(userad) then
    do n=1,npred
     do j=1,jpch
      rnorm8t=rnorm8t+1._8*rpred(j,n)**2
     end do
    end do
   end if

           if(mype.eq.0) write(0,*)' at 6 in mdescent, time=',(timef()-time1)*.001
   call dot_prod8g(prodtt_8,rq,rq,nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec)
        if(mype.eq.0) print *,' rq*rq part of gradient = ',prodtt_8
   rnorm8t=rnorm8t+prodtt_8

   call dot_prod8g(prodtt_8,rw,rw,nxc,nyc,lmetaex*2,myxsc,myxec,myysc,myyec)
        if(mype.eq.0) print *,' rw*rw part of gradient = ',prodtt_8
   rnorm8t=rnorm8t+prodtt_8

   call dot_prod8g(prodtt_8,rt,rt,nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec)
        if(mype.eq.0) print *,' rt*rt part of gradient = ',prodtt_8
   rnorm8t=rnorm8t+prodtt_8

   call dot_prod8g(prodtt_8,rp,rp,nxc,nyc,1,myxsc,myxec,myysc,myyec)
        if(mype.eq.0) print *,' rp*rp part of gradient = ',prodtt_8
   rnorm8t=rnorm8t+prodtt_8

   rnormt=rnorm8t

!    set refnorm as norm of first gradient computed (and then new norm when qc turned on)

   if(iter.eq.1) then
    refnormt=rnormt
   end if

   if(mype.eq.0) print *,' iteration, doqc, gradient = ',iter,doqc,rnormt
   if(mype.eq.0) write(0,*)' iteration, doqc, gradient = ',iter,doqc,rnormt
           if(mype.eq.0) write(0,*)' at 6.5 in mdescent, time=',(timef()-time1)*.001

!  compute mixing parameter for next conjugate gradient descent direction

   if(rnormold8t.ne.0._8) then
    beta8t=rnorm8t/rnormold8t
   else
    beta8t = 0._8
   endif
   beta4t=beta8t

!  get new descent direction

   if(userad) ppred=-rpred+beta4t*ppred
   pq=-rq+beta4t*pq
   pw=-rw+beta4t*pw
   pp=-rp+beta4t*pp
   pt=-rt+beta4t*pt

!    save this gradient norm for use estimating descent direction in next iteration

   rnormold8t=rnorm8t

!  get parallel descent direction variables on eta grid and at obs locations

   if(userad) rpred=varpred*ppred
   rq=pq
   call regular_ad_raf(rq,isofilter_q)
   rw=pw
   call regular_ad_raf(rw(1,1,1,1),isofilter_w(1,1))
   call regular_ad_raf(rw(1,1,1,2),isofilter_w(1,2))

   rt=pt
   call regular_ad_raf(rt,isofilter_t)

   rp=pp
   call regular_ad_raf(rp,isofilter_p)

   call slow_plus_fast(rt,rp,rw(1,1,1,1), &
                       lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp,nxc,nyc,lmetaex)
                           
   call bigeetahopq(rq,qetadir,imeta,jmeta,lmetaex,lbig2data, &
                   nxc,nyc,myis2,myie2,myjs2,myje2,qsatetaanl,bigeetah,ibigeetah)
   call bigeetavop(rw,wetadir,imeta,jmeta,lmetaex,lbig2data, &
                   nxc,nyc,myis2,myie2,myjs2,myje2,bigeetav,ibigeetav)
   call bigeetahop(rt,tetadir,imeta,jmeta,lmetaex,lbig2data, &
                   nxc,nyc,myis2,myie2,myjs2,myje2,bigeetah,ibigeetah)
   call bigeetahop(rp,petadir,imeta,jmeta,1,lbig2data, &
                   nxc,nyc,myis2,myie2,myjs2,myje2,bigeetah,ibigeetah)

   call bighop(qetadir,pbarq,mqdata,ibighq,bighq, &
                       pbarpw,mpwdata,ibighpw,bighpw, &
               wetadir,pbarw,mwdata,ibighw,bighw, &
                       pbarwr,mwrdata,ibighwr,bighwr, &
               petadir,pbarp,mpdata,ibighp,bighp, &
               tetadir,pbart,mtdata,ibight,bight, &
             rpred,pbarrad,mraddata,ibighradh,bighradh,bighradv,icxrad,predrad,npred,userad, &
               lbig2ges,lbig3ges,imeta,jmeta,lmetaex,jpch,jpchus)
        
!  now get step-size, using line-search algorithm

   call stepsize(bigj(iter),alpha8t, &
                 xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                   xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
                 xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                    xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
                 xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
                 xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
         xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
                       nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
         if(mype.eq.0) print *,' iter,bigj(iter)=',iter,bigj(iter)

!      test to see if we should stop based on actual values of objective function

   if(iter.gt.2) then
    pseudograd=bigj(iter-2)-bigj(iter-1)
    pseudogradmax=max(pseudogradmax,pseudograd)
    if(mype.eq.0) print *,' iteration, pseudograd = ',iter,pseudograd
    if(mype.eq.0) write(0,*)' iteration, pseudograd = ',iter,pseudograd
           if(mype.eq.0) write(0,*)' at 6.6 in mdescent, time=',(timef()-time1)*.001
    if(pseudograd.le.0.) exit
   end if
     

!  save descent parameters for use in Jim Purser's condition number estimator

   alpha4t=alpha8t
   iterused=iterused+1
   alphat(iterused)=alpha8t
   betat(max(1,iterused-1))=beta8t

!  update analysis increment on grid

   xq=xq+alpha4t*pq
   xw=xw+alpha4t*pw
   xt=xt+alpha4t*pt
   xp=xp+alpha4t*pp
   if(userad) xpred=xpred+alpha4t*ppred

!  update obs analysis increment

   xbarq=xbarq+alpha4t*pbarq
   xbarpw=xbarpw+alpha4t*pbarpw
   xbarw=xbarw+alpha4t*pbarw
   xbarwr=xbarwr+alpha4t*pbarwr
   xbarp=xbarp+alpha4t*pbarp
   xbart=xbart+alpha4t*pbart
   if(userad) xbarrad=xbarrad+alpha4t*pbarrad

!  update eta grid analysis

   qetaanl=qetaanl+alpha4t*qetadir
   wetaanl=wetaanl+alpha4t*wetadir
   tetaanl=tetaanl+alpha4t*tetadir
   petaanl=petaanl+alpha4t*petadir

!   end of conjugate gradient iteration

  end do
  if(diagnostic) then
   rq=xq
   call regular_ad_raf(rq,isofilter_q)
   rw=xw
   call regular_ad_raf(rw(1,1,1,1),isofilter_w(1,1))
   call regular_ad_raf(rw(1,1,1,2),isofilter_w(1,2))

   rt=xt
   call regular_ad_raf(rt,isofilter_t)

   rp=xp
   call regular_ad_raf(rp,isofilter_p)

   call slow_plus_fast(rt,rp,rw(1,1,1,1), &
                       lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp,nxc,nyc,lmetaex)
        call out_psi_inc(rw(1,1,1,1), &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
        call out_chi_inc(rw(1,1,1,2), &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
        call out_t_inc(rt, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
        call out_q_inc(rq, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
        call out_p_inc(rp, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
  end if

!     need to unscale xpred, which is only control variable left which is needed (to update sat bias coefs)

  if(userad) xpred=varpred*xpred

!   restore guess to eta common

  call restore_eta_ges(tetages,qetages,q2etages,cwmetages,wetages(1,1,1,1),wetages(1,1,1,2),petages, &
                 imeta,jmeta,lmeta)

          if(mype.eq.0) write(0,*)' time to get analysis increment = ',(timef()-time0)*.001
  call etaupdate(inpes_out,jnpes_out, &
             tetaanl,qetaanl,q2etaanl,cwmetaanl,wetaanl(1,1,1,1),wetaanl(1,1,1,2),petaanl, &
             imeta,jmeta,lmeta,iayear,iamonth,iaday,iahour,coldstart,cwm_adjust)
            if(mype.eq.0) write(0,*)' at 7 in mdescent'

!  collect info, preparatory for printing out
!  info on obs which have small analysis weight

  C = 1.2533/b

  wtlim = 0.1
  pgnot = 1.-pgq
  pgc = pgq*C
  print *,' mdescent--pgq,b,C,pgnot,pgc: ',pgq,b,C,pgnot,pgc
! iq = 0
  wtq = bmiss
 if(mqdata.gt.0) then
  do i=1,mqdata
    arg = exp(-.5*(yoq(i)-xbarbq(i)-xbarq(i))**2)
    wtq(1,i) = pgnot*arg/(pgnot*arg+pgc)
    wtq(2,i) = yoq(i)
    wtq(3,i) = xbarbq(i)
    wtq(4,i) = xbarq(i)
    if(abs(wtq(1,i)).lt.wtlim) iq(i) = 1
  enddo
 end if
  call lastevents_q(wtq,mqdata,lbig3ges,mype)
            if(mype.eq.0) write(0,*)' at 8 in mdescent'

  wtlim = 0.1
  pgnot = 1.-pgpw
  pgc = pgpw*C
! ipw = 0
  wtpw = bmiss
 if(mpwdata.gt.0) then
  do i=1,mpwdata
    arg = exp(-.5*(yopw(i)-xbarbpw(i)-xbarpw(i))**2)
    wtpw(1,i) = pgnot*arg/(pgnot*arg+pgc)
    wtpw(2,i) = yopw(i)
    wtpw(3,i) = xbarbpw(i)
    wtpw(4,i) = xbarpw(i)
    if(abs(wtpw(1,i)).lt.wtlim) ipw(i) = 1
  enddo
 end if
  call lastevents_pw(wtpw,mpwdata,lbig2ges,mype,lmetaex)
            if(mype.eq.0) write(0,*)' at 9 in mdescent'

  wtlim = 0.1
  pgnot = 1.-pgw
  pgc = pgw*C
! iw = 0
  wtw = bmiss
 if(mwdata.gt.0) then
  do i=1,mwdata
    arg = exp(-.5*(yow(i)-xbarbw(i)-xbarw(i))**2)
    wtw(1,i) = pgnot*arg/(pgnot*arg+pgc)
    wtw(2,i) = yow(i)
    wtw(3,i) = xbarbw(i)
    wtw(4,i) = xbarw(i)
    if(abs(wtw(1,i)).lt.wtlim) iw(i) = 1
  enddo
 end if
  call lastevents_w(wtw,mwdata,lbig3ges,mype)
            if(mype.eq.0) write(0,*)' at 10 in mdescent'

  wtlim = 0.1
  pgnot = 1.-pgp
  pgc = pgp*C
! ip = 0
  wtp = bmiss
 if(mpdata.gt.0) then
  do i=1,mpdata
    arg = exp(-.5*(yop(i)-xbarbp(i)-xbarp(i))**2)
    wtp(1,i) = pgnot*arg/(pgnot*arg+pgc)
    wtp(2,i) = yop(i)
    wtp(3,i) = xbarbp(i)
    wtp(4,i) = xbarp(i)
    if(abs(wtp(1,i)).lt.wtlim) ip(i) = 1
  enddo
 end if
             print *,' at 10.1 in mdescent, mype,mpdata=',mype,mpdata
  call lastevents_p(wtp,mpdata,lbig2ges,mype)
            if(mype.eq.0) write(0,*)' at 11 in mdescent'

  wtlim = 0.1
  pgnot = 1.-pgt
  pgc = pgt*C
! it = 0
  wtt = bmiss
 if(mtdata.gt.0) then
  do i=1,mtdata
    arg = exp(-.5*(yot(i)-xbarbt(i)-xbart(i))**2)
    wtt(1,i) = pgnot*arg/(pgnot*arg+pgc)
    wtt(2,i) = yot(i)
    wtt(3,i) = xbarbt(i)
    wtt(4,i) = xbart(i)
    if(abs(wtt(1,i)).lt.wtlim) it(i) = 1
  enddo
 end if
  call lastevents_t(wtt,mtdata,lbig3ges,mype)
            if(mype.eq.0) write(0,*)' at 12 in mdescent'

  deallocate(pbarq)
  deallocate(xbarq)
  deallocate(pbarpw)
  deallocate(xbarpw)
  deallocate(pbarw)
  deallocate(xbarw)
  deallocate(pbarwr)
  deallocate(xbarwr)
  deallocate(pbarp)
  deallocate(xbarp)
  deallocate(pbart)
  deallocate(xbart)

!----- compute condition numbers

            if(mype.eq.0) write(0,*)' at 13 in mdescent'
! mdata_myall=mwdata+mwrdata+mqdata+mpwdata+mpdata+mtdata+mraddata
  mdata_myall=mwdata+mqdata+mpwdata+mpdata+mtdata  !??????????????don*t leave this in
  call mpi_allreduce(mdata_myall,mdata_all,1,mpi_integer,mpi_sum,my_comm,ierror)
        if(mype.eq.0) print *,'mdata_all=',mdata_all
            if(mype.eq.0) write(0,*)' at 14 in mdescent'
  if(mype.eq.0) then
   do i=1,iterused-1
    if(mdata_all.gt.0) print *,' alphat,betat(',i,')=',alphat(i),betat(i)
   end do
  end if
            if(mype.eq.0) write(0,*)' at 15 in mdescent'

  if(iterused.ge.2) then
    iterusedm=iterused-1
    iterusedm=min(icondition,iterusedm)
    do loop=1,iterusedm
      condnum=-1._8
      if(mdata_all.gt.0) call cg_cond_num(loop,condnum,alphat,betat)
      condnum=condnum+1._8
      numit=loop+1
      if(mype.eq.0) print *,' for ',numit,' iterations, t condition number',' estimate = ',condnum
    end do
  end if
            if(mype.eq.0) write(0,*)' at 16 in mdescent'

  deallocate(pq) ; deallocate(rq) ; deallocate(xq) ; deallocate(qetadir)
            if(mype.eq.0) write(0,*)' at 17 in mdescent'
  deallocate(pw) ; deallocate(rw) ; deallocate(xw) ; deallocate(wetadir)
            if(mype.eq.0) write(0,*)' at 18 in mdescent'
  deallocate(pp) ; deallocate(rp) ; deallocate(xp) ; deallocate(petadir)
            if(mype.eq.0) write(0,*)' at 19 in mdescent'
  deallocate(pt) ; deallocate(rt) ; deallocate(xt) ; deallocate(tetadir)
            if(mype.eq.0) write(0,*)' at 20 in mdescent'

return
end subroutine mdescent
