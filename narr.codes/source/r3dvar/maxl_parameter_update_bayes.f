subroutine maxl_parameter_update_bayes(blambda_in,blambda_out,lambda_in,lambda_out,dlambda,nlambda, &
               rlenxyp0,rlenxyt0,rlenxyq0,rlenxypsi0,rlenxychi0, &
               rlenpt0,rlenpq0,rlenppsi0,rlenpchi0, &
               ep0,et0,eq0,epsi0,echi0, &
        tetaanl,qetaanl,q2etaanl,cwmetaanl,wetaanl,petaanl, &
        tetages,qetages,q2etages,cwmetages,wetages,petages, &
        iayear,iamonth,iaday,iahour,coldstart,cwm_adjust, &
        maxinner,maxouter,teststop,npass,nsmooth, &
        yoq,xbarbq,mqdata,mqdataall,ibighq,bighq, &
        yopw,xbarbpw,mpwdata,mpwdataall,ibighpw,bighpw, &
        yow,xbarbw,mwdata,mwdataall,ibighw,bighw, &
        yop,xbarbp,mpdata,mpdataall,ibighp,bighp, &
        yot,xbarbt,mtdata,mtdataall,ibight,bight, &
        yorad,xbarbrad,mraddata,mraddataall,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
        lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
        lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
        nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec, &
        myxsc_glb,myysc_glb,nxcglb,nycglb,erlat0,rxc,ryc,jpch,jpchus, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!   update background error parameters using stocastic version of bayesian maximum-likelihood method.

  include 'mpif.h'
      include "my_comm.h"
  include 'filtertype.h'

  real(4) tetaanl(imeta,jmeta,lmetaex)
  real(4) qetaanl(imeta,jmeta,lmetaex)
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
  logical coldstart,cwm_adjust

  real(8) obs_space_prod_8,prod_8
  real(4) blambda_in(nlambda,nlambda)
  real(4) blambda_out(nlambda,nlambda)
  real(4) lambda_in(nlambda)
  real(4) lambda_out(nlambda)
  real(4) dlambda(nlambda)
  real(4) lalpha(nlambda),lalphahat(nlambda)
  real(4) lalpha_mask(nlambda)
  real(4) lalphabeta_sd
  real(4) rxc(nxc),ryc(nyc)

  INTEGER(4), INTENT(IN) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  real(4) rlenxyp0                     !  1
  real(4) rlenxyt0  (lmetaex)          !  2
  real(4) rlenxyq0  (lmetaex)          !  3
  real(4) rlenxypsi0(lmetaex)          !  4
  real(4) rlenxychi0(lmetaex)          !  5
  real(4) rlenpt0   (lmetaex)          !  6
  real(4) rlenpq0   (lmetaex)          !  7
  real(4) rlenppsi0 (lmetaex)          !  8
  real(4) rlenpchi0 (lmetaex)          !  9
  real(4) ep0       (        lat1:lat2)!  10
  real(4) et0       (lmetaex,lat1:lat2)!  11
  real(4) eq0       (lmetaex,lat1:lat2)!  12
  real(4) epsi0     (lmetaex,lat1:lat2)!  13
  real(4) echi0     (lmetaex,lat1:lat2)!  14

  real(4) rlenxyp          (2)          !  1
  real(4) rlenxyt  (lmetaex,2)          !  2
  real(4) rlenxyq  (lmetaex,2)          !  3
  real(4) rlenxypsi(lmetaex,2)          !  4
  real(4) rlenxychi(lmetaex,2)          !  5
  real(4) rlenpt   (lmetaex,2)          !  6
  real(4) rlenpq   (lmetaex,2)          !  7
  real(4) rlenppsi (lmetaex,2)          !  8
  real(4) rlenpchi (lmetaex,2)          !  9
  real(4) ep               (lat1:lat2,2)!  10
  real(4) et       (lmetaex,lat1:lat2,2)!  11
  real(4) eq       (lmetaex,lat1:lat2,2)!  12
  real(4) epsi     (lmetaex,lat1:lat2,2)!  13
  real(4) echi     (lmetaex,lat1:lat2,2)!  14


  real(4) dda(2,nlambda)

!    moisture stuff:

  real(4) yoq(max(1,mqdata))                               ! observations
  real(4) xbarbq(max(1,mqdata))                            ! guess (obs units)
  integer(4) ibighq(lbig3ges,max(1,mqdata))                ! H operator
  real(4) bighq(lbig3ges,max(1,mqdata))                    !   (eta to obs)
  type(filter_cons) isofilter_q  (14,2,2,2)                 !  e,rlxy,rlp
  integer(4) iddaq(3,2,nlambda)

  real(4),allocatable::epsq(:),rq(:)
  real(4),allocatable::epsbarq(:),dbarq(:),f0barq(:),qbarq(:),rbarq(:)
  real(4),allocatable::palphabarq(:,:)
  real(4),allocatable::galphabarq(:,:)
  real(4),allocatable::gsdbarq(:)
  real(4),allocatable::hsdbarq(:)

!    pw stuff:

  real(4) yopw(max(1,mpwdata))                             ! observations
  real(4) xbarbpw(max(1,mpwdata))                          ! guess (obs units)
  integer(4) ibighpw(lbig2ges,max(1,mpwdata))              ! H operator
  real(4) bighpw(lmetaex,lbig2ges,max(1,mpwdata))          !   (eta to obs)

  real(4),allocatable::epsbarpw(:),dbarpw(:),f0barpw(:),qbarpw(:),rbarpw(:)
  real(4),allocatable::palphabarpw(:,:)
  real(4),allocatable::galphabarpw(:,:)
  real(4),allocatable::gsdbarpw(:)
  real(4),allocatable::hsdbarpw(:)

!    wind stuff:
   
  real(4) yow(max(1,mwdata))                                  ! observations
  real(4) xbarbw(max(1,mwdata))                               ! guess (obs units)
  integer(4) ibighw(lbig3ges,max(1,mwdata))                   ! H operator
  real(4) bighw(lbig3ges,2,max(1,mwdata))                     !   (eta to obs)
  type(filter_cons) isofilter_w(14,2,2,2,2)                    !  e,rlxy,rlp   1 is psi, 2 is chi
  integer(4) iddapsi(3,2,nlambda),iddachi(3,2,nlambda)

  real(4),allocatable::epsw(:),rw(:)
  real(4),allocatable::epsbarw(:),dbarw(:),f0barw(:),qbarw(:),rbarw(:)
  real(4),allocatable::palphabarw(:,:)
  real(4),allocatable::galphabarw(:,:)
  real(4),allocatable::gsdbarw(:)
  real(4),allocatable::hsdbarw(:)

!    h stuff:

  real(4) yop(max(1,mpdata))                               ! observations
  real(4) xbarbp(max(1,mpdata))                  ! guess (obs units)
  integer(4) ibighp(lbig2ges,max(1,mpdata))                ! H operator
  real(4) bighp(lbig2ges,max(1,mpdata))                    !   (eta to obs)
  type(filter_cons) isofilter_p  (14,2,2)                   !  e,rlxy
  integer(4) iddap(2,2,nlambda)

  real(4),allocatable::epsp(:),rp(:)
  real(4),allocatable::epsbarps(:),dbarps(:),f0barps(:),qbarps(:),rbarps(:)
  real(4),allocatable::palphabarps(:,:)
  real(4),allocatable::galphabarps(:,:)
  real(4),allocatable::gsdbarps(:)
  real(4),allocatable::hsdbarps(:)

!    t stuff:

  real(4) yot(max(1,mtdata))                               ! observations
  real(4) xbarbt(max(1,mtdata))                            ! guess (obs units)
  integer(4) ibight(lbig3ges,max(1,mtdata))                ! H operator
  real(4) bight(lbig3ges,max(1,mtdata))                    !   (eta to obs)
  type(filter_cons) isofilter_t  (14,2,2,2)                 !  e,rlxy,rlp
  integer(4) iddat(3,2,nlambda)
  
  real(4),allocatable::epst(:),rt(:)
  real(4),allocatable::epsbart(:),dbart(:),f0bart(:),qbart(:),rbart(:)
  real(4),allocatable::palphabart(:,:)
  real(4),allocatable::galphabart(:,:)
  real(4),allocatable::gsdbart(:)
  real(4),allocatable::hsdbart(:)

!    rad stuff:

  real(4) yorad(max(1,mraddata))
  real(4) xbarbrad(max(1,mraddata))
  integer(4) ibighradh(lbig2ges,max(1,mraddata))
  real(4) bighradh(lbig2ges,max(1,mraddata))
  real(4) bighradv(2*lmetaex,max(1,mraddata))
  real(4) predrad(npred+jpchus-1,max(1,mraddata))
  integer(4) icxrad(2,max(1,mraddata))
  real(4) varpred(jpch,npred)
  logical userad

  real(4),allocatable::epspred(:),rpred(:)
  real(4),allocatable::epsbarrad(:),dbarrad(:),f0barrad(:),qbarrad(:),rbarrad(:)
  real(4),allocatable::palphabarrad(:,:)
  real(4),allocatable::galphabarrad(:,:)
  real(4),allocatable::gsdbarrad(:)
  real(4),allocatable::hsdbarrad(:)

!-------- Eeta:  interpolation constants for coarse grid to eta grid

  real(4) qsatetaanl(imeta,jmeta,lmetaex)
  real(4) bigeetah(imeta,jmeta,lbig2data)
  integer(4) ibigeetah(imeta,jmeta,lbig2data)
  real(4) bigeetav(imeta,jmeta,lbig2data,2)
  integer(4) ibigeetav(imeta,jmeta,lbig2data)

!--------  Ht_psi, Hp_psi: psi is balanced variable. These define balanced part of T, pdr

  real(4) bight_psi(lmetaex,lmetaex,lat1:lat2),bighp_psi(lmetaex,lat1:lat2)
  real(4) wgts_bightp(2,nxc*nyc)
  integer(4) iwgts_bightp(2,nxc*nyc)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
            lalpha_mask=1.
            lalpha_mask(1:9)=0.         !      set gradient to zero for all correlation length variables
                                   !     (because here we will only try to estimate amplitude parameters)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11

!      bring in previously computed parameters

! if(mype.eq.0) then
!  open(234,file='lambda_parms',form='unformatted',action='readwrite')
!  read(234) mlambda,mmetaex,lambda_in, &
!        rlenxyp0,rlenxyt0,rlenxyq0,rlenxypsi0,rlenxychi0, &
!              rlenpt0,rlenpq0,rlenppsi0,rlenpchi0, &
!              ep0,et0,bscaleq0,epsi0,echi0
!         if(mlambda.ne.nlambda.or.mmetaex.ne.lmetaex) then
!               print *,' PROBLEM UPDATING BACKGROUND ERROR PARMS'
!               print *,'     mlambda,nlambda=',mlambda,nlambda
!               print *,'     lmetaex,mmetaex=',lmetaex,mmetaex
!                stop
!         end if
!  close(234)
! end if
! call mpi_bcast(lambda_in,nlambda,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(rlenxyp0,1,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(rlenxyt0,lmetaex,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(rlenxyq0,lmetaex,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(rlenxypsi0,lmetaex,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(rlenxychi0,lmetaex,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(rlenpt0,lmetaex,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(rlenpq0,lmetaex,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(rlenppsi0,lmetaex,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(rlenpchi0,lmetaex,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(ep0,1,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(et0,lmetaex,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(bscaleq0,1,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(epsi0,lmetaex,mpi_real4,0,my_comm,ierr)
! call mpi_bcast(echi0,lmetaex,mpi_real4,0,my_comm,ierr)

!        to start with, just have lambda_in=0.

  lambda_in=0.

!   get eps, the random vector

  allocate(epsbart(max(1,mtdata))) ; allocate(epsbarw(max(1,mwdata)))
  allocate(epsbarq(max(1,mqdata))) ; allocate(epsbarpw(max(1,mpwdata)))
  allocate(epsbarps(max(1,mpdata))) ; allocate(epsbarrad(max(1,mraddata)))
  allocate(epspred(jpch*npred))
  allocate(epst(nxc*nyc*lmetaex))
  allocate(epsq(nxc*nyc*lmetaex))
  allocate(epsw(nxc*nyc*lmetaex*2))
  allocate(epsp(nxc*nyc))
  epsbart=0. ; epsbarw=0. ; epsbarq=0. ; epsbarpw=0.
  epsbarps=0. ; epsbarrad=0.
  epspred=0.
  epst=0.
  epsq=0.
  epsw=0.
  epsp=0.

  call gen_eps_bayes( &
      epsbart,epsbarw,epsbarq,epsbarpw,epsbarps,epsbarrad,epspred,epst,epsq,epsw,epsp, &
      mtdata,mwdata,mqdata,mpwdata,mpdata,mraddata, &
      npred,nxc,nyc,lmetaex,userad,myxsc,myxec,myysc,myyec,jpch)

!  initialize differencing indices and constants

         if(mype.eq.0) write(0,*)' at 1 in maxl_parameter_update_bayes'
  call set_1st_diffcons_bayes(iddaq,iddapsi,iddachi,iddap,iddat,dda,dlambda,nlambda)

do loopouter=1,maxouter       !    do a steepest descent minimization for starters

!  set up perturbed parameters so derivatives wrt each parameter can be estimated

         if(mype.eq.0) write(0,*)' at 2 in maxl_parameter_update_bayes'
  call perturbparms_bayes( &
               rlenxyp0,rlenxyt0,rlenxyq0,rlenxypsi0,rlenxychi0, &
               rlenpt0,rlenpq0,rlenppsi0,rlenpchi0, &
               ep0,et0,eq0,epsi0,echi0, &
                rlenxyp,rlenxyt,rlenxyq,rlenxypsi,rlenxychi, &
                rlenpt,rlenpq,rlenppsi,rlenpchi, &
                ep,et,eq,epsi,echi, &
                lat1,lat2,lmetaex,lambda_in,dlambda,nlambda,mype)

!  compute various perturbed background error filter constants

  do ie=1,2
   do i=1,2
    do k=1,2
         if(mype.eq.0) write(0,*)' at 3 in maxl_parameter_update_bayes, ie,i,k=',ie,i,k
     call get3berr(echi(1:lmetaex,lat1:lat2,ie),lat1,lat2,wgts_bightp,iwgts_bightp, &
                   rlenxychi(1,i),rlenpchi(1,k),isofilter_w(1,2,ie,i,k),npass,nsmooth, &
                   nxc,nyc,lmetaex,myxsc,myysc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
         if(mype.eq.0) write(0,*)' at 4 in maxl_parameter_update_bayes, ie,i,k=',ie,i,k
     call get3berr(et(1:lmetaex,lat1:lat2,ie),lat1,lat2,wgts_bightp,iwgts_bightp, &
                   rlenxyt(1,i),rlenpt(1,k),isofilter_t(1,ie,i,k),npass,nsmooth, &
                   nxc,nyc,lmetaex,myxsc,myysc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
         if(mype.eq.0) write(0,*)' at 5 in maxl_parameter_update_bayes, ie,i,k=',ie,i,k
     call get3berr(eq(1:lmetaex,lat1:lat2,ie),lat1,lat2,wgts_bightp,iwgts_bightp, &
                   rlenxyq(1,i),rlenpq(1,k),isofilter_q(1,ie,i,k),npass,nsmooth, &
                   nxc,nyc,lmetaex,myxsc,myysc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
         if(mype.eq.0) write(0,*)' at 6 in maxl_parameter_update_bayes, ie,i,k=',ie,i,k
     call get3berr(epsi(1:lmetaex,lat1:lat2,ie),lat1,lat2,wgts_bightp,iwgts_bightp, &
                   rlenxypsi(1,i),rlenppsi(1,k),isofilter_w(1,1,ie,i,k),npass,nsmooth, &
                   nxc,nyc,lmetaex,myxsc,myysc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
    end do
         if(mype.eq.0) write(0,*)' at 7 in maxl_parameter_update_bayes, ie,i=',ie,i
    call get2berrs(ep(lat1:lat2,ie),lat1,lat2,wgts_bightp,iwgts_bightp, &
                   rlenxyp(i),isofilter_p(1,ie,i),npass,nsmooth, &
                   nxc,nyc,myxsc,myysc, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
   end do
  end do

  
         if(mype.eq.0) write(0,*)' at 8 in maxl_parameter_update_bayes, ie,i=',ie,i

!------ define innovation vector


  allocate(dbart(max(1,mtdata))) ; allocate(dbarw(max(1,mwdata)))
  allocate(dbarq(max(1,mqdata))) ; allocate(dbarpw(max(1,mpwdata)))
  allocate(dbarps(max(1,mpdata))) ; allocate(dbarrad(max(1,mraddata)))
  dbart=yot-xbarbt ; dbarw=yow-xbarbw ; dbarq=yoq-xbarbq ; dbarpw=yopw-xbarbpw
  dbarps=yop-xbarbp ; dbarrad=yorad-xbarbrad
      if(mype.eq.0) write(0,*)' at 9 in maxl_parameter_update_bayes, mype,mwdata=',mype,mwdata
      if(mtdataall.gt.0) then
       call dot_prod8(prod_8,dbart,dbart,mtdata)
       rmsdbart=sqrt(prod_8/mtdataall)
       if(mype.eq.0) print *,' rmsdbart=',rmsdbart
      end if
      if(mwdataall.gt.0) then
       call dot_prod8(prod_8,dbarw,dbarw,mwdata)
       rmsdbarw=sqrt(prod_8/mwdataall)
       if(mype.eq.0) print *,' rmsdbarw=',rmsdbarw
      end if
      if(mqdataall.gt.0) then
       call dot_prod8(prod_8,dbarq,dbarq,mqdata)
       rmsdbarq=sqrt(prod_8/mqdataall)
       if(mype.eq.0) print *,' rmsdbarq=',rmsdbarq
      end if
      if(mpwdataall.gt.0) then
       call dot_prod8(prod_8,dbarpw,dbarpw,mpwdata)
       rmsdbarpw=sqrt(prod_8/mpwdataall)
       if(mype.eq.0) print *,' rmsdbarpw=',rmsdbarpw
      end if
      if(mpdataall.gt.0) then
       call dot_prod8(prod_8,dbarps,dbarps,mpdata)
       rmsdbarps=sqrt(prod_8/mpdataall)
       if(mype.eq.0) print *,' rmsdbarps=',rmsdbarps
      end if
      if(mraddataall.gt.0) then
       call dot_prod8(prod_8,dbarrad,dbarrad,mraddata)
       rmsdbarrad=sqrt(prod_8/mraddataall)
       if(mype.eq.0) print *,' rmsdbarrad=',rmsdbarrad
      end if
!--------
!-------- compute f0
!--------
  allocate(f0bart(max(1,mtdata))) ; allocate(f0barw(max(1,mwdata)))
  allocate(f0barq(max(1,mqdata))) ; allocate(f0barpw(max(1,mpwdata)))
  allocate(f0barps(max(1,mpdata))) ; allocate(f0barrad(max(1,mraddata)))
  allocate(rt(nxc*nyc*lmetaex))
  allocate(rq(nxc*nyc*lmetaex))
  allocate(rw(nxc*nyc*lmetaex*2))
  allocate(rp(nxc*nyc))
  allocate(rpred(jpch*npred))
  f0bart=dbart ; f0barw=dbarw ; f0barq=dbarq ; f0barpw=dbarpw ; f0barps=dbarps ; f0barrad=dbarrad
  deallocate(dbart) ; deallocate(dbarw)
  deallocate(dbarq) ; deallocate(dbarpw)
  deallocate(dbarps) ; deallocate(dbarrad)
  call bigqop_inv_bayes(maxinner,teststop, &
              f0barq,rq,mqdata,ibighq,bighq,isofilter_q(1,1,1,1), &
              f0barpw,mpwdata,ibighpw,bighpw, &
              f0barw,rw,mwdata,ibighw,bighw,isofilter_w(1,1,1,1,1),isofilter_w(1,2,1,1,1), &
              f0barps,rp,mpdata,ibighp,bighp,isofilter_p(1,1,1), &
              f0bart,rt,mtdata,ibight,bight,isofilter_t(1,1,1,1), &
              f0barrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
       nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!  if(maxouter.eq.1) then      
!                           generate analysis from f0bar and write out for diagnostic purposes
!    call bigbht_bayes( &
!             qetaanl,f0barq,rq,mqdata,ibighq,bighq,isofilter_q(1,1,1,1), &
!                     f0barpw,mpwdata,ibighpw,bighpw, &
!             wetaanl,f0barw,rw,mwdata,ibighw,bighw,isofilter_w(1,1,1,1,1),isofilter_w(1,2,1,1,1), &
!             petaanl,f0barps,rp,mpdata,ibighp,bighp,isofilter_p(1,1,1), &
!             tetaanl,f0bart,rt,mtdata,ibight,bight,isofilter_t(1,1,1,1), &
!                     f0barrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
!                       predrad,icxrad,varpred,npred,userad, &
!        lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
!        lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
!     nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
!               ids, ide, jds, jde, kds, kde, &                          ! domain indices
!               ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
!               ims, ime, jms, jme, kms, kme, &                          ! memory indices
!               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
!    call restore_eta_ges(tetages,qetages,q2etages,cwmetages,wetages(1,1,1,1),wetages(1,1,1,2),petages, &
!                imeta,jmeta,lmeta)

!    call etaupdate(tetaanl,qetaanl,q2etaanl,cwmetaanl,wetaanl(1,1,1,1),wetaanl(1,1,1,2),petaanl, &
!            imeta,jmeta,lmeta,iayear,iamonth,iaday,iahour,coldstart,cwm_adjust)
!        call mpi_finalize(ierr)
!        stop
!  end if

      if(mype.eq.0) write(0,*)' at 10 in maxl_parameter_update_bayes, mype,mwdata=',mype,mwdata
      if(mtdataall.gt.0) then
       call dot_prod8(prod_8,f0bart,f0bart,mtdata)
       rmsf0bart=sqrt(prod_8/mtdataall)
       if(mype.eq.0) print *,' rmsf0bart=',rmsf0bart
      end if
      if(mwdataall.gt.0) then
       call dot_prod8(prod_8,f0barw,f0barw,mwdata)
       rmsf0barw=sqrt(prod_8/mwdataall)
       if(mype.eq.0) print *,' rmsf0barw=',rmsf0barw
      end if
      if(mqdataall.gt.0) then
       call dot_prod8(prod_8,f0barq,f0barq,mqdata)
       rmsf0barq=sqrt(prod_8/mqdataall)
       if(mype.eq.0) print *,' rmsf0barq=',rmsf0barq
      end if
      if(mpwdataall.gt.0) then
       call dot_prod8(prod_8,f0barpw,f0barpw,mpwdata)
       rmsf0barpw=sqrt(prod_8/mpwdataall)
       if(mype.eq.0) print *,' rmsf0barpw=',rmsf0barpw
      end if
      if(mpdataall.gt.0) then
       call dot_prod8(prod_8,f0barps,f0barps,mpdata)
       rmsf0barps=sqrt(prod_8/mpdataall)
       if(mype.eq.0) print *,' rmsf0barps=',rmsf0barps
      end if
      if(mraddataall.gt.0) then
       call dot_prod8(prod_8,f0barrad,f0barrad,mraddata)
       rmsf0barrad=sqrt(prod_8/mraddataall)
       if(mype.eq.0) print *,' rmsf0barrad=',rmsf0barrad
      end if

!------  compute q

      if(mype.eq.0) write(0,*)' at 10.1 in maxl_parameter_update_bayes, mype,mtdata=',mype,mtdata
  allocate(qbart(max(1,mtdata)))
  allocate(qbarw(max(1,mwdata)))
  allocate(qbarpw(max(1,mpwdata)))
  allocate(qbarps(max(1,mpdata)))
  allocate(qbarrad(max(1,mraddata)))
  allocate(qbarq(max(1,mqdata)))
  qbart=epsbart ; qbarw=epsbarw ; qbarq=epsbarq ; qbarps=epsbarps ; qbarpw=epsbarpw ; qbarrad=epsbarrad
  rq=epsq ; rw=epsw ; rp=epsp ; rt=epst ; rpred=epspred
  call bigcop_bayes( &
              qbarq,rq,mqdata,ibighq,bighq,isofilter_q(1,1,1,1), &
              qbarpw,mpwdata,ibighpw,bighpw, &
              qbarw,rw,mwdata,ibighw,bighw,isofilter_w(1,1,1,1,1),isofilter_w(1,2,1,1,1), &
              qbarps,rp,mpdata,ibighp,bighp,isofilter_p(1,1,1), &
              qbart,rt,mtdata,ibight,bight,isofilter_t(1,1,1,1), &
              qbarrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
      nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
      if(mype.eq.0) write(0,*)' at 11 in maxl_parameter_update_bayes, mype,mwdata=',mype,mwdata
      if(mtdataall.gt.0) then
       call dot_prod8(prod_8,epsbart,epsbart,mtdata)
       rmsepsbart=sqrt(prod_8/mtdataall)
       if(mype.eq.0) print *,' rmsepsbart=',rmsepsbart
      end if
      if(mwdataall.gt.0) then
       call dot_prod8(prod_8,epsbarw,epsbarw,mwdata)
       rmsepsbarw=sqrt(prod_8/mwdataall)
       if(mype.eq.0) print *,' rmsepsbarw=',rmsepsbarw
      end if
      if(mqdataall.gt.0) then
       call dot_prod8(prod_8,epsbarq,epsbarq,mqdata)
       rmsepsbarq=sqrt(prod_8/mqdataall)
       if(mype.eq.0) print *,' rmsepsbarq=',rmsepsbarq
      end if
      if(mpwdataall.gt.0) then
       call dot_prod8(prod_8,epsbarpw,epsbarpw,mpwdata)
       rmsepsbarpw=sqrt(prod_8/mpwdataall)
       if(mype.eq.0) print *,' rmsepsbarpw=',rmsepsbarpw
      end if
      if(mpdataall.gt.0) then
       call dot_prod8(prod_8,epsbarps,epsbarps,mpdata)
       rmsepsbarps=sqrt(prod_8/mpdataall)
       if(mype.eq.0) print *,' rmsepsbarps=',rmsepsbarps
      end if
      if(mraddataall.gt.0) then
       call dot_prod8(prod_8,epsbarrad,epsbarrad,mraddata)
       rmsepsbarrad=sqrt(prod_8/mraddataall)
       if(mype.eq.0) print *,' rmsepsbarrad=',rmsepsbarrad
      end if

      if(mtdataall.gt.0) then
       call dot_prod8(prod_8,qbart,qbart,mtdata)
       rmsqbart=sqrt(prod_8/mtdataall)
       if(mype.eq.0) print *,' rmsqbart=',rmsqbart
      end if
      if(mwdataall.gt.0) then
       call dot_prod8(prod_8,qbarw,qbarw,mwdata)
       rmsqbarw=sqrt(prod_8/mwdataall)
       if(mype.eq.0) print *,' rmsqbarw=',rmsqbarw
      end if
      if(mqdataall.gt.0) then
       call dot_prod8(prod_8,qbarq,qbarq,mqdata)
       rmsqbarq=sqrt(prod_8/mqdataall)
       if(mype.eq.0) print *,' rmsqbarq=',rmsqbarq
      end if
      if(mpwdataall.gt.0) then
       call dot_prod8(prod_8,qbarpw,qbarpw,mpwdata)
       rmsqbarpw=sqrt(prod_8/mpwdataall)
       if(mype.eq.0) print *,' rmsqbarpw=',rmsqbarpw
      end if
      if(mpdataall.gt.0) then
       call dot_prod8(prod_8,qbarps,qbarps,mpdata)
       rmsqbarps=sqrt(prod_8/mpdataall)
       if(mype.eq.0) print *,' rmsqbarps=',rmsqbarps
      end if
      if(mraddataall.gt.0) then
       call dot_prod8(prod_8,qbarrad,qbarrad,mraddata)
       rmsqbarrad=sqrt(prod_8/mraddataall)
       if(mype.eq.0) print *,' rmsqbarrad=',rmsqbarrad
      end if

!-------- compute r

  allocate(rbart(max(1,mtdata))) ; allocate(rbarw(max(1,mwdata)))
  allocate(rbarq(max(1,mqdata))) ; allocate(rbarpw(max(1,mpwdata)))
  allocate(rbarps(max(1,mpdata))) ; allocate(rbarrad(max(1,mraddata)))
  rbart=qbart ; rbarw=qbarw ; rbarq=qbarq ; rbarpw=qbarpw ; rbarps=qbarps ; rbarrad=qbarrad
  deallocate(qbart)
  deallocate(qbarw)
  deallocate(qbarpw)
  deallocate(qbarps)
  deallocate(qbarrad)
  deallocate(qbarq)
  call bigqop_inv_bayes(maxinner,teststop, &
              rbarq,rq,mqdata,ibighq,bighq,isofilter_q(1,1,1,1), &
              rbarpw,mpwdata,ibighpw,bighpw, &
              rbarw,rw,mwdata,ibighw,bighw,isofilter_w(1,1,1,1,1),isofilter_w(1,2,1,1,1), &
              rbarps,rp,mpdata,ibighp,bighp,isofilter_p(1,1,1), &
              rbart,rt,mtdata,ibight,bight,isofilter_t(1,1,1,1), &
              rbarrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
       nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
      if(mype.eq.0) write(0,*)' at 12 in maxl_parameter_update_bayes, mype,mwdata=',mype,mwdata
      if(mtdataall.gt.0) then
       call dot_prod8(prod_8,rbart,rbart,mtdata)
       rmsrbart=sqrt(prod_8/mtdataall)
       if(mype.eq.0) print *,' rmsrbart=',rmsrbart
      end if
      if(mwdataall.gt.0) then
       call dot_prod8(prod_8,rbarw,rbarw,mwdata)
       rmsrbarw=sqrt(prod_8/mwdataall)
       if(mype.eq.0) print *,' rmsrbarw=',rmsrbarw
      end if
      if(mqdataall.gt.0) then
       call dot_prod8(prod_8,rbarq,rbarq,mqdata)
       rmsrbarq=sqrt(prod_8/mqdataall)
       if(mype.eq.0) print *,' rmsrbarq=',rmsrbarq
      end if
      if(mpwdataall.gt.0) then
       call dot_prod8(prod_8,rbarpw,rbarpw,mpwdata)
       rmsrbarpw=sqrt(prod_8/mpwdataall)
       if(mype.eq.0) print *,' rmsrbarpw=',rmsrbarpw
      end if
      if(mpdataall.gt.0) then
       call dot_prod8(prod_8,rbarps,rbarps,mpdata)
       rmsrbarps=sqrt(prod_8/mpdataall)
       if(mype.eq.0) print *,' rmsrbarps=',rmsrbarps
      end if
      if(mraddataall.gt.0) then
       call dot_prod8(prod_8,rbarrad,rbarrad,mraddata)
       rmsrbarrad=sqrt(prod_8/mraddataall)
       if(mype.eq.0) print *,' rmsrbarrad=',rmsrbarrad
      end if
!--------
!-------- compute palpha:
!--------
  allocate(palphabart(max(1,mtdata),nlambda)) ; allocate(palphabarw(max(1,mwdata),nlambda))
  allocate(palphabarq(max(1,mqdata),nlambda)) ; allocate(palphabarpw(max(1,mpwdata),nlambda))
  allocate(palphabarps(max(1,mpdata),nlambda)) ; allocate(palphabarrad(max(1,mraddata),nlambda))

      if(mype.eq.0) write(0,*)' at 13 in maxl_parameter_update_bayes, mype,mwdata=',mype,mwdata
  call bigqalphaop_bayes(dda,nlambda, &
              palphabarq,rbarq,rq,mqdata,ibighq,bighq,isofilter_q,iddaq, &
              palphabarpw,rbarpw,mpwdata,ibighpw,bighpw, &
              palphabarw,rbarw,rw,mwdata,ibighw,bighw,isofilter_w,iddapsi,iddachi, &
              palphabarps,rbarps,rp,mpdata,ibighp,bighp,isofilter_p,iddap, &
              palphabart,rbart,rt,mtdata,ibight,bight,isofilter_t,iddat, &
              palphabarrad,rbarrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
     nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
!--------
!-------- compute galpha:
!--------
      if(mype.eq.0) write(0,*)' at 14 in maxl_parameter_update_bayes, mype,mwdata=',mype,mwdata
  allocate(galphabart(max(1,mtdata),nlambda)) ; allocate(galphabarw(max(1,mwdata),nlambda))
  allocate(galphabarq(max(1,mqdata),nlambda)) ; allocate(galphabarpw(max(1,mpwdata),nlambda))
  allocate(galphabarps(max(1,mpdata),nlambda)) ; allocate(galphabarrad(max(1,mraddata),nlambda))

      if(mype.eq.0) write(0,*)' at 15 in maxl_parameter_update_bayes, mype,mwdata=',mype,mwdata
  call bigqalphaop_bayes(dda,nlambda, &
              galphabarq,f0barq,rq,mqdata,ibighq,bighq,isofilter_q,iddaq, &
              galphabarpw,f0barpw,mpwdata,ibighpw,bighpw, &
              galphabarw,f0barw,rw,mwdata,ibighw,bighw,isofilter_w,iddapsi,iddachi, &
              galphabarps,f0barps,rp,mpdata,ibighp,bighp,isofilter_p,iddap, &
              galphabart,f0bart,rt,mtdata,ibight,bight,isofilter_t,iddat, &
              galphabarrad,f0barrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
     nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
!--------
!-------- get lambda_alpha and gsd
!--------
      if(mype.eq.0) write(0,*)' at 16 in maxl_parameter_update_bayes, mype,mwdata=',mype,mwdata
  allocate(gsdbart(max(1,mtdata))) ; allocate(gsdbarw(max(1,mwdata)))
  allocate(gsdbarq(max(1,mqdata))) ; allocate(gsdbarpw(max(1,mpwdata)))
  allocate(gsdbarps(max(1,mpdata))) ; allocate(gsdbarrad(max(1,mraddata)))
  gsdbarq=0. ;  gsdbarpw=0. ;  gsdbarw=0.
  gsdbarps=0. ;  gsdbart=0. ;  gsdbarrad=0.
  rnorm_lalpha=0.
  do i=1,nlambda

!       compute gradient lalpha (note that gradient has been deflated by factor of 2 (p/(p+1),
!                           where p is the random vector sample size, p=1 here)

   lalpha(i)=.25_8*(obs_space_prod_8( &
       rbart,rbarw,rbarq,rbarpw,rbarps,rbarrad, &
       palphabart(1,i),palphabarw(1,i),palphabarq(1,i),palphabarpw(1,i),palphabarps(1,i),palphabarrad(1,i), &
       mtdata,mwdata,mqdata,mpwdata,mpdata,mraddata,userad)  &
                 -obs_space_prod_8( &
       f0bart,f0barw,f0barq,f0barpw,f0barps,f0barrad, &
       galphabart(1,i),galphabarw(1,i),galphabarq(1,i),galphabarpw(1,i),galphabarps(1,i),galphabarrad(1,i), &
               mtdata,mwdata,mqdata,mpwdata,mpdata,mraddata,userad) )

!    project galpha on direction of lalpha

   rnorm_lalpha=rnorm_lalpha+lalpha_mask(i)*lalpha(i)**2
   gsdbarq=gsdbarq+lalpha_mask(i)*lalpha(i)*galphabarq(:,i)
   gsdbarpw=gsdbarpw+lalpha_mask(i)*lalpha(i)*galphabarpw(:,i)
   gsdbarw=gsdbarw+lalpha_mask(i)*lalpha(i)*galphabarw(:,i)
   gsdbarps=gsdbarps+lalpha_mask(i)*lalpha(i)*galphabarps(:,i)
   gsdbart=gsdbart+lalpha_mask(i)*lalpha(i)*galphabart(:,i)
   gsdbarrad=gsdbarrad+lalpha_mask(i)*lalpha(i)*galphabarrad(:,i)

      if(mype.eq.0) print *,' lalpha(',i,')=',lalpha(i)
      if(mype.eq.0) print *,' lalpha_used(',i,')=',lalpha_mask(i)*lalpha(i)
      if(mype.eq.0) write(0,*)' lalpha(',i,')=',lalpha(i)
      if(mype.eq.0) write(0,*)' lalpha_used(',i,')=',lalpha_mask(i)*lalpha(i)
  end do
  deallocate(f0bart) ; deallocate(f0barw)
  deallocate(f0barq) ; deallocate(f0barpw)
  deallocate(f0barps) ; deallocate(f0barrad)
!        get normalized lalpha and projected galpha
  rnorm_lalpha=1./sqrt(rnorm_lalpha)
  lalphahat=rnorm_lalpha*lalpha
  gsdbarq=rnorm_lalpha*gsdbarq ; gsdbarpw=rnorm_lalpha*gsdbarpw ; gsdbarw=rnorm_lalpha*gsdbarw
  gsdbarps=rnorm_lalpha*gsdbarps ; gsdbart=rnorm_lalpha*gsdbart ; gsdbarrad=rnorm_lalpha*gsdbarrad

  allocate(hsdbart(max(1,mtdata))) ; allocate(hsdbarw(max(1,mwdata)))
  allocate(hsdbarq(max(1,mqdata))) ; allocate(hsdbarpw(max(1,mpwdata)))
  allocate(hsdbarps(max(1,mpdata))) ; allocate(hsdbarrad(max(1,mraddata)))
  
  hsdbarq=gsdbarq ; hsdbarpw=gsdbarpw ; hsdbarw=gsdbarw
  hsdbarps=gsdbarps ; hsdbart=gsdbart ; hsdbarrad=gsdbarrad
  
! if(loopouter.eq.1.or.mod(loopouter,2).ne.0) then
   call bigqop_inv_bayes(maxinner,teststop, &
              hsdbarq,rq,mqdata,ibighq,bighq,isofilter_q(1,1,1,1), &
              hsdbarpw,mpwdata,ibighpw,bighpw, &
              hsdbarw,rw,mwdata,ibighw,bighw,isofilter_w(1,1,1,1,1),isofilter_w(1,2,1,1,1), &
              hsdbarps,rp,mpdata,ibighp,bighp,isofilter_p(1,1,1), &
              hsdbart,rt,mtdata,ibight,bight,isofilter_t(1,1,1,1), &
              hsdbarrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
       nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!    get projection of hessian along steepest descent  (estimate of curvature along this line)

   lalphabeta_sd=.5_8*obs_space_prod_8( &
       gsdbart,gsdbarw,gsdbarq,gsdbarpw,gsdbarps,gsdbarrad, &
       hsdbart,hsdbarw,hsdbarq,hsdbarpw,hsdbarps,hsdbarrad, &
       mtdata,mwdata,mqdata,mpwdata,mpdata,mraddata,userad)
! end if

  if(mype.eq.0) print *,' lalphabeta_sd=',lalphabeta_sd
  if(mype.eq.0) write(0,*)' lalphabeta_sd=',lalphabeta_sd

!   update lambda_in, then go to next outer loop

!??????????????????????--decide what to do with update of lambda
  do i=1,nlambda
   lambda_in(i)=lambda_in(i)-lalpha_mask(i)*lalpha(i)/lalphabeta_sd
  end do
  if(mype.eq.0) then
   open(235,file='lalpha_parms',form='unformatted',action='readwrite')
   write(235) nlambda,lmetaex,lalpha,lalpha_mask,lalphabeta_sd
   close(235)
  end if
! if(mype.eq.0) then
!  open(234,file='lambda_parms',form='unformatted',action='readwrite')
!  write(234) nlambda,lmetaex,lambda_in, &
!        rlenxyp0,rlenxyt0,rlenxyq0,rlenxypsi0,rlenxychi0, &
!              rlenpt0,rlenpq0,rlenppsi0,rlenpchi0, &
!              ep0,et0,bscaleq0,epsi0,echi0
!  close(234)
! end if
  deallocate(rt)
  deallocate(rq)
  deallocate(rw)
  deallocate(rp)
  deallocate(rpred)
  deallocate(rbart) ; deallocate(rbarw)
  deallocate(rbarq) ; deallocate(rbarpw)
  deallocate(rbarps) ; deallocate(rbarrad)
  deallocate(palphabart) ; deallocate(palphabarw)
  deallocate(palphabarq) ; deallocate(palphabarpw)
  deallocate(palphabarps) ; deallocate(palphabarrad)
  deallocate(galphabart) ; deallocate(galphabarw)
  deallocate(galphabarq) ; deallocate(galphabarpw)
  deallocate(galphabarps) ; deallocate(galphabarrad)
  deallocate(gsdbart) ; deallocate(gsdbarw)
  deallocate(gsdbarq) ; deallocate(gsdbarpw)
  deallocate(gsdbarps) ; deallocate(gsdbarrad)
  deallocate(hsdbart) ; deallocate(hsdbarw)
  deallocate(hsdbarq) ; deallocate(hsdbarpw)
  deallocate(hsdbarps) ; deallocate(hsdbarrad)

end do
  deallocate(epsbart) ; deallocate(epsbarw) ; deallocate(epsbarq)
  deallocate(epsbarpw) ; deallocate(epsbarps) ; deallocate(epsbarrad)
  deallocate(epsq) ; deallocate(epsw) ; deallocate(epsp) ; deallocate(epst) ; deallocate(epspred)
                 if(mype.gt.-1000) then
                      call mpi_finalize(ierr)
                      stop
                 end if
return
end subroutine maxl_parameter_update_bayes
