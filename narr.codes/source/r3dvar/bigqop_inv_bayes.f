subroutine bigqop_inv_bayes(maxinner,teststop, &
              xbarq,rq,mqdata,ibighq,bighq,isofilter_q, &
              xbarpw,mpwdata,ibighpw,bighpw, &
              xbarw,rw,mwdata,ibighw,bighw,isofilter_psi,isofilter_chi, &
              xbarps,rp,mpdata,ibighp,bighp,isofilter_p, &
              xbart,rt,mtdata,ibight,bight,isofilter_t, &
              xbarrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
         nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!                                   T
!      solve Qx = f where  Q = ( HBH + R )
!

  include 'mpif.h'
         include "my_comm.h"
  include 'filtertype.h'

  INTEGER(4), INTENT(IN) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

!    moisture stuff:

  real(4) xbarq(max(1,mqdata))                             ! input analysis inc (obs units)
  real(4) rq(nxc,nyc,lmetaex)                              ! output gradient
  integer(4) ibighq(lbig3ges,max(1,mqdata))                ! H operator
  real(4) bighq(lbig3ges,max(1,mqdata))                    !   (eta to obs)
  type(filter_cons) isofilter_q(14)                        ! coarse background error Ctilde

  real(4),allocatable::pbarq(:)
  real(4),allocatable::qbarq(:)
  real(4),allocatable::rbarq(:)

!    pw stuff:

  real(4) xbarpw(max(1,mpwdata))                           ! input analysis inc (obs units)
  integer(4) ibighpw(lbig2ges,max(1,mpwdata))              ! H operator
  real(4) bighpw(lmetaex,lbig2ges,max(1,mpwdata))          !   (eta to obs)

  real(4),allocatable::pbarpw(:)
  real(4),allocatable::qbarpw(:)
  real(4),allocatable::rbarpw(:)

!    wind stuff:

  real(4) xbarw(max(1,mwdata))                                ! input analysis inc (obs units)
  real(4) rw(nxc,nyc,lmetaex,2)                               ! output gradient      (1=psi,u, 2=chi,v)
  integer(4) ibighw(lbig3ges,max(1,mwdata))                   ! H operator
  real(4) bighw(lbig3ges,2,max(1,mwdata))                     !   (eta to obs)
  type(filter_cons) isofilter_psi(14),isofilter_chi(14)       ! coarse background error Ctilde

  real(4),allocatable::pbarw(:)
  real(4),allocatable::qbarw(:)
  real(4),allocatable::rbarw(:)

!    h stuff:

  real(4) xbarps(max(1,mpdata))                   ! input analysis inc (obs units)
  real(4) rp(nxc,nyc)                                      ! output gradient
  integer(4) ibighp(lbig2ges,max(1,mpdata))                ! H operator
  real(4) bighp(lbig2ges,max(1,mpdata))                    !   (eta to obs)
  type(filter_cons) isofilter_p(14)                        ! coarse background error Ctilde

  real(4),allocatable::pbarps(:)
  real(4),allocatable::qbarps(:)
  real(4),allocatable::rbarps(:)

!    t stuff:

  real(4) xbart(max(1,mtdata))                             ! input analysis inc (obs units)
  real(4) rt(nxc,nyc,lmetaex)                              ! output gradient
  integer(4) ibight(lbig3ges,max(1,mtdata))                ! H operator
  real(4) bight(lbig3ges,max(1,mtdata))                    !   (eta to obs)
  type(filter_cons) isofilter_t(14)                        ! coarse background error Ctilde

  real(4),allocatable::pbart(:)
  real(4),allocatable::qbart(:)
  real(4),allocatable::rbart(:)

!    rad stuff:

  real(4) xbarrad(max(1,mraddata))
  real(4) rpred(jpch,npred)
  integer(4) ibighradh(lbig2ges,max(1,mraddata))
  real(4) bighradh(lbig2ges,max(1,mraddata))
  real(4) bighradv(2*lmetaex,max(1,mraddata))
  real(4) predrad(npred+jpchus-1,max(1,mraddata))
  integer(4) icxrad(2,max(1,mraddata))
  real(4) varpred(jpch,npred)
  logical userad

  real(4),allocatable::pbarrad(:)
  real(4),allocatable::qbarrad(:)
  real(4),allocatable::rbarrad(:)

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

  real(8) obs_space_prod_8,prod_8

  allocate(pbarq(max(1,mqdata)))
  allocate(pbarpw(max(1,mpwdata)))
  allocate(pbarw(max(1,mwdata)))
  allocate(pbarps(max(1,mpdata)))
  allocate(pbart(max(1,mtdata)))
  allocate(pbarrad(max(1,mraddata)))

  allocate(qbarq(max(1,mqdata)))
  allocate(qbarpw(max(1,mpwdata)))
  allocate(qbarw(max(1,mwdata)))
  allocate(qbarps(max(1,mpdata)))
  allocate(qbart(max(1,mtdata)))
  allocate(qbarrad(max(1,mraddata)))

  allocate(rbarq(max(1,mqdata)))
  allocate(rbarpw(max(1,mpwdata)))
  allocate(rbarw(max(1,mwdata)))
  allocate(rbarps(max(1,mpdata)))
  allocate(rbart(max(1,mtdata)))
  allocate(rbarrad(max(1,mraddata)))

  rbarq=-xbarq
  rbarpw=-xbarpw
  rbarw=-xbarw
  rbarps=-xbarps
  rbart=-xbart
  rbarrad=-xbarrad

  xbarq=0.
  xbarpw=0.
  xbarw=0.
  xbarps=0.
  xbart=0.
  xbarrad=0.

  pbarq=0.
  pbarpw=0.
  pbarw=0.
  pbarps=0.
  pbart=0.
  pbarrad=0.

  beta=0.

  rnorm=obs_space_prod_8( &
                        rbart,rbarw,rbarq,rbarpw,rbarps,rbarrad, &
                        rbart,rbarw,rbarq,rbarpw,rbarps,rbarrad, &
                    mtdata,mwdata,mqdata,mpwdata,mpdata,mraddata,userad)
  refnorm=rnorm
      if(mype.eq.0) print *,' refnorm=',refnorm
      if(mype.eq.0) write(0,*)' refnorm=',refnorm

  do loop=1,maxinner
   pbarq=beta*pbarq-rbarq
   pbarpw=beta*pbarpw-rbarpw
   pbarw=beta*pbarw-rbarw
   pbarps=beta*pbarps-rbarps
   pbart=beta*pbart-rbart
   pbarrad=beta*pbarrad-rbarrad

   qbarq=pbarq
   qbarpw=pbarpw
   qbarw=pbarw
   qbarps=pbarps
   qbart=pbart
   qbarrad=pbarrad

   call bigqop_bayes( &
              qbarq,rq,mqdata,ibighq,bighq,isofilter_q, &
              qbarpw,mpwdata,ibighpw,bighpw, &
              qbarw,rw,mwdata,ibighw,bighw,isofilter_psi,isofilter_chi, &
              qbarps,rp,mpdata,ibighp,bighp,isofilter_p, &
              qbart,rt,mtdata,ibight,bight,isofilter_t, &
              qbarrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
        nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
   alpha=rnorm/obs_space_prod_8( &
                        pbart,pbarw,pbarq,pbarpw,pbarps,pbarrad, &
                        qbart,qbarw,qbarq,qbarpw,qbarps,qbarrad, &
                    mtdata,mwdata,mqdata,mpwdata,mpdata,mraddata,userad)
   xbarq=alpha*pbarq+xbarq
   xbarpw=alpha*pbarpw+xbarpw
   xbarw=alpha*pbarw+xbarw
   xbarps=alpha*pbarps+xbarps
   xbart=alpha*pbart+xbart
   xbarrad=alpha*pbarrad+xbarrad
   rbarq=alpha*qbarq+rbarq
   rbarpw=alpha*qbarpw+rbarpw
   rbarw=alpha*qbarw+rbarw
   rbarps=alpha*qbarps+rbarps
   rbart=alpha*qbart+rbart
   rbarrad=alpha*qbarrad+rbarrad
   rnormold=rnorm
   rnorm=obs_space_prod_8( &
                        rbart,rbarw,rbarq,rbarpw,rbarps,rbarrad, &
                        rbart,rbarw,rbarq,rbarpw,rbarps,rbarrad, &
                    mtdata,mwdata,mqdata,mpwdata,mpdata,mraddata,userad)
   if(mype.eq.0) print *,' loop,rnorm=',loop,rnorm 
   if(mype.eq.0) write(0,*)' loop,rnorm=',loop,rnorm 
   if(rnorm.lt.teststop*refnorm) exit
   beta=rnorm/rnormold
  end do

  deallocate(pbarq)
  deallocate(pbarpw)
  deallocate(pbarw)
  deallocate(pbarps)
  deallocate(pbart)
  deallocate(pbarrad)

  deallocate(qbarq)
  deallocate(qbarpw)
  deallocate(qbarw)
  deallocate(qbarps)
  deallocate(qbart)
  deallocate(qbarrad)

  deallocate(rbarq)
  deallocate(rbarpw)
  deallocate(rbarw)
  deallocate(rbarps)
  deallocate(rbart)
  deallocate(rbarrad)

return
end subroutine bigqop_inv_bayes
