subroutine bigqop_bayes( &
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

!                             T
!    apply operator Qx = ( HBH + R )x
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

  real(4),allocatable::resq(:)                             ! intermediate non-linear residuals
  real(4),allocatable::qetadir(:,:,:)                      !  intermediate descent direction

!    pw stuff:

  real(4) xbarpw(max(1,mpwdata))                           ! input analysis inc (obs units)
  integer(4) ibighpw(lbig2ges,max(1,mpwdata))              ! H operator
  real(4) bighpw(lmetaex,lbig2ges,max(1,mpwdata))          !   (eta to obs)

  real(4),allocatable::respw(:)                            ! intermediate non-linear residuals

!    wind stuff:

  real(4) xbarw(max(1,mwdata))                                ! input analysis inc (obs units)
  real(4) rw(nxc,nyc,lmetaex,2)                               ! output gradient      (1=psi,u, 2=chi,v)
  integer(4) ibighw(lbig3ges,max(1,mwdata))                   ! H operator
  real(4) bighw(lbig3ges,2,max(1,mwdata))                     !   (eta to obs)
  type(filter_cons) isofilter_psi(14),isofilter_chi(14)       ! coarse background error Ctilde

  real(4),allocatable::resw(:)                                ! intermediate non-linear residuals
  real(4),allocatable::wetadir(:,:,:,:)                       !  intermediate descent direction

!    h stuff:

  real(4) xbarps(max(1,mpdata))                   ! input analysis inc (obs units)
  real(4) rp(nxc,nyc)                                      ! output gradient
  integer(4) ibighp(lbig2ges,max(1,mpdata))                ! H operator
  real(4) bighp(lbig2ges,max(1,mpdata))                    !   (eta to obs)
  type(filter_cons) isofilter_p(14)                        ! coarse background error Ctilde

  real(4),allocatable::resps(:)                            ! intermediate non-linear residuals
  real(4),allocatable::petadir(:,:)                        !  intermediate descent direction

!    t stuff:

  real(4) xbart(max(1,mtdata))                             ! input analysis inc (obs units)
  real(4) rt(nxc,nyc,lmetaex)                              ! output gradient
  integer(4) ibight(lbig3ges,max(1,mtdata))                ! H operator
  real(4) bight(lbig3ges,max(1,mtdata))                    !   (eta to obs)
  type(filter_cons) isofilter_t(14)                        ! coarse background error Ctilde

  real(4),allocatable::rest(:)                             ! intermediate non-linear residuals
  real(4),allocatable::tetadir(:,:,:)                      !  intermediate descent direction

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

  real(4),allocatable::resrad(:)                             ! intermediate non-linear residuals

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
              real(8) prod_8

  allocate(resq(max(1,mqdata)))
  allocate(respw(max(1,mpwdata)))
  allocate(resw(max(1,mwdata)))
  allocate(resps(max(1,mpdata)))
  allocate(rest(max(1,mtdata)))
  allocate(resrad(max(1,mraddata)))
  allocate(qetadir(imeta,jmeta,lmetaex))
  allocate(wetadir(imeta,jmeta,lmetaex,2))
  allocate(petadir(imeta,jmeta))
  allocate(tetadir(imeta,jmeta,lmetaex))

  resq=xbarq
  respw=xbarpw
  resw=xbarw
  resps=xbarps
  rest=xbart
  resrad=xbarrad

!              T T    T
!       apply H Eeta C

!       if(mype.eq.0) write(0,*)' at 3 in bigqop'
  call ad_bighop(qetadir,resq,mqdata,ibighq,bighq, &
                       respw,mpwdata,ibighpw,bighpw, &
                 wetadir,resw,mwdata,ibighw,bighw, &
                 petadir,resps,mpdata,ibighp,bighp, &
                 tetadir,rest,mtdata,ibight,bight, &
                 rpred,resrad,mraddata,ibighradh,bighradh,bighradv,icxrad,predrad,npred,userad, &
                 lbig2ges,lbig3ges,imeta,jmeta,lmetaex,jpch,jpchus)
  call ad_bigeetahopq(rq,qetadir,imeta,jmeta,lmetaex,lbig2data, &
                     nxc,nyc,myis2,myie2,myjs2,myje2,qsatetaanl,bigeetah,ibigeetah)
  call ad_bigeetavop(rw,wetadir,imeta,jmeta,lmetaex,lbig2data, &
                     nxc,nyc,myis2,myie2,myjs2,myje2,bigeetav,ibigeetav)
!       if(mype.eq.0) write(0,*)' at 4 in bigqop'
  call ad_bigeetahop(rt,tetadir,imeta,jmeta,lmetaex,lbig2data, &
                     nxc,nyc,myis2,myie2,myjs2,myje2,bigeetah,ibigeetah)
  call ad_bigeetahop(rp,petadir,imeta,jmeta,1,lbig2data, &
                     nxc,nyc,myis2,myie2,myjs2,myje2,bigeetah,ibigeetah)
  call ad_slow_plus_fast(rt,rp,rw(1,1,1,1), &
                         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp,nxc,nyc,lmetaex)
  call regular_raf(rq,isofilter_q)
  call regular_raf(rw(1,1,1,1),isofilter_psi)
  call regular_raf(rw(1,1,1,2),isofilter_chi)
  call regular_raf(rt,isofilter_t)
!       if(mype.eq.0) write(0,*)' at 5 in bigqop'
  call regular_raf(rp,isofilter_p)
  if(userad) rpred=varpred*rpred


!   now apply C Eeta H

  if(userad) rpred=varpred*rpred

  call regular_ad_raf(rq,isofilter_q)
  call regular_ad_raf(rw(1,1,1,1),isofilter_psi)
  call regular_ad_raf(rw(1,1,1,2),isofilter_chi)
  call regular_ad_raf(rt,isofilter_t)
  call regular_ad_raf(rp,isofilter_p)

  call slow_plus_fast(rt,rp,rw(1,1,1,1), &
                      lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp,nxc,nyc,lmetaex)
  call bigeetahop(rq,qetadir,imeta,jmeta,lmetaex,lbig2data, &
                   nxc,nyc,myis2,myie2,myjs2,myje2,bigeetah,ibigeetah)
  call bigeetavop(rw,wetadir,imeta,jmeta,lmetaex,lbig2data, &
                   nxc,nyc,myis2,myie2,myjs2,myje2,bigeetav,ibigeetav)
  call bigeetahop(rt,tetadir,imeta,jmeta,lmetaex,lbig2data, &
                   nxc,nyc,myis2,myie2,myjs2,myje2,bigeetah,ibigeetah)
  call bigeetahop(rp,petadir,imeta,jmeta,1,lbig2data, &
                   nxc,nyc,myis2,myie2,myjs2,myje2,bigeetah,ibigeetah)

  call bighop(qetadir,resq,mqdata,ibighq,bighq, &
                       respw,mpwdata,ibighpw,bighpw, &
               wetadir,resw,mwdata,ibighw,bighw, &
               petadir,resps,mpdata,ibighp,bighp, &
               tetadir,rest,mtdata,ibight,bight, &
             rpred,resrad,mraddata,ibighradh,bighradh,bighradv,icxrad,predrad,npred,userad, &
               lbig2ges,lbig3ges,imeta,jmeta,lmetaex,jpch,jpchus)

  xbarq=xbarq+resq
  xbarpw=xbarpw+respw
  xbarw=xbarw+resw
  xbarps=xbarps+resps
  xbart=xbart+rest
  xbarrad=xbarrad+resrad

  deallocate(resq)
  deallocate(respw)
  deallocate(resw)
  deallocate(resps)
  deallocate(rest)
  deallocate(resrad)
  deallocate(qetadir)
  deallocate(wetadir)
  deallocate(petadir)
  deallocate(tetadir)


return
end subroutine bigqop_bayes
