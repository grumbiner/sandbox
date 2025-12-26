subroutine bigqalphaop_bayes(dda,nlambda, &
              palphabarq,xbarq,rq,mqdata,ibighq,bighq,isofilter_q,iddaq, &
              palphabarpw,xbarpw,mpwdata,ibighpw,bighpw, &
              palphabarw,xbarw,rw,mwdata,ibighw,bighw,isofilter_w,iddapsi,iddachi, &
              palphabarps,xbarps,rp,mpdata,ibighp,bighp,isofilter_p,iddap, &
              palphabart,xbart,rt,mtdata,ibight,bight,isofilter_t,iddat, &
              palphabarrad,xbarrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
         nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info


!                                     T
!    apply operator (Qalpha)x = (( HBH + R )alpha)x
!
!              where Qalpha is derivative of Q wrt parameter lambda(alpha)
!

  include 'mpif.h'
         include "my_comm.h"
  include 'filtertype.h'

  INTEGER(4), INTENT(IN) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  real(4) dda(2,nlambda)

!    moisture stuff:

  real(4) palphabarq(max(1,mqdata),nlambda)                ! output (obs units)
  real(4) xbarq(max(1,mqdata))                             ! input analysis inc (obs units)
  real(4) rq(nxc,nyc,lmetaex)                              ! output gradient
  integer(4) ibighq(lbig3ges,max(1,mqdata))                ! H operator
  real(4) bighq(lbig3ges,max(1,mqdata))                    !   (eta to obs)
  type(filter_cons) isofilter_q(14,2,2,2)                  ! coarse background error Ctilde
  integer(4) iddaq(3,2,nlambda)                            ! differencing indices

  real(4),allocatable::resq(:)                                ! intermediate non-linear residuals
  real(4),allocatable::pesq(:)                                ! intermediate non-linear residuals

!    pw stuff:

  real(4) palphabarpw(max(1,mpwdata),nlambda)                ! output (obs units)
  real(4) xbarpw(max(1,mpwdata))                           ! input analysis inc (obs units)
  integer(4) ibighpw(lbig2ges,max(1,mpwdata))              ! H operator
  real(4) bighpw(lmetaex,lbig2ges,max(1,mpwdata))          !   (eta to obs)

  real(4),allocatable::respw(:)                            ! intermediate non-linear residuals
  real(4),allocatable::pespw(:)                            ! intermediate non-linear residuals

!    wind stuff:

  real(4) palphabarw(max(1,mwdata),nlambda)                ! output (obs units)
  real(4) xbarw(max(1,mwdata))                                ! input analysis inc (obs units)
  real(4) rw(nxc,nyc,lmetaex,2)                               ! output gradient      (1=psi,u, 2=chi,v)
  integer(4) ibighw(lbig3ges,max(1,mwdata))                   ! H operator
  real(4) bighw(lbig3ges,2,max(1,mwdata))                     !   (eta to obs)
  type(filter_cons) isofilter_w(14,2,2,2,2)                  ! coarse background error Ctilde (1=psi,2=chi)
  integer(4) iddapsi(3,2,nlambda)                            ! differencing indices
  integer(4) iddachi(3,2,nlambda)                            ! differencing indices

  real(4),allocatable::resw(:)                                ! intermediate non-linear residuals
  real(4),allocatable::pesw(:)                                ! intermediate non-linear residuals

!    h stuff:

  real(4) palphabarps(max(1,mpdata),nlambda)                ! output (obs units)
  real(4) xbarps(max(1,mpdata))                   ! input analysis inc (obs units)
  real(4) rp(nxc,nyc)                                      ! output gradient
  integer(4) ibighp(lbig2ges,max(1,mpdata))                ! H operator
  real(4) bighp(lbig2ges,max(1,mpdata))                    !   (eta to obs)
  type(filter_cons) isofilter_p(14,2,2)                    ! coarse background error Ctilde
  integer(4) iddap(2,2,nlambda)                            ! differencing indices

  real(4),allocatable::resps(:)                           ! intermediate non-linear residuals
  real(4),allocatable::pesps(:)                           ! intermediate non-linear residuals

!    t stuff:

  real(4) palphabart(max(1,mtdata),nlambda)                ! output (obs units)
  real(4) xbart(max(1,mtdata))                             ! input analysis inc (obs units)
  real(4) rt(nxc,nyc,lmetaex)                              ! output gradient
  integer(4) ibight(lbig3ges,max(1,mtdata))                ! H operator
  real(4) bight(lbig3ges,max(1,mtdata))                    !   (eta to obs)
  type(filter_cons) isofilter_t(14,2,2,2)                  ! coarse background error Ctilde
  integer(4) iddat(3,2,nlambda)                            ! differencing indices

  real(4),allocatable::rest(:)                             ! intermediate non-linear residuals
  real(4),allocatable::pest(:)                             ! intermediate non-linear residuals

!    rad stuff:

  real(4) palphabarrad(max(1,mraddata),nlambda)            ! output (obs units)
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
  real(4),allocatable::pesrad(:)                             ! intermediate non-linear residuals

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

  allocate(resq(max(1,mqdata)))
  allocate(respw(max(1,mpwdata)))
  allocate(resw(max(1,mwdata)))
  allocate(resps(max(1,mpdata)))
  allocate(rest(max(1,mtdata)))
  allocate(resrad(max(1,mraddata)))
  allocate(pesq(max(1,mqdata)))
  allocate(pespw(max(1,mpwdata)))
  allocate(pesw(max(1,mwdata)))
  allocate(pesps(max(1,mpdata)))
  allocate(pest(max(1,mtdata)))
  allocate(pesrad(max(1,mraddata)))


  resq=xbarq ; respw=xbarpw ; resw=xbarw ; resps=xbarps ; rest=xbart ; resrad=xbarrad
  call bigqop_bayes( &
              resq,rq,mqdata,ibighq,bighq,isofilter_q(1,1,1,1), &
              respw,mpwdata,ibighpw,bighpw, &
              resw,rw,mwdata,ibighw,bighw,isofilter_w(1,1,1,1,1),isofilter_w(1,2,1,1,1), &
              resps,rp,mpdata,ibighp,bighp,isofilter_p(1,1,1), &
              rest,rt,mtdata,ibight,bight,isofilter_t(1,1,1,1), &
              resrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
       nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
  do ial=1,nlambda
   k=1
   pesq=xbarq ; pespw=xbarpw ; pesw=xbarw ; pesps=xbarps ; pest=xbart ; pesrad=xbarrad
   call bigqop_bayes( &
              pesq,rq,mqdata,ibighq,bighq, &
                 isofilter_q(1,iddaq(1,k,ial),iddaq(2,k,ial),iddaq(3,k,ial)), &
              pespw,mpwdata,ibighpw,bighpw, &
              pesw,rw,mwdata,ibighw,bighw, &
                 isofilter_w(1,1,iddapsi(1,k,ial),iddapsi(2,k,ial),iddapsi(3,k,ial)), &
                 isofilter_w(1,2,iddachi(1,k,ial),iddachi(2,k,ial),iddachi(3,k,ial)), &
              pesps,rp,mpdata,ibighp,bighp, &
                 isofilter_p(1,iddap(1,k,ial),iddap(2,k,ial)), &
              pest,rt,mtdata,ibight,bight, &
                 isofilter_t(1,iddat(1,k,ial),iddat(2,k,ial),iddat(3,k,ial)), &
              pesrad,rpred,mraddata,ibighradh,bighradh,bighradv, &
                predrad,icxrad,varpred,npred,userad, &
         lbig2data,lbig2ges,lbig3ges,qsatetaanl,bigeetah,ibigeetah,bigeetav,ibigeetav, &
         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp, &
       nxc,nyc,imeta,jmeta,lmeta,lmetaex,myis2,myie2,myjs2,myje2,myxsc,myxec,myysc,myyec,jpch,jpchus, &
                ids, ide, jds, jde, kds, kde, &                          ! domain indices
                ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                ims, ime, jms, jme, kms, kme, &                          ! memory indices
                inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
   palphabart(:,ial)=pest*dda(1,ial)+rest*dda(2,ial)
   palphabarw(:,ial)=pesw*dda(1,ial)+resw*dda(2,ial)
   palphabarq(:,ial)=pesq*dda(1,ial)+resq*dda(2,ial)
   palphabarpw(:,ial)=pespw*dda(1,ial)+respw*dda(2,ial)
   palphabarps(:,ial)=pesps*dda(1,ial)+resps*dda(2,ial)
   palphabarrad(:,ial)=pesrad*dda(1,ial)+resrad*dda(2,ial)
  end do

  deallocate(resq)
  deallocate(respw)
  deallocate(resw)
  deallocate(resps)
  deallocate(rest)
  deallocate(resrad)
  deallocate(pesq)
  deallocate(pespw)
  deallocate(pesw)
  deallocate(pesps)
  deallocate(pest)
  deallocate(pesrad)

return
end subroutine bigqalphaop_bayes
