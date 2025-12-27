subroutine diagnostic_bvar(eyot0,eyoq0,eyop0,eyorad0,eyow0,ivart,ivarq,ivarp,ivarrad,ivaru,ivarv, &
              varlats,varpres,nvarlats,nvarpres, &
              xbarq,rq,mqdata,ibighq,bighq,isofilter_q, &
              xbarpw,mpwdata,ibighpw,bighpw, &
              xbarw,rw,mwdata,ibighw,bighw,isofilter_psi,isofilter_chi, &
              xbarps,rp,mpdata,ibighp,bighp,isofilter_p, &
              xbart,xbarbt,yot,rt,mtdata,ibight,bight,isofilter_t, &
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

  real(4) varlats(nvarlats),varpres(nvarpres)

!    moisture stuff:

  real(4) xbarq(max(1,mqdata))                             ! input analysis inc (obs units)
  real(4) eyoq0(max(1,mqdata))                             ! obs errors
  real(4) rq(nxc,nyc,lmetaex)                              ! output gradient
  integer(4) ibighq(lbig3ges,max(1,mqdata))                ! H operator
  real(4) bighq(lbig3ges,max(1,mqdata))                    !   (eta to obs)
  type(filter_cons) isofilter_q(14)                        ! coarse background error Ctilde
  integer(4) ivarq(nvarlats,nvarpres)

  real(4),allocatable::resq(:)                             ! intermediate non-linear residuals
  real(4),allocatable::qetadir(:,:,:)                      !  intermediate descent direction

!    pw stuff:

  real(4) xbarpw(max(1,mpwdata))                           ! input analysis inc (obs units)
  integer(4) ibighpw(lbig2ges,max(1,mpwdata))              ! H operator
  real(4) bighpw(lmetaex,lbig2ges,max(1,mpwdata))          !   (eta to obs)

  real(4),allocatable::respw(:)                            ! intermediate non-linear residuals

!    wind stuff:

  real(4) xbarw(max(1,mwdata))                                ! input analysis inc (obs units)
  real(4) eyow0(max(1,mwdata))                             ! obs errors
  real(4) rw(nxc,nyc,lmetaex,2)                               ! output gradient      (1=psi,u, 2=chi,v)
  integer(4) ibighw(lbig3ges,max(1,mwdata))                   ! H operator
  real(4) bighw(lbig3ges,2,max(1,mwdata))                     !   (eta to obs)
  type(filter_cons) isofilter_psi(14),isofilter_chi(14)       ! coarse background error Ctilde
  integer(4) ivaru(nvarlats,nvarpres)
  integer(4) ivarv(nvarlats,nvarpres)

  real(4),allocatable::resw(:)                                ! intermediate non-linear residuals
  real(4),allocatable::wetadir(:,:,:,:)                       !  intermediate descent direction

!    h stuff:

  real(4) xbarps(max(1,mpdata))                   ! input analysis inc (obs units)
  real(4) eyop0(max(1,mpdata))                             ! obs errors
  real(4) rp(nxc,nyc)                                      ! output gradient
  integer(4) ibighp(lbig2ges,max(1,mpdata))                ! H operator
  real(4) bighp(lbig2ges,max(1,mpdata))                    !   (eta to obs)
  type(filter_cons) isofilter_p(14)                        ! coarse background error Ctilde
  integer(4) ivarp(nvarlats)

  real(4),allocatable::resps(:)                            ! intermediate non-linear residuals
  real(4),allocatable::petadir(:,:)                        !  intermediate descent direction

!    t stuff:

  real(4) xbart(max(1,mtdata))                             ! input analysis inc (obs units)
  real(4) eyot0(max(1,mtdata))                             ! obs errors
  real(4) rt(nxc,nyc,lmetaex)                              ! output gradient
  integer(4) ibight(lbig3ges,max(1,mtdata))                ! H operator
  real(4) bight(lbig3ges,max(1,mtdata))                    !   (eta to obs)
  type(filter_cons) isofilter_t(14)                        ! coarse background error Ctilde
  integer(4) ivart(nvarlats,nvarpres)
  real(4) xbarbt(max(1,mtdata))                            ! input background values
  real(4) yot(max(1,mtdata))                               ! input obs values

  real(4),allocatable::rest(:)                             ! intermediate non-linear residuals
  real(4),allocatable::tetadir(:,:,:)                      !  intermediate descent direction

!    rad stuff:

  real(4) xbarrad(max(1,mraddata))
  real(4) eyorad0(max(1,mraddata))                             ! obs errors
  real(4) rpred(jpch,npred)
  integer(4) ibighradh(lbig2ges,max(1,mraddata))
  real(4) bighradh(lbig2ges,max(1,mraddata))
  real(4) bighradv(2*lmetaex,max(1,mraddata))
  real(4) predrad(npred+jpchus-1,max(1,mraddata))
  integer(4) icxrad(2,max(1,mraddata))
  real(4) varpred(jpch,npred)
  logical userad
  integer(4) ivarrad(nvarlats,jpch)

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

!      t var

  do k=1,nvarpres
   do j=1,nvarlats
    xbarq=0.
    xbarpw=0.
    xbarw=0.
    xbarps=0.
    xbart=0.
    xbarrad=0.
    if(ivart(j,k).gt.0) xbart(ivart(j,k))=1.
                if(mype.eq.0) write(0,*)' before bigqop_bayes'
    call bigqop_bayes( &
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
           if(mype.eq.0) write(0,*)' after bigqop_bayes'
    if(ivart(j,k).gt.0) then
     xbart(ivart(j,k))=xbart(ivart(j,k))-1.
     bvar=xbart(ivart(j,k))*eyot0(ivart(j,k))**2
     bsig=sqrt(bvar)
     print *,' lat,pres,bvart,bsigt,osigt=',varlats(j),varpres(k),bvar,bsig,eyot0(ivart(j,k))
    end if
   end do
  end do

!      q var

  do k=1,nvarpres
   do j=1,nvarlats
    xbarq=0.
    xbarpw=0.
    xbarw=0.
    xbarps=0.
    xbart=0.
    xbarrad=0.
    if(ivarq(j,k).gt.0) xbarq(ivarq(j,k))=1.
                if(mype.eq.0) write(0,*)' before bigqop_bayes'
    call bigqop_bayes( &
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
           if(mype.eq.0) write(0,*)' after bigqop_bayes'
    if(ivarq(j,k).gt.0) then
     xbarq(ivarq(j,k))=xbarq(ivarq(j,k))-1.
     bvar=xbarq(ivarq(j,k))*eyoq0(ivarq(j,k))**2
     bsig=sqrt(bvar)
     print *,' lat,pres,bvarq,bsigq,osigq=',varlats(j),varpres(k),bvar,bsig,eyoq0(ivarq(j,k))
    end if
   end do
  end do

!      p var

  do j=1,nvarlats
   xbarq=0.
   xbarpw=0.
   xbarw=0.
   xbarps=0.
   xbart=0.
   xbarrad=0.
   if(ivarp(j).gt.0) xbarps(ivarp(j))=1.
               if(mype.eq.0) write(0,*)' before bigqop_bayes'
   call bigqop_bayes( &
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
           if(mype.eq.0) write(0,*)' after bigqop_bayes'
   if(ivarp(j).gt.0) then
    xbarps(ivarp(j))=xbarps(ivarp(j))-1.
    bvar=xbarps(ivarp(j))*eyop0(ivarp(j))**2
    bsig=sqrt(bvar)
    print *,' lat,bvarp,bsigp,osigp=',varlats(j),bvar,bsig,eyop0(ivarp(j))
   end if
  end do

!      rad var

  do k=1,jpch
   do j=1,nvarlats
    xbarq=0.
    xbarpw=0.
    xbarw=0.
    xbarps=0.
    xbart=0.
    xbarrad=0.
    if(ivarrad(j,k).gt.0) then
     xbarrad(ivarrad(j,k))=1.
    end if
    call mpi_allreduce(ivarrad(j,k),ivarradmax,1,mpi_integer,mpi_max,my_comm,ierr)
                if(mype.eq.0) write(0,*)' before bigqop_bayes'
    if(ivarradmax.gt.0) &
      call bigqop_bayes( &
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
           if(mype.eq.0) write(0,*)' after bigqop_bayes'
    if(ivarrad(j,k).gt.0) then
     xbarrad(ivarrad(j,k))=xbarrad(ivarrad(j,k))-1.
     bvar=xbarrad(ivarrad(j,k))*eyorad0(ivarrad(j,k))**2
     bsig=sqrt(bvar)
     print *,' lat,chan,bvarrad,bsigrad,osigrad=',varlats(j),k,bvar,bsig,eyorad0(ivarrad(j,k))
    end if
   end do
  end do

!      u var

  do k=1,nvarpres
   do j=1,nvarlats
    xbarq=0.
    xbarpw=0.
    xbarw=0.
    xbarps=0.
    xbart=0.
    xbarrad=0.
    if(ivaru(j,k).gt.0) xbarw(ivaru(j,k))=1.
    call bigqop_bayes( &
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
    if(ivaru(j,k).gt.0) then
     xbarw(ivaru(j,k))=xbarw(ivaru(j,k))-1.
     bvar=xbarw(ivaru(j,k))*eyow0(ivaru(j,k))**2
     bsig=sqrt(bvar)
     print *,' lat,pres,bvaru,bsigu,osigu=',varlats(j),varpres(k),bvar,bsig,eyow0(ivaru(j,k))
    end if
   end do
  end do

!      v var

  do k=1,nvarpres
   do j=1,nvarlats
    xbarq=0.
    xbarpw=0.
    xbarw=0.
    xbarps=0.
    xbart=0.
    xbarrad=0.
    if(ivarv(j,k).gt.0) xbarw(ivarv(j,k))=1.
    call bigqop_bayes( &
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
    if(ivarv(j,k).gt.0) then
     xbarw(ivarv(j,k))=xbarw(ivarv(j,k))-1.
     bvar=xbarw(ivarv(j,k))*eyow0(ivarv(j,k))**2
     bsig=sqrt(bvar)
     print *,' lat,pres,bvarv,bsigv,osigv=',varlats(j),varpres(k),bvar,bsig,eyow0(ivarv(j,k))
    end if
   end do
  end do
     if(mype.gt.-1000) then
       call mpi_finalize(ierr)
       stop
     end if
     
return
end subroutine diagnostic_bvar
