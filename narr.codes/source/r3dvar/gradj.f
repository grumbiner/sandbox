subroutine gradj( &
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

!-------- obtain gradient of objective function J

!              2    o    _b  _  2
!      2J = |x| + |y - R(x + x)|

!                        T T  o    _b  _
!     r = grad(J) = x - C K [y - R(x + x)]

!  note:  for now, R and K are identity operators.  Later, as non-linear forward models
!                are introduced, R and K will evolve accordingly


  include 'mpif.h'
      include "my_comm.h"
  include 'filtertype.h'
  include 'qcparam.h'

  INTEGER(4), INTENT(IN) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  logical doqc

!    moisture stuff:

  real(4) xq(nxc,nyc,lmetaex)                              ! input analysis inc
  real(4) xbarq(max(1,mqdata))                             ! input analysis inc (obs units)
  real(4) rq(nxc,nyc,lmetaex)                              ! output gradient
  real(4) yoq(max(1,mqdata))                               ! observations
  real(4) xbarbq(max(1,mqdata))                            ! guess (obs units)
  integer(4) ibighq(lbig3ges,max(1,mqdata))                ! H operator
  real(4) bighq(lbig3ges,max(1,mqdata))                    !   (eta to obs)
  type(filter_cons) isofilter_q(14)                        ! coarse background error Ctilde
  logical qfw(max(1,mqdata))                               ! qc related keep flags

  real(4),allocatable::resq(:),rqeta(:)                    ! intermediate non-linear residuals

!    pw stuff:

  real(4) xbarpw(max(1,mpwdata))                           ! input analysis inc (obs units)
  real(4) yopw(max(1,mpwdata))                             ! observations
  real(4) xbarbpw(max(1,mpwdata))                          ! guess (obs units)
  integer(4) ibighpw(lbig2ges,max(1,mpwdata))              ! H operator
  real(4) bighpw(lmetaex,lbig2ges,max(1,mpwdata))          !   (eta to obs)
  logical pwfw(max(1,mpwdata))                             ! qc related keep flags

  real(4),allocatable::respw(:)                            ! intermediate non-linear residuals

!    wind stuff:

  real(4) xw(nxc,nyc,lmetaex,2)                               ! input analysis inc   (1=psi,u, 2=chi,v)
  real(4) xbarw(max(1,mwdata))                                ! input analysis inc (obs units)
  real(4) rw(nxc,nyc,lmetaex,2)                               ! output gradient      (1=psi,u, 2=chi,v)
  real(4) yow(max(1,mwdata))                                  ! observations
  real(4) xbarbw(max(1,mwdata))                               ! guess (obs units)
  integer(4) ibighw(lbig3ges,max(1,mwdata))                   ! H operator
  real(4) bighw(lbig3ges,2,max(1,mwdata))                     !   (eta to obs)
  type(filter_cons) isofilter_w(14,2)                         ! coarse background error Ctilde (1=psi,2=chi)
  logical wfw(max(1,mqdata))                                  ! qc related keep flags

  real(4),allocatable::resw(:),rweta(:)                       ! intermediate non-linear residuals

!    radar wind stuff:

  real(4) xbarwr(lmetaex,max(1,mwrdata))                      ! input analysis inc (obs units)
  real(4) yowr(max(1,mwrdata))                                ! observations
  real(4) xbarbwr(lmetaex,max(1,mwrdata))                     ! guess (obs units)
  integer(4) ibighwr(lbig2ges+2,max(1,mwrdata))               ! H operator
  real(4) bighwr(lbig2ges,2,max(1,mwrdata))                   !   (eta to obs)

  real(4),allocatable::reswr(:,:)                             ! intermediate non-linear residuals

!    h stuff:

  real(4) xp(nxc,nyc)                                      ! input analysis inc
  real(4) xbarp(max(1,mpdata))                             ! input analysis inc (obs units)
  real(4) rp(nxc,nyc)                                      ! output gradient
  real(4) yop(max(1,mpdata))                               ! observations
  real(4) xbarbp(max(1,mpdata))                            ! guess (obs units)
  integer(4) ibighp(lbig2ges,max(1,mpdata))                ! H operator
  real(4) bighp(lbig2ges,max(1,mpdata))                    !   (eta to obs)
  type(filter_cons) isofilter_p(14)                        ! coarse background error Ctilde
  logical psfw(max(1,mpdata))                              ! qc related keep flags

  real(4),allocatable::resp(:),rpeta(:)                    ! intermediate non-linear residuals

!    t stuff:

  real(4) xt(nxc,nyc,lmetaex)                              ! input analysis inc
  real(4) xbart(max(1,mtdata))                             ! input analysis inc (obs units)
  real(4) rt(nxc,nyc,lmetaex)                              ! output gradient
  real(4) yot(max(1,mtdata))                               ! observations
  real(4) xbarbt(max(1,mtdata))                            ! guess (obs units)
  integer(4) ibight(lbig3ges,max(1,mtdata))                ! H operator
  real(4) bight(lbig3ges,max(1,mtdata))                    !   (eta to obs)
  type(filter_cons) isofilter_t(14)                        ! coarse background error Ctilde
  logical tfw(max(1,mtdata))                               ! qc related keep flags

  real(4),allocatable::rest(:),rteta(:)                    ! intermediate non-linear residuals

!    rad stuff:

  real(4) xpred(jpch,npred)
  real(4) xbarrad(max(1,mraddata))
  real(4) rpred(jpch,npred)
  real(4) yorad(max(1,mraddata))
  real(4) xbarbrad(max(1,mraddata))
  integer(4) ibighradh(lbig2ges,max(1,mraddata))
  real(4) bighradh(lbig2ges,max(1,mraddata))
  real(4) bighradv(2*lmetaex,max(1,mraddata))
  real(4) predrad(npred+jpchus-1,max(1,mraddata))
  integer(4) icxrad(2,max(1,mraddata))
  real(4) varpred(jpch,npred)
  logical userad
  logical radfw(max(1,mraddata))                               ! qc related keep flags

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
                  real(8) prod8

!  first apply R to xbarb+xbar
        if(mype.eq.0) write(0,*)' at 1 in gradj'

  allocate(resq(max(1,mqdata))) ; allocate(rqeta(imeta*jmeta*lmetaex))
  allocate(respw(max(1,mpwdata)))
  allocate(resw(max(1,mwdata))) ; allocate(rweta(imeta*jmeta*lmetaex*2))
  allocate(reswr(lmetaex,max(1,mwrdata)))
  allocate(resp(max(1,mpdata))) ; allocate(rpeta(imeta*jmeta))
  allocate(rest(max(1,mtdata))) ; allocate(rteta(imeta*jmeta*lmetaex))
  allocate(resrad(max(1,mraddata)))

!   R is identity for moment

      cg = 1.2533/b          ! cg = sqrt(2*pi)/2b

!  Observation contribution

  call gradnl(resq,pgq,cg,yoq,xbarbq,xbarq,mqdata,qfw,doqc)
  call gradnl(respw,pgpw,cg,yopw,xbarbpw,xbarpw,mpwdata,pwfw,doqc)
  call gradnl(resw,pgw,cg,yow,xbarbw,xbarw,mwdata,wfw,doqc)
         reswmax=-huge(reswmax)
         reswmin=huge(reswmin)
         if(mwdata.gt.0) then
          do i=1,mwdata
           reswmax=max(resw(i),reswmax)
           reswmin=min(resw(i),reswmin)
          end do
         end if
         call mpi_reduce(reswmax,reswmax0,1,mpi_real4,mpi_max,0,my_comm,ierr)
         call mpi_reduce(reswmin,reswmin0,1,mpi_real4,mpi_min,0,my_comm,ierr)
         if(mype.eq.0) print *,' reswmin,max=',reswmin0,reswmax0
      call dot_prod8(prod8,resw,resw,mwdata)
          if(mype.eq.0) print *,' in gradj, resw*resw=',prod8
  call gradnl_radar(reswr,yowr,xbarbwr,xbarwr,mwrdata,lmetaex,ibighwr,lbig2ges)
         reswmax=-huge(reswmax)
         reswmin=huge(reswmin)
         if(mwrdata.gt.0) then
          do i=1,mwrdata
           do k=1,lmetaex
            reswmax=max(reswr(k,i),reswmax)
            reswmin=min(reswr(k,i),reswmin)
           end do
          end do
         end if
         call mpi_reduce(reswmax,reswmax0,1,mpi_real4,mpi_max,0,my_comm,ierr)
         call mpi_reduce(reswmin,reswmin0,1,mpi_real4,mpi_min,0,my_comm,ierr)
         if(mype.eq.0) print *,' reswrmin,max=',reswmin0,reswmax0
      call dot_prod8(prod8,reswr,reswr,lmetaex*mwrdata)
          if(mype.eq.0) print *,' in gradj, reswr*reswr=',prod8
        if(mype.eq.0) write(0,*)' at 2 in gradj'
  call gradnl(resp,pgp,cg,yop,xbarbp,xbarp,mpdata,psfw,doqc)
  call gradnl(rest,pgt,cg,yot,xbarbt,xbart,mtdata,tfw,doqc)
  resrad=0.
  if(userad) call gradnl(resrad,pgrad,cg,yorad,xbarbrad,xbarrad,mraddata,radfw,doqc)

!              T T    T
!   now apply H Eeta C

        if(mype.eq.0) write(0,*)' at 3 in gradj'
  call ad_bighop(rqeta,resq,mqdata,ibighq,bighq, &
                       respw,mpwdata,ibighpw,bighpw, &
                 rweta,resw,mwdata,ibighw,bighw, &
                       reswr,mwrdata,ibighwr,bighwr, &
                 rpeta,resp,mpdata,ibighp,bighp, &
                 rteta,rest,mtdata,ibight,bight, &
                 rpred,resrad,mraddata,ibighradh,bighradh,bighradv,icxrad,predrad,npred,userad, &
                 lbig2ges,lbig3ges,imeta,jmeta,lmetaex,jpch,jpchus)

  deallocate(resq) ; deallocate(resw) ; deallocate(reswr)
  deallocate(resp) ; deallocate(rest) ; deallocate(resrad)

  call ad_bigeetahopq(rq,rqeta,imeta,jmeta,lmetaex,lbig2data, &
                     nxc,nyc,myis2,myie2,myjs2,myje2,qsatetaanl,bigeetah,ibigeetah)
  call ad_bigeetavop(rw,rweta,imeta,jmeta,lmetaex,lbig2data, &
                     nxc,nyc,myis2,myie2,myjs2,myje2,bigeetav,ibigeetav)
        if(mype.eq.0) write(0,*)' at 4 in gradj'
  call ad_bigeetahop(rt,rteta,imeta,jmeta,lmetaex,lbig2data, &
                     nxc,nyc,myis2,myie2,myjs2,myje2,bigeetah,ibigeetah)
  call ad_bigeetahop(rp,rpeta,imeta,jmeta,1,lbig2data, &
                     nxc,nyc,myis2,myie2,myjs2,myje2,bigeetah,ibigeetah)

  deallocate(rqeta) ; deallocate(rweta) ; deallocate(rpeta) ; deallocate(rteta)

  call ad_slow_plus_fast(rt,rp,rw(1,1,1,1), &
                         lat1,lat2,bight_psi,bighp_psi,wgts_bightp,iwgts_bightp,nxc,nyc,lmetaex)

  call regular_raf(rq,isofilter_q)
  call regular_raf(rw(1,1,1,1),isofilter_w(1,1))
  call regular_raf(rw(1,1,1,2),isofilter_w(1,2))
  call regular_raf(rt,isofilter_t)
        if(mype.eq.0) write(0,*)' at 5 in gradj'
  call regular_raf(rp,isofilter_p)
  if(userad) rpred=varpred*rpred


! finally subtract from x

  rq=xq-rq ; rw=xw-rw ; rp=xp-rp ; rt=xt-rt
  if(userad) rpred=xpred-rpred

return
end subroutine gradj
