subroutine get_coarse_background(tetages,uetages,vetages,qetages,qsatetages,petages, &
                 tg,ug,vg,qg,pg,ksfc,lmh,etam,ptop,nxc,nyc, &
                 bighetah,bighetav,ibighetah,ibighetav,lbig2ges,imeta,jmeta,lmeta, &
                 iter_restore,iter_smooth, &
                 ids, ide, jds, jde, kds, kde, &         ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                 ims, ime, jms, jme, kms, kme, &                     ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  IMPLICIT NONE
  include 'mpif.h'
      include "my_comm.h"

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)


  integer(4) imeta,jmeta,lmeta,nxc,nyc,iter_restore,iter_smooth

  real(4) tetages(imeta*jmeta,lmeta)    ! background virtual temperature
  real(4) uetages(imeta*jmeta,lmeta)    ! background u
  real(4) vetages(imeta*jmeta,lmeta)    ! background v
  real(4) qetages(imeta*jmeta,lmeta)    ! background q
  real(4) qsatetages(imeta*jmeta,lmeta) ! badkground qsat
  real(4) petages(imeta*jmeta)          ! background pressure variable
  integer(4) lmh(imeta*jmeta)           ! vertical index of first eta level above surface
  real(4)   etam(lmeta)                 ! eta coordinate
  real(4) ptop                          ! top pressure

  real(4) tg(ims:ime,jms:jme,kms:kme),ug(ims:ime,jms:jme,kms:kme)   ! smoothed output fields on
  real(4) vg(ims:ime,jms:jme,kms:kme),qg(ims:ime,jms:jme,kms:kme)   !  coarse analysis grid
  real(4) pg(ims:ime,jms:jme)
  integer(4) ksfc(ips:ipe,jps:jpe)


!            interpolation constants from coarse analysis grid to eta grid

  real(4) bighetah(nxc*nyc,lbig2ges),bighetav(nxc*nyc,lbig2ges)
  integer(4) ibighetah(nxc*nyc,lbig2ges),ibighetav(nxc*nyc,lbig2ges)
  integer(4) lbig2ges

  real(4),allocatable::rnormh(:,:),rnormv(:,:),worketa(:,:)
  real(4),allocatable::rmh(:),rksfc(:,:)

  integer(4) i,j,kk,k
  integer(4) ierr
  integer(4) numhbad,numvbad
  real(4) rkappa,this_pres,conmc
  real(4) rnormhmax,rnormhmin,rnormvmax,rnormvmin
  real(4) rnormhmaxall,rnormhminall,rnormvmaxall,rnormvminall

             if(ime-ims+1.ne.nxc.or.jme-jms+1.ne.nyc) write(0,*)' mype,ims,ime,nxc,jms,jme,nyc=', &
                                            mype,ims,ime,nxc,jms,jme,nyc
       if(mype.eq.0) write(0,*)' at 1 in get_coarse_background,iter_restore,iter_smooth=', &
                             iter_restore,iter_smooth

!     get t,u,v,peta on coarse analysis grid


  allocate(rksfc(ims:ime,jms:jme))
  allocate(rnormh(ims:ime,jms:jme))
  allocate(rnormv(ims:ime,jms:jme))
  tg=0.
  ug=0.
  vg=0.
  pg=0.
  qg=0.
  rksfc=0.
  rnormh=0.
  rnormv=0.
  allocate(rmh(imeta*jmeta))
  rmh=1.
  call bighetahop(rnormh,rmh,imeta,jmeta,1,lbig2ges,nxc,nyc,bighetah,ibighetah)
  rksfc=rnormh
  call smooth_restore(rksfc,1,nxc,nyc,rnormh,iter_restore,iter_smooth)
             if(mype.eq.0) then
      do j=jms,jps+7
       print *,' rnormhs(.,',j,')=',rksfc(ims:ips+7,j)
      end do
             end if
  rksfc=0.
        numhbad=0
        do j=jms,jme
         do i=ims,ime
          if(rnormh(i,j).ne.0..and.rnormh(i,j).lt..99999) numhbad=numhbad+1
         end do
        end do
        rnormhmax=maxval(rnormh,rnormh.ne.0.)
        rnormhmin=minval(rnormh,rnormh.ne.0.)
  call bighetahop(rnormv,rmh,imeta,jmeta,1,lbig2ges,nxc,nyc,bighetav,ibighetav)
        numvbad=0
        do j=jms,jme
         do i=ims,ime
          if(rnormv(i,j).ne.0..and.rnormv(i,j).lt..99999) numvbad=numvbad+1
         end do
        end do
        print *,' mype,numhbad,numvbad=',mype,numhbad,numvbad
        rnormvmax=maxval(rnormv,rnormv.ne.0.)
        rnormvmin=minval(rnormv,rnormv.ne.0.)
        call mpi_reduce(rnormhmax,rnormhmaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
        call mpi_reduce(rnormhmin,rnormhminall,1,mpi_real4,mpi_min,0,my_comm,ierr)
        call mpi_reduce(rnormvmax,rnormvmaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
        call mpi_reduce(rnormvmin,rnormvminall,1,mpi_real4,mpi_min,0,my_comm,ierr)
             if(mype.eq.0) then
               print *,' ims,ips=',ims,ips
               print *,' jms,jps=',jms,jps
               print *,' rnormhmax,min=',rnormhmaxall,rnormhminall
               print *,' rnormvmax,min=',rnormvmaxall,rnormvminall
      do j=jms,jps+7
       print *,' rnormh(.,',j,')=',rnormh(ims:ips+7,j)
      end do
      do j=jms,jps+7
       print *,' rnormv(.,',j,')=',rnormv(ims:ips+7,j)
      end do
             end if
        if(mype.eq.0) write(0,*)' at 3 in get_coarse_background'
  call bighetahop(tg,tetages,imeta,jmeta,lmeta,lbig2ges,nxc,nyc,bighetah,ibighetah)
  call smooth_restore(tg,lmeta,nxc,nyc,rnormh,iter_restore,iter_smooth)
             if(mype.eq.0) then
      do j=jms,jps+7
       print *,' tg(.,',j,',1)=',tg(ims:ips+7,j,1)
      end do
             end if
  call bighetahop(ug,uetages,imeta,jmeta,lmeta,lbig2ges,nxc,nyc,bighetav,ibighetav)
  call smooth_restore(ug,lmeta,nxc,nyc,rnormv,iter_restore,iter_smooth)
             if(mype.eq.0) then
      do j=jms,jps+7
       print *,' ug(.,',j,',1)=',ug(ims:ips+7,j,1)
      end do
             end if
  call bighetahop(vg,vetages,imeta,jmeta,lmeta,lbig2ges,nxc,nyc,bighetav,ibighetav)
  call smooth_restore(vg,lmeta,nxc,nyc,rnormv,iter_restore,iter_smooth)
             if(mype.eq.0) then
      do j=jms,jps+7
       print *,' vg(.,',j,',1)=',vg(ims:ips+7,j,1)
      end do
             end if
  allocate(worketa(imeta*jmeta,lmeta))
  worketa=0.
  do k=1,lmeta
   do i=1,imeta*jmeta
    if(qsatetages(i,k).gt.1.e-20.and.qsatetages(i,k).lt.1.e20) &
     worketa(i,k)=100.*qetages(i,k)/qsatetages(i,k)
   end do
  end do
  call bighetahop(qg,worketa,imeta,jmeta,lmeta,lbig2ges,nxc,nyc,bighetah,ibighetah)
  deallocate(worketa)
  call smooth_restore(qg,lmeta,nxc,nyc,rnormh,iter_restore,iter_smooth)
             if(mype.eq.0) then
      do j=jms,jps+7
       print *,' qg(.,',j,',1)=',qg(ims:ips+7,j,1)
      end do
             end if
  call bighetahop(pg,petages,imeta,jmeta,1,lbig2ges,nxc,nyc,bighetah,ibighetah)
  call smooth_restore(pg,1,nxc,nyc,rnormh,iter_restore,iter_smooth)
             if(mype.eq.0) then
      do j=jms,jps+7
       print *,' pg(.,',j,')=',pg(ims:ips+7,j)
      end do
             end if
  rmh=lmh
  call bighetahop(rksfc,rmh,imeta,jmeta,1,lbig2ges,nxc,nyc,bighetah,ibighetah)
  call smooth_restore(rksfc,1,nxc,nyc,rnormh,iter_restore,iter_smooth)
             if(mype.eq.0) then
      do j=jms,jps+7
       print *,' rksfc(.,',j,')=',rksfc(ims:ips+7,j)
      end do
             end if
  deallocate(rmh)
  deallocate(rnormh)
  deallocate(rnormv)
        if(mype.eq.0) write(0,*)' at 4 in get_coarse_background'

  do j=jps,jpe
   do i=ips,ipe
    ksfc(i,j)=nint(rksfc(i,j))
    ksfc(i,j)=max(kps,min(kpe,ksfc(i,j)))
   end do
  end do
  deallocate(rksfc)
        if(mype.eq.0) write(0,*)' at 5 in get_coarse_background'

!--------- convert T to Theta

  rkappa=conmc('rd$')/conmc('cp$')
            print *,' mype,lmeta,kps,kpe=',mype,lmeta,kps,kpe
  do j=jps,jpe
   do i=ips,ipe
    k=kps-1
    do kk=1,lmeta
     k=k+1
     this_pres=etam(kk)*pg(i,j)+ptop
     tg(i,j,k)=tg(i,j,k)/(.001*this_pres)**rkappa
           if(tg(i,j,k).lt.10.) then
              print *,' mype,i,j,k,1/rkappa,this_pres,tg=',mype,i,j,k,rkappa,this_pres,tg(i,j,k)
                  if(mype.gt.-1000) then
                       call mpi_finalize(ierr)
                        stop
                  end if
           end if
    end do
   end do
  end do
        if(mype.eq.0) write(0,*)' at 6 in get_coarse_background'

return
end subroutine get_coarse_background
