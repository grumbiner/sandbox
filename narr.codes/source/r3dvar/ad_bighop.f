subroutine ad_bighop(qeta,pbarq,mqdata,ibighq,bighq, &
                          pbarpw,mpwdata,ibighpw,bighpw, &
                     weta,pbarw,mwdata,ibighw,bighw, &
                          pbarwr,mwrdata,ibighwr,bighwr, &
                     peta,pbarp,mpdata,ibighp,bighp, &
                     teta,pbart,mtdata,ibight,bight, &
                 ppred,pbarrad,mraddata,ibighradh,bighradh,bighradv,icxrad,predrad,npred,userad, &
                     lbig2ges,lbig3ges,imeta,jmeta,lmetaex,jpch,jpchus)

!-------- adjoint of bighop

!  --> pt,etc:       input model variable vector scaled by half of background error
! <--  pbart,etc:    output model variables at obs locations scaled by obs error
!  --> mtdata,etc:   number of observations of each type
!  --> ibight,etc:   linear forward model index arrays
!  --> bight,etc:    linear forward model weights
!  --> lbig2ges,lbig3ges: number of interpolating points for 2- and 3-dim interpolation
!  --> imeta,jmeta,lmetaex:  grid dimensions

  include 'mpif.h'
      include "my_comm.h"

  real(4) qeta(imeta*jmeta,lmetaex)
  real(4) pbarq(max(1,mqdata))
  integer(4) ibighq(lbig3ges,max(1,mqdata))
  real(4) bighq(lbig3ges,max(1,mqdata))

  real(4) pbarpw(max(1,mpwdata))
  integer(4) ibighpw(lbig2ges,max(1,mpwdata))
  real(4) bighpw(lmetaex,lbig2ges,max(1,mpwdata))

  real(4) weta(imeta*jmeta*lmetaex,2)
  real(4) pbarw(max(1,mwdata))
  integer(4) ibighw(lbig3ges,max(1,mwdata))
  real(4) bighw(lbig3ges,2,max(1,mwdata))

  real(4) pbarwr(lmetaex,max(1,mwrdata))
  integer(4) ibighwr(lbig2ges+2,max(1,mwrdata))
  real(4) bighwr(lbig2ges,2,max(1,mwrdata))

  real(4) peta(imeta*jmeta)
  real(4) pbarp(max(1,mpdata))
  integer(4) ibighp(lbig2ges,max(1,mpdata))
  real(4) bighp(lbig2ges,max(1,mpdata))

  real(4) teta(imeta*jmeta,lmetaex)
  real(4) pbart(max(1,mtdata))
  integer(4) ibight(lbig3ges,max(1,mtdata))
  real(4) bight(lbig3ges,max(1,mtdata))

  logical userad
  real(4) ppred(jpch,npred)
  real(4) pbarrad(max(1,mraddata))
  integer(4) ibighradh(lbig2ges,max(1,mraddata))
  real(4) bighradh(lbig2ges,max(1,mraddata))
  real(4) bighradv(2*lmetaex,max(1,mraddata))
  integer(4) icxrad(2,max(1,mraddata))
  real(4) predrad(npred+jpchus-1,max(1,mraddata))

  real(4),allocatable::ppredloc(:,:)

  imjmeta=imeta*jmeta
         kbeamtopmax=-huge(kbeamtopmax)
         kbeambotmax=-huge(kbeambotmax)
         kbeamtopmin=huge(kbeamtopmin)
         kbeambotmin=huge(kbeambotmin)
!------------------------------------------------------------------
!----   apply linear operator H
!------------------------------------------------------------------
!---------------------------------------rad

  qeta=0.
  weta=0.
  peta=0.
  teta=0.
  ppred=0.
  if(userad) then
!--------------------------------------- contribution from bias correction
   allocate(ppredloc(jpch,npred))
   ppredloc=0.
   if(mraddata.gt.0) then
    do i=1,mraddata
     ic=icxrad(1,i)
     icl=npred+icxrad(2,i)-2
     do n=1,npred-2
      ppredloc(ic,n)=ppredloc(ic,n)+predrad(n,i)*pbarrad(i)
     end do
     ppredloc(ic,npred)=ppredloc(ic,npred)+predrad(icl,i)*pbarrad(i)
     ppredloc(ic,npred-1)=ppredloc(ic,npred)+predrad(icl,i)**2*pbarrad(i)
    end do
   end if
   call mpi_allreduce(ppredloc,ppred,jpch*npred,mpi_real,mpi_sum,my_comm,ierr)
   deallocate(ppredloc)
!-------------------------------------- contribution from t
   if(mraddata.gt.0) then
    do i=1,mraddata
     do k=1,lmetaex
      pthis=bighradv(k,i)*pbarrad(i)
      do m=1,lbig2ges
       teta(ibighradh(m,i),k)=teta(ibighradh(m,i),k)+pthis*bighradh(m,i)
      end do
     end do
    end do
   end if
!--------------------------------------- contribution from q
   if(mraddata.gt.0) then
    do i=1,mraddata
     do k=1,lmetaex
      pthis=bighradv(k+lmetaex,i)*pbarrad(i)
      do m=1,lbig2ges
       qeta(ibighradh(m,i),k)=qeta(ibighradh(m,i),k)+pthis*bighradh(m,i)
      end do
     end do
    end do
   end if
  end if
!---------------------------------------t
  if(mtdata.gt.0) then
   do i=1,mtdata
    do m=1,lbig3ges
     teta(ibight(m,i),1)=teta(ibight(m,i),1)+bight(m,i)*pbart(i)
    end do
   end do
  end if
!---------------------------------------p
  if(mpdata.gt.0) then
   do i=1,mpdata
    do m=1,lbig2ges
     peta(ibighp(m,i))=peta(ibighp(m,i))+bighp(m,i)*pbarp(i)
    end do
   end do
  end if
!---------------------------------------wr
  if(mwrdata.gt.0) then
   do k=1,2
    do i=1,mwrdata
     kbeamtop=ibighwr(lbig2ges+1,i)
     kbeambot=ibighwr(lbig2ges+2,i)
         kbeamtopmax=max(kbeamtop,kbeamtopmax)
         kbeambotmax=max(kbeambot,kbeambotmax)
         kbeamtopmin=min(kbeamtop,kbeamtopmin)
         kbeambotmin=min(kbeambot,kbeambotmin)
     do kk=kbeamtop,kbeambot
      do m=1,lbig2ges
       weta(ibighwr(m,i)+(kk-1)*imjmeta,k)=weta(ibighwr(m,i)+(kk-1)*imjmeta,k)+bighwr(m,k,i)*pbarwr(kk,i)
      end do
     end do
    end do
   end do
  end if
!---------------------------------------w
  if(mwdata.gt.0) then
   do k=1,2
    do i=1,mwdata
     do m=1,lbig3ges
      weta(ibighw(m,i),k)=weta(ibighw(m,i),k)+bighw(m,k,i)*pbarw(i)
     end do
    end do
   end do
  end if
!---------------------------------------pw
  if(mpwdata.gt.0) then
   do i=1,mpwdata
    do m=1,lbig2ges
     do k=1,lmetaex
      qeta(ibighpw(m,i),k)=qeta(ibighpw(m,i),k)+bighpw(k,m,i)*pbarpw(i)
     end do
    end do
   end do
  end if
!---------------------------------------q
  if(mqdata.gt.0) then
   do i=1,mqdata
    do m=1,lbig3ges
     qeta(ibighq(m,i),1)=qeta(ibighq(m,i),1)+bighq(m,i)*pbarq(i)
    end do
   end do
  end if

  call ad_exch(peta,1,5,5)
  call exch(peta,1,5,5)
  call ad_exch(teta,lmetaex,5,5)
  call exch(teta,lmetaex,5,5)
  call ad_exch(weta,2*lmetaex,5,5)
  call exch(weta,2*lmetaex,5,5)
  call ad_exch(qeta,lmetaex,5,5)
  call exch(qeta,lmetaex,5,5)

  call mpi_comm_rank(my_comm,mype,ierr)
  call mpi_reduce(kbeambotmin,kbeambotmin0,1,mpi_integer4,mpi_min,0,my_comm,ierr)
  call mpi_reduce(kbeambotmax,kbeambotmax0,1,mpi_integer4,mpi_max,0,my_comm,ierr)
  call mpi_reduce(kbeamtopmin,kbeamtopmin0,1,mpi_integer4,mpi_min,0,my_comm,ierr)
  call mpi_reduce(kbeamtopmax,kbeamtopmax0,1,mpi_integer4,mpi_max,0,my_comm,ierr)
  if(mype.eq.0) print *,' in ad_bighop, kbeambotmin,max,topmin,max=', &
                kbeambotmin0,kbeambotmax0,kbeamtopmin0,kbeamtopmax0
return
end subroutine ad_bighop
