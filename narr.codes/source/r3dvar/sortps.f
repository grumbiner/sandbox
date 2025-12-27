subroutine sortps(iordges,lbig2ges,lhalfges, &
         pdres01,zsfcges,tsfcges,res,tlapseges,imeta,jmeta, &
         ptop,delhour,grossp,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb,iuniterr,npes)

!-------- evaluate guess at each obs point. then write out obs to temp
!-------- storage.

  include 'mpif.h'
      include "my_comm.h"
  include 'writges.h'

!-------- external arrays

  real(4) pdres01(imeta*jmeta),zsfcges(imeta*jmeta),tsfcges(imeta*jmeta),res(imeta*jmeta)
  real(4) tlapseges(imeta*jmeta)

!--------  internal work space

  real(4),allocatable::wgts2(:,:)
  integer(4),allocatable::iwgts2(:,:)
  real(4),allocatable::perr(:),plon(:),plat(:),plong(:),platg(:)
  real(4),allocatable::pobs(:),pelev(:),zqm(:),ptobs(:),pobs0(:)
  real(4),allocatable::ptime(:),ptype(:)
  integer(4),allocatable::icode(:)
  integer(8),allocatable::iplabel(:)
  character(8),allocatable::pstaid(:)
  character(8),allocatable::pestaid(:)
  real(4),allocatable::peobs(:),petime(:),petype(:),pepres(:)
  real(4),allocatable::pges(:),pges0(:),petaobs(:)
  real(4),allocatable::hsfc(:)
  real(4),allocatable::etaobssfc(:)
  real(4),allocatable::bighhh(:,:)
  integer(4),allocatable::ibighhh(:,:)
  integer(4),allocatable::ibadtype(:),ibadtypeall(:)
  integer(4),allocatable::ngross_table(:),ngross_disp(:)
  character(90),allocatable::jwrite(:),jwrite0(:)
  real(8) ptrms8,ptbar8,count8
  real(8) ptrmsall8,ptbarall8,countall8
  character*16 filename,filenameg
  real(4) xmsg
  character(10)eventfile

  xmsg=9999.
  call mpi_comm_rank(my_comm,mype,ierr)

!-------- open dataset to hold diagnostic statistics output

  iwrite=150
  if(mype.eq.0) open(iwrite,file='fith',form='formatted')

!-------- bring in all sfcps from common

  call count_sfcps(npdata)
  call mpi_allreduce(npdata,npdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
  if(npdataall.le.0) return     !  no data to process

!        if(mype.eq.0) write(0,*)' at 1 in sortps, npdata=',npdataall
  allocate(ibadtype(500))
  allocate(ibadtypeall(500))
    ibadtype=0
  allocate(perr(max(1,npdata))) ; allocate(plong(max(1,npdata)))
  allocate(platg(max(1,npdata)))
  allocate(plon(max(1,npdata))) ; allocate(plat(max(1,npdata)))
  allocate(pobs(max(1,npdata))) ; allocate(pelev(max(1,npdata))) ; allocate(zqm(max(1,npdata)))
  allocate(petaobs(max(1,npdata))); allocate(pobs0(max(1,npdata)))
  allocate(hsfc(max(1,npdata)))
  allocate(etaobssfc(max(1,npdata)))
  allocate(bighhh(lbig2ges,max(1,npdata)))
  allocate(ibighhh(lbig2ges,max(1,npdata)))
  allocate(pges(max(1,npdata)))
  allocate(pges0(max(1,npdata)))
  allocate(ptobs(max(1,npdata)))
  allocate(ptime(max(1,npdata))) ; allocate(ptype(max(1,npdata)))
  allocate(pstaid(max(1,npdata)))
  allocate(peobs(max(1,npdata))); allocate(pestaid(max(1,npdata)))
  allocate(petime(max(1,npdata))); allocate(petype(max(1,npdata)))
  allocate(pepres(max(1,npdata)))
  allocate(iplabel(max(1,npdata)))
  allocate(icode(max(1,npdata)))
!        if(mype.eq.0) write(0,*)' at 2 in sortps, mype,npdata=',mype,npdataall
  call rdsfcps(perr,plon,plat,plong,platg,pobs,pelev,pges, &
               hsfc,etaobssfc,bighhh,ibighhh, &
               ptobs,ptime,zqm,ptype,pstaid,iplabel,npdata,lbig2ges)
  ptype = nint(ptype)
  pobs0 = exp(pobs)
!        if(mype.eq.0) write(0,*)' at 3 in sortps, mype,npdata=',mype,npdataall

!-------- evaluate guess height at obs pressure
!-------- do conversion from elevation to pressure for sfc obs

  istaghglb=0
  allocate(wgts2(max(1,npdata),lbig2ges))
  allocate(iwgts2(max(1,npdata),lbig2ges))
!        if(mype.eq.0) write(0,*)' at 4 in sortps, mype,npdata=',mype,npdataall
  call getpges(pges,petaobs,plong,platg,pobs,ptobs,pelev,pstaid,npdata, &
       iordges,lbig2ges,lhalfges,wgts2,iwgts2,imeta,jmeta,pdres01,zsfcges,tsfcges,res, &
       tlapseges,ptop,perr,wbglb,dlon0,sbglb,dlat0,istaghglb,imetaglb,jmetaglb,mype,icode)
  pobs=petaobs
  pges0=pges
!        if(mype.eq.0) write(0,*)' at 5 in sortps, mype,npdata=',mype,npdataall
  if(npdata.gt.0) then
   do k=1,lbig2ges
    do i=1,npdata
     bighhh(k,i)=wgts2(i,k)
     ibighhh(k,i)=iwgts2(i,k)
    end do
   end do
  end if
  deallocate(wgts2)
  deallocate(iwgts2)

!-------- make sure all points outside domain are tagged
!--------  (except surface points, which will probably lie
!--------   somewhat outside in vertical)

!        if(mype.eq.0) write(0,*)' at 6 in sortps, mype,npdata=',mype,npdataall
  ptimemax=-huge(ptimemax)
  ptimemin=huge(ptimemax)
  if(npdata.gt.0) then
   do i=1,npdata
    iptype=nint(ptype(i))
    iptypea=nint(abs(ptype(i)))
    if(iptype.lt.0) then
     pges(i)=1.e20
     icode(i)=7
    end if
    if(pelev(i).gt.9.e4) then
     pges(i)=1.e20
     icode(i)=4
    end if
    if(perr(i).gt.20.) then
     pges(i)=1.e20
     icode(i)=5
     kptypea=max(1,min(500,iptypea))
     ibadtype(kptypea)=ibadtype(kptypea)+1
    end if
    if(ptime(i).lt.-delhour.or.ptime(i).gt.delhour) then
     pges(i)=1.e20
     icode(i)=3
    end if
    ptimemax=max(ptime(i),ptimemax)
    ptimemin=min(ptime(i),ptimemin)
   end do
  end if
!        if(mype.eq.0) write(0,*)' at 7 in sortps, mype,npdata=',mype,npdataall
  call mpi_reduce(ptimemax,ptimemaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(ptimemin,ptimeminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(ibadtype,ibadtypeall,500,mpi_integer,mpi_sum,0,my_comm,ierror)
        if(mype.eq.0) then
         print *,' in sortps, ptimemax,min,delhour=',ptimemaxall,ptimeminall,delhour
         do i=1,500
          if(ibadtypeall(i).gt.0) &
               print *,' in sortps, num type ',i,' obs with obserr > 20mb = ',ibadtypeall(i)
         end do 
        end if
  deallocate(ibadtype)
  deallocate(ibadtypeall)
  ptmax=-huge(ptmax)
  ptmin=huge(ptmax)
  ptrms8=0.
  ptbar8=0.
  count8=0.
!        if(mype.eq.0) write(0,*)' at 8 in sortps, mype,npdata=',mype,npdataall
  if(npdata.gt.0) then
   do i=1,npdata
    if(pges(i).lt.1.e19) then
          if(mype.eq.0) print *,' i,pob-pges,pob,pges=',i,pobs(i)-pges(i),pobs(i),pges(i)
     iptype=nint(ptype(i))
     ptmax=max(pobs(i)-pges(i),ptmax)
     ptmin=min(pobs(i)-pges(i),ptmin)
     ptrms8=ptrms8+(1._8*(pobs(i)-pges(i)))**2
     ptbar8=ptbar8+1._8*(pobs(i)-pges(i))
     count8=count8+1._8
    end if
   end do
  end if
!        if(mype.eq.0) write(0,*)' at 9 in sortps, mype,npdata=',mype,npdataall

!------------------ print out stats on fit of ges to data

  call resps(pobs,pges,ptype,npdata,iwrite)
  call mpi_reduce(ptmax,ptmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(ptmin,ptminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(ptrms8,ptrmsall8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(ptbar8,ptbarall8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(count8,countall8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0.and.countall8.gt.0._8) then
   ptrmsall8=sqrt(ptrmsall8/countall8)
   ptbarall8=ptbarall8/countall8
   write(iwrite,'('' in sortps, residual pmax,min='',2es14.5)')ptmaxall,ptminall
   write(iwrite,'('' residual mean, rmsp ='',2es14.5)')ptbarall8,ptrmsall8
  end if

!-------- do gross check for really bad sfcps

  ptmax=-huge(ptmax)
  ptmin=huge(ptmax)
  ptrms8=0._8
  ptbar8=0._8
  count8=0._8
  ngrossp=0
  if(npdata.gt.0) then
   allocate(jwrite(1000))
   do i=1,npdata
    if(pges(i).lt.1.e19) then
     if(abs(pobs(i)-pges(i)).gt.grossp) then
      ngrossp=ngrossp+1
      if(ngrossp.lt.1000) then 
       write(jwrite(ngrossp),347)pobs(i),pges(i),perr(i),plat(i),plon(i), &
                        pelev(i),ptime(i),ptype(i),pstaid(i)
347          format(1h ,2f8.1,f6.1,f7.2,f9.2,f8.1,f12.2,f6.0,2x,a8)
       pepres(ngrossp)= pobs0(i)
       pestaid(ngrossp) = pstaid(i)
       petime(ngrossp) = ptime(i)
       petype(ngrossp) = nint(ptype(i))
       peobs(ngrossp) = pelev(i)
      end if
      pges(i)=1.e20
      icode(i)=2
     else
      ptmax=max(pobs(i)-pges(i),ptmax)
      ptmin=min(pobs(i)-pges(i),ptmin)
      ptrms8=ptrms8+(1._8*(pobs(i)-pges(i)))**2
      ptbar8=ptbar8+1._8*(pobs(i)-pges(i))
      count8=count8+1._8
     end if
    end if
   end do
  end if
  ip = 1
  irc = 1
! if(mype.eq.0) write(0,*) ' sortps(8)--ngrossp, ip: ',ngrossp,ip
! if(ngrossp.gt.0) then
!   iunit = iuniterr + mype
!   write(filename,'(''qcerror'',i3.3)')mype
!   open(iunit,file=filename,form='formatted')
!   write(iunit,500) ngrossp, ip, irc
!   write(iunit,501) (pestaid(i),pepres(i),petime(i),petype(i),i=1,ngrossp)
! end if
  if(mype.eq.0) write(6,500) ngrossp, ip, irc
  if(mype.eq.0) write(6,501) (pestaid(i),pepres(i),petime(i),petype(i),i=1,ngrossp)
500 format(1x,3i8)
501 format(1x,a10,f10.1,f10.2,f10.0)
502 format(1x,a10,f10.1,f10.2,f10.0,f10.1)

!--------write out guess values

    irc = 3
    iunitg = iuniterr + mype + npes + 1
    write(filenameg,'(''guess'',i3.3)')mype
!!  open(iunitg,file=filenameg,form='formatted')
    if(writges .and. npdata.gt.0) then
!-------first replace large pges values with 999999.9
!     do i=1,npdata
!      pges0(i)=pges(i)
!      if(pges(i).gt.1.e19) pges0(i)=999999.9
!     end do
!!    write(iunitg,500) npdata, ip, irc
!!    write(iunitg,502) (pstaid(i),pobs0(i),ptime(i),ptype(i),pges0(i),i=1,npdata)
    endif

!------------------ print out stats on fit of ges to data after gross 
!-------------------             check

  call mpi_allreduce(ngrossp,ngrosspall,1,mpi_integer,mpi_sum,my_comm,ierror)
  if(ngrosspall.gt.0) then
   allocate(ngross_table(0:npes-1)) ; allocate(ngross_disp(0:npes-1))
   call mpi_gather(ngrossp,1,mpi_integer,ngross_table,1,mpi_integer,0,my_comm,ierror)
   ngross_disp=0
   if(mype.eq.0) then
    do ipe=0,npes-1
     if(ipe.gt.0) ngross_disp(ipe)=ngross_disp(ipe-1)+ngross_table(ipe-1)
    end do
   end if
   allocate(jwrite0(ngrosspall))
   call mpi_gatherv(jwrite,90*ngrossp,mpi_character, &
          jwrite0,90*ngross_table,90*ngross_disp,mpi_character,0,my_comm,ierror)
   deallocate(ngross_table) ; deallocate(ngross_disp)
   if(mype.eq.0) then
    write(iwrite,*)' sfcps which fail gross check follow:'
    write(iwrite,*)'  pobs, pges, perr, plat,  plon, pelev,',' htime,  htype,  hid'
    do i=1,ngrosspall
     write(iwrite,*)jwrite0(i)
    end do

    write(iwrite,*)' ngrossp=',ngrosspall
    write(iwrite,*)' fit to sfcps after gross errors removed follows:'
   end if
   deallocate(jwrite0)
   call resps(pobs,pges,ptype,npdata,iwrite)
   call mpi_reduce(ptmax,ptmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
   call mpi_reduce(ptmin,ptminall,1,mpi_real,mpi_min,0,my_comm,ierror)
   call mpi_reduce(ptrms8,ptrmsall8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   call mpi_reduce(ptbar8,ptbarall8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   call mpi_reduce(count8,countall8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   if(mype.eq.0.and.countall8.gt.0._8) then
    ptrmsall8=sqrt(ptrmsall8/countall8)
    ptbarall8=ptbarall8/countall8
    write(iwrite,'('' in sortps, residual hmax,min='',2es14.5)')ptmaxall,ptminall
    write(iwrite,'('' residual mean, rmsh ='',2es14.5)')ptbarall8,ptrmsall8
   end if
  end if

!   before removing flagged obs, write to events file

 if(npdata.gt.0) then
  write(eventfile,'("events",i4)')mype+9000
  ievout=4
  open(ievout,file=eventfile,form='formatted',position='append')
  do i=1,npdata
   if(icode(i).gt.0)then
    write(ievout,'("PP.",i3.3,11e13.5,2x,a8)')icode(i), &
     pobs(i),pges0(i),xmsg,plon(i),plat(i),pelev(i), &
     ptime(i),ptobs(i),zqm(i),perr(i),ptype(i),pstaid(i)
   end if
  end do
  close(ievout)
 end if

!       remove flagged obs

  ii=0
  if(npdata.gt.0) then
   do i=1,npdata
    if(pges(i).lt.1.e19) then
     ii=ii+1
     perr(ii)=perr(i)
     plon(ii)=plon(i)
     plat(ii)=plat(i)
     plong(ii)=plong(i)
     platg(ii)=platg(i)
     pobs(ii)=pobs(i)
     pelev(ii)=pelev(i)
     pges(ii)=pges(i)
     ptobs(ii)=ptobs(i)
     ptime(ii)=ptime(i)
     zqm(ii)=zqm(i)
     ptype(ii)=ptype(i)
     pstaid(ii)=pstaid(i)
     iplabel(ii)=iplabel(i)
     bighhh(1:lbig2ges,ii)=bighhh(1:lbig2ges,i)
     ibighhh(1:lbig2ges,ii)=ibighhh(1:lbig2ges,i)
     hsfc(ii)=hsfc(i)
     etaobssfc(ii)=etaobssfc(i)
    end if
   end do
  end if
  mpdata=ii
                 print *,' in sortps, mype,mpdata=',mype,mpdata

  call wrsfcps(perr,plon,plat,plong,platg,pobs,pelev,pges, &
               hsfc,etaobssfc,bighhh,ibighhh, &
               ptobs,ptime,zqm,ptype,pstaid,iplabel,mpdata,lbig2ges)

  deallocate(perr)
  deallocate(pobs) ; deallocate(pelev)
  deallocate(pobs0)
  deallocate(ptime) ; deallocate(ptype)
  deallocate(pstaid)
  deallocate(plong) ; deallocate(platg)
  deallocate(plon) ; deallocate(plat)
  deallocate(pges)
  deallocate(pges0)
  deallocate(ptobs)
  deallocate(zqm)
  deallocate(peobs); deallocate(pestaid)
  deallocate(petime); deallocate(petype)
  deallocate(pepres)
  deallocate(iplabel)
  deallocate(icode)
  call mpi_reduce(npdata,npdataall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(mpdata,mpdataall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) then
   write(iwrite,*)' in sortps, npdata,mpdata=',npdataall,mpdataall
   close(iwrite)
  end if

return
end subroutine sortps
