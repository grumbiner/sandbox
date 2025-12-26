subroutine sortpws(mpwdata,iordges,lbig2ges,lhalfges, &
           pdres01,qgges,imeta,jmeta,lmetaex,lmh,etaiex,ptop,delhour,grosspw, &
           wbglb,dlon,sbglb,dlat,imetaglb,jmetaglb,iuniterr,npes)


!-------- evaluate guess at each obs point. then write out obs to temp
!-------- storage.

  include 'mpif.h'
      include "my_comm.h"
  include 'writges.h'

!-------- external arrays

  real(4) pdres01(imeta*jmeta),qgges(imeta*jmeta,lmetaex)
  integer(4) lmh(imeta*jmeta)
  real(4) etaiex(lmetaex+1)
  real(4),pointer::yopw_com(:),eyopw0_com(:),xbarbpw_com(:)
  real(4),pointer::bighpw_com(:,:,:),pwdata_com(:,:),pwprest_com(:)
  character(8),pointer::pwstaid_com(:)
  integer(4),pointer::ibighpw_com(:,:)
  common/r3dv_pwdata/ibighpw_com,yopw_com,eyopw0_com,xbarbpw_com, &
                     bighpw_com,pwdata_com,pwstaid_com,pwprest_com

!--------  internal work space

  real(4),allocatable::pwerr(:),pwlon(:),pwlat(:),pwobs(:)
  integer(4),allocatable::icode(:)
  integer(8),allocatable::ipwlabel(:)
  character(8),allocatable::pwstaid(:)
  character(8) thisid
  real(4),allocatable::pwpresb(:),pwprest(:),pwges(:),pwges0(:)
  real(4),allocatable::pwtime(:),pwtype(:),pwlong(:),pwlatg(:),pwq(:)
  integer(4),allocatable::ngross_table(:),ngross_disp(:)
  character(90),allocatable::jwrite(:),jwrite0(:)
  real(8) pwtbar_8,pwtrms_8,count_8
  real(8) pwtbarall_8,pwtrmsall_8,countall_8
  character(8),allocatable::pewstaid(:)
  real(4),allocatable::pewobs(:),pewtime(:),pewtype(:)
  real(4),allocatable::bighpw(:,:,:)
  integer(4),allocatable::iwgts(:,:),iwgtsr(:,:)
  character*16 filename,filenameg
  real(4) xmsg
  character(10)eventfile

  call mpi_comm_rank(my_comm,mype,ierr)
  xmsg=9999.

!-------- open dataset to hold diagnostic statistics output

  iwrite=152
  if(mype.eq.0) open(iwrite,file='fitpw',form='formatted')

!-------- bring in all pws from disk

  mpwdata=0
  call count_pws(npwdata)
  call mpi_allreduce(npwdata,npwdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
  if(npwdataall.le.0) return    !  no data to process

  allocate(pwerr(max(1,npwdata))) ; allocate(pwlon(max(1,npwdata)))
  allocate(pwlat(max(1,npwdata))) ; allocate(pwobs(max(1,npwdata)))
  allocate(pwlong(max(1,npwdata))) ; allocate(pwlatg(max(1,npwdata)))
  allocate(pwstaid(max(1,npwdata)))
  allocate(pwpresb(max(1,npwdata))) ; allocate(pwprest(max(1,npwdata)))
  allocate(pwtime(max(1,npwdata))) ; allocate(pwtype(max(1,npwdata)))
  allocate(pwges(max(1,npwdata))) ; allocate(pwq(max(1,npwdata)))
  allocate(pwges0(max(1,npwdata)))
  allocate(pewobs(max(1,npwdata))); allocate(pewstaid(max(1,npwdata)))
  allocate(pewtime(max(1,npwdata))); allocate(pewtype(max(1,npwdata)))
  allocate(ipwlabel(max(1,npwdata)))
  allocate(icode(max(1,npwdata)))
  icode=0
  call rdpws(pwerr,pwlon,pwlat,pwlong,pwlatg,pwobs,pwges,pwstaid, &
            pwtime,pwq,pwtype,pwpresb,pwprest,ipwlabel,npwdata)
  pwtype = nint(pwtype)
       pwerrmax=-huge(pwerrmax)
       pwerrmin=huge(pwerrmax)
       if(npwdata.gt.0) then
        do i=1,npwdata
         pwerrmax=max(pwerr(i),pwerrmax)
         pwerrmin=min(pwerr(i),pwerrmin)
        end do
       end if
       call mpi_reduce(pwerrmax,pwerrmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
       call mpi_reduce(pwerrmin,pwerrminall,1,mpi_real,mpi_min,0,my_comm,ierror)
       if(mype.eq.0) print *,' after rdpws in sortpws, pwerrmax,min=',pwerrmaxall,pwerrminall

!-------- evaluate guess q at obs locations. then get guess 

  istaghglb=0
  allocate(iwgts(max(1,npwdata),lbig2ges))
  allocate(bighpw(lmetaex,lbig2ges,max(1,npwdata)))
  call getpwges(pwges,pwlong,pwlatg,pwpresb,pwprest,npwdata,iordges,lbig2ges,lhalfges, &
              iwgts,bighpw, &
       imeta,jmeta,lmetaex,pdres01,qgges,lmh,ptop,pwtype,etaiex,wbglb,dlon,sbglb,dlat,istaghglb, &
        imetaglb,jmetaglb)
  pwges0=pwges
  allocate(iwgtsr(lbig2ges,max(1,npwdata)))
  if(npwdata.gt.0) then
   do i=1,npwdata
    do k=1,lbig2ges
     iwgtsr(k,i)=iwgts(i,k)
    end do
   end do
  end if
!    print *,' in sortpws, mype,ijmeta,max,min(iwgts,iwgtsr)', &
!        mype,imeta*jmeta,maxval(iwgts),minval(iwgts),maxval(iwgtsr),minval(iwgtsr)
  deallocate(iwgts)

!-------- flag obs that are outside time window and/or have unreasonable values

  pwtimemax=-huge(pwtimemax)
  pwtimemin=huge(pwtimemax)
  if(npwdata.gt.0) then
   do i=1,npwdata
    if(pwobs(i).gt.9.e4) then
     icode(i)=4
     pwges(i)=1.e20
    end if
    if(pwerr(i).gt.1000.) then
     icode(i)=5
     pwges(i)=1.e20
    end if
    if(pwtime(i).lt.-delhour.or.pwtime(i).gt.delhour) then
     icode(i)=3
     pwges(i)=1.e20
    end if
    pwtimemax=max(pwtime(i),pwtimemax)
    pwtimemin=min(pwtime(i),pwtimemin)
   end do
  end if
  call mpi_reduce(pwtimemax,pwtimemaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(pwtimemin,pwtimeminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) print *,' in sortpws, pwtimemax,min,delhour=',pwtimemaxall,pwtimeminall,delhour
  pwtmax=-huge(pwtmax)
  pwtmin=huge(pwtmax)
  pwtobsmax=-huge(pwtmax)
  pwtobsmin=huge(pwtmax)
  pwtgesmax=-huge(pwtmax)
  pwtgesmin=huge(pwtmax)
  pwtrms_8=0._8
  pwtbar_8=0._8
  count_8=0._8
  if(npwdata.gt.0) then
   do i=1,npwdata
    if(pwges(i).lt.1.e19) then
     pwtobsmax=max(pwobs(i),pwtobsmax)
     pwtobsmin=min(pwobs(i),pwtobsmin)
     pwtgesmax=max(pwges(i),pwtgesmax)
     pwtgesmin=min(pwges(i),pwtgesmin)
     pwtmax=max(pwobs(i)-pwges(i),pwtmax)
     pwtmin=min(pwobs(i)-pwges(i),pwtmin)
     diff=pwobs(i)-pwges(i)
     pwtrms_8=pwtrms_8+1._8*diff*diff
     pwtbar_8=pwtbar_8+1._8*diff
     count_8=count_8+1._8
    end if
   end do
  end if

!------------------- print out stats on fit of ges to data

  call respw(pwobs,pwges,pwtype,npwdata,iwrite,mype)
  call mpi_reduce(pwtgesmax,pwtgesmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(pwtgesmin,pwtgesminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(pwtobsmax,pwtobsmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(pwtobsmin,pwtobsminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(pwtmax,pwtmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(pwtmin,pwtminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(count_8,countall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(pwtrms_8,pwtrmsall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(pwtbar_8,pwtbarall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0.and.countall_8.gt.0._8) then
   pwtrmsall_8=sqrt(pwtrmsall_8/countall_8)
   pwtbarall_8=pwtbarall_8/countall_8
   write(iwrite,'('' in sortpws, ges pwmax,min='',2es14.5)')pwtgesmaxall,pwtgesminall
   write(iwrite,'('' in sortpws, obs pwmax,min='',2es14.5)')pwtobsmaxall,pwtobsminall
   write(iwrite,'('' in sortpws, residual pwmax,min='',2es14.5)')pwtmaxall,pwtminall
   write(iwrite,'('' residual mean, rmspw ='',2es14.5)')pwtbarall_8,pwtrmsall_8
  end if

!-------- do gross check for really bad obs

  pwtmax=-huge(pwtmax)
  pwtmin=huge(pwtmax)
  pwtrms_8=0._8
  pwtbar_8=0._8
  count_8=0._8
  ngrosspw=0
  if(npwdata.gt.0) then
   allocate(jwrite(1000))
   p0 = 1000.
   do i=1,npwdata
    if(pwges(i).lt.1.e19) then
     if(abs(pwobs(i)-pwges(i)).gt.grosspw) then
      icode(i)=2
      ngrosspw=ngrosspw+1
      if(ngrosspw.lt.1000) then
       write(jwrite(ngrosspw),347)pwobs(i),pwges(i),pwerr(i),pwlat(i), &
         pwlon(i),pwtime(i),pwtype(i),pwpresb(i),pwprest(i)
347          format(' ',2f8.1,f6.1,f7.2,f9.2,f12.2,f6.0,2x,f12.2,f12.2)
!     pewobs(ngrosspw) = pwobs(i)
      pewobs(ngrosspw) = p0
      pewstaid(ngrosspw) = pwstaid(i)
      pewtime(ngrosspw) = pwtime(i)
      pewtype(ngrosspw) = nint(pwtype(i))
      end if
      pwges(i)=1.e20
     else
      pwtmax=max(pwobs(i)-pwges(i),pwtmax)
      pwtmin=min(pwobs(i)-pwges(i),pwtmin)
      pwtrms_8=pwtrms_8+1._8*(pwobs(i)-pwges(i))**2
      pwtbar_8=pwtbar_8+1._8*(pwobs(i)-pwges(i))
      count_8=count_8+1._8
     end if
    end if
   end do
  end if
  ipw = 3
  irc = 1
  if(mype.eq.0) write(0,*)' sortpws(10)--ngrosspw,ipw: ',ngrosspw,ipw
  iunit = iuniterr + mype
! write(filename,'(''qcerror'',i3.3)')mype
! open(iunit,file=filename,form='formatted')
!!if(ngrosspw.gt.0) then
!!  write(iunit,500) ngrosspw, ipw, irc
!!  write(iunit,501) (pewstaid(i),pewobs(i),pewtime(i),pewtype(i),i=1,ngrosspw)
!!endif
  if(mype.eq.0) write(6,500) ngrosspw, ipw, irc
  if(mype.eq.0) write(6,501) (pewstaid(i),pewobs(i),pewtime(i),pewtype(i),i=1,min(2000,ngrosspw))
500 format(1x,3i8)
501 format(1x,a10,f10.1,f10.2,f10.0)
502 format(1x,a10,f10.1,f10.2,f10.0,f10.1)

!--------write out guess values

!   irc = 3
!   if(writges .and. npwdata.gt.0) then
!     iunitg = iuniterr + mype + npes + 1
!     write(iunitg,500) npwdata, ipw, irc
!     write(iunitg,501) (pwstaid(i),p0,pwtime(i),pwtype(i),pwges(i),i=1,npwdata)
!     close(iunitg)
!   endif

!------------------ print out stats on fit of ges to data after gross
!-------------------             check

  call mpi_allreduce(ngrosspw,ngrosspwall,1,mpi_integer,mpi_sum,my_comm,ierror)
  if(ngrosspwall.gt.0) then
   allocate(ngross_table(0:npes-1)) ; allocate(ngross_disp(0:npes-1))
   call mpi_gather(ngrosspw,1,mpi_integer,ngross_table,1,mpi_integer,0,my_comm,ierror)
   ngross_disp=0
   if(mype.eq.0) then
    do ipe=0,npes-1
     if(ipe.gt.0) ngross_disp(ipe)=ngross_disp(ipe-1)+ngross_table(ipe-1)
    end do
   end if
   allocate(jwrite0(ngrosspwall))
   call mpi_gatherv(jwrite,90*ngrosspw,mpi_character, &
          jwrite0,90*ngross_table,90*ngross_disp,mpi_character,0,my_comm,ierror)
   deallocate(ngross_table) ; deallocate(ngross_disp)
   if(mype.eq.0) then
       write(iwrite,*)' pws which fail gross check follow:'
       write(iwrite,*)'  pwobs, pwges, pwerr, pwlat,  pwlon, ',' pwtime,  pwtype,  pwid '
    do i=1,ngrosspwall
     write(iwrite,*)jwrite0(i)
    end do
    write(iwrite,*)' ngrosspw=',ngrosspwall
    write(iwrite,*)' fit to pw after gross errors removed follows:'
   end if
   deallocate(jwrite0)
   call respw(pwobs,pwges,pwtype,npwdata,iwrite,mype)
   call mpi_reduce(pwtmax,pwtmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
   call mpi_reduce(pwtmin,pwtminall,1,mpi_real,mpi_min,0,my_comm,ierror)
   call mpi_reduce(pwtrms_8,pwtrmsall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   call mpi_reduce(pwtbar_8,pwtbarall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   call mpi_reduce(count_8,countall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   if(mype.eq.0.and.countall_8.gt.0._8) then
    pwtrmsall_8=sqrt(pwtrmsall_8/countall_8)
    pwtbarall_8=pwtbarall_8/countall_8
    write(iwrite,'('' in sortpws, residual pwmax,min='',2es14.5)')pwtmaxall,pwtminall
    write(iwrite,'('' residual mean, rmspw ='',2es14.5)')pwtbarall_8,pwtrmsall_8
   end if
  end if

!   before removing flagged obs, write to events file

 if(npwdata.gt.0) then
  write(eventfile,'("events",i4)')mype+9000
  ievout=4
  open(ievout,file=eventfile,form='formatted',position='append')
  do i=1,npwdata
   if(icode(i).gt.0)then
    write(ievout,'("PW.",i3.3,11e13.5,2x,a8)')icode(i), &
     pwobs(i),pwges0(i),xmsg,pwlon(i),pwlat(i),pwpresb(i),pwprest(i), &
     pwtime(i),pwq(i),pwerr(i),pwtype(i),pwstaid(i)
   end if
  end do
  close(ievout)
 end if

!     remove flagged obs

  ii=0
  if(npwdata.gt.0) then
   do i=1,npwdata
    if(pwges(i).lt.1.e19) then
     ii=ii+1
     pwerr(ii)=pwerr(i)
     pwlon(ii)=pwlon(i)
     pwlat(ii)=pwlat(i)
     pwlong(ii)=pwlong(i)
     pwlatg(ii)=pwlatg(i)
     pwobs(ii)=pwobs(i)
     pwges(ii)=pwges(i)
     pwstaid(ii)=pwstaid(i)
     pwtime(ii)=pwtime(i)
     pwq(ii)=pwq(i)
     pwtype(ii)=pwtype(i)
     pwpresb(ii)=pwpresb(i)
     pwprest(ii)=pwprest(i)
     ipwlabel(ii)=ipwlabel(i)
     do kk=1,lbig2ges
      iwgtsr(kk,ii)=iwgtsr(kk,i)
      do k=1,lmetaex
       bighpw(k,kk,ii)=bighpw(k,kk,i)
      end do
     end do
    end if
   end do
  end if
  mpwdata=ii

  allocate(yopw_com(max(1,mpwdata)))
  allocate(eyopw0_com(max(1,mpwdata)))
  allocate(xbarbpw_com(max(1,mpwdata)))
  allocate(bighpw_com(lmetaex,lbig2ges,max(1,mpwdata)))
  allocate(ibighpw_com(lbig2ges,max(1,mpwdata)))
  allocate(pwdata_com(max(1,mpwdata),6))
  allocate(pwprest_com(max(1,mpwdata)))
  allocate(pwstaid_com(max(1,mpwdata)))

  if(mpwdata.gt.0) then
   do i=1,mpwdata
    yopw_com(i)=pwobs(i)
    eyopw0_com(i)=pwerr(i)
    xbarbpw_com(i)=pwges(i)
    do kk=1,lbig2ges
     ibighpw_com(kk,i)=iwgtsr(kk,i)
     do k=1,lmetaex
      bighpw_com(k,kk,i)=bighpw(k,kk,i)
     end do
    end do
    pwstaid_com(i)=pwstaid(i)
    pwdata_com(i,1)=pwpresb(i)
    pwdata_com(i,2)=pwlon(i)
    pwdata_com(i,3)=pwlat(i)
    pwdata_com(i,4)=pwtime(i)
    pwdata_com(i,5)=pwtype(i)
    pwdata_com(i,6)=pwq(i)
    pwprest_com(i)=pwprest(i)
   end do
  end if
!    print *,' in sortpws, mype,ijmeta,max,min(ibighpw_com)', &
!        mype,imeta*jmeta,maxval(ibighpw_com),minval(ibighpw_com)
    
  call mpi_reduce(npwdata,npwdataall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(mpwdata,mpwdataall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) write(iwrite,*)' in sortpws, npwdata,mpwdata=',npwdataall,mpwdataall
       pwerrmax=-huge(pwerrmax)
       pwerrmin=huge(pwerrmax)
       if(mpwdata.gt.0) then
        do i=1,mpwdata
         pwerrmax=max(pwerr(i),pwerrmax)
         pwerrmin=min(pwerr(i),pwerrmin)
        end do
       end if
       call mpi_reduce(pwerrmax,pwerrmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
       call mpi_reduce(pwerrmin,pwerrminall,1,mpi_real,mpi_min,0,my_comm,ierror)
       if(mype.eq.0) print *,'  at end of sortpws, pwerrmax,min=',pwerrmaxall,pwerrminall

   deallocate(iwgtsr)
   deallocate(bighpw)
   deallocate(pwerr)
   deallocate(pwlon)
   deallocate(pwlat)
   deallocate(pwlong) ; deallocate(pwlatg)
   deallocate(pwobs)
   deallocate(pwges)
   deallocate(pwges0)
   deallocate(pwstaid)
   deallocate(pwtime) ; deallocate(pwtype)
   deallocate(pwpresb) ; deallocate(pwprest)
   deallocate(pwq)
   deallocate(pewobs); deallocate(pewstaid)
   deallocate(pewtime); deallocate(pewtype)
   deallocate(ipwlabel)
   deallocate(icode)
   if(mype.eq.0) close(iwrite)

return
end subroutine sortpws
