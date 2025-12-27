subroutine sortwinds(erlon0,erlat0,iordges,lbig2ges,lbig3ges,lhalfges, &
        pdres01,ugges,vgges,hgges,imeta,jmeta,lmetaex, & 
        etamex,etaiex,ptop,lmh,delhour,grossw,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb,iuniterr,npes)

!-------- evaluate guess at each obs point. then  write out obs to temp
!-------- storage.
!--------

  include 'mpif.h'
      include "my_comm.h"
  include 'writges.h'

!-------- external arrays

  real(4) pdres01(imeta*jmeta),ugges(imeta*jmeta,lmetaex)
  real(4) vgges(imeta*jmeta,lmetaex),hgges(imeta*jmeta,lmetaex+1)
  integer(4) lmh(imeta*jmeta)
  real(4) etamex(lmetaex),etaiex(lmetaex+1)

!--------  internal work space

  real(4),allocatable::werr(:),wlon(:),wlat(:),wlong(:),wlatg(:),wrange(:)
  real(4),allocatable::wpres(:),wobs(:),wges(:),wges0(:)
  integer(4),allocatable::icode(:)
  real(4),allocatable::wletaobs(:),bighw(:,:)
  integer(4),allocatable::ibighw(:,:)
  character(8),allocatable::wstaid(:)
  real(4),allocatable::wtime(:),welev(:),wtype(:),wtypep(:),wqm(:)
  real(4),allocatable::etheta(:),delta(:),epsilnw(:)
  integer(4),allocatable::ngross_table(:),ngross_disp(:)
  character(90),allocatable::jwrite(:),jwrite0(:)
  real(8) wtrms_8,wtbar_8,count_8
  real(8) wtrmsall_8,wtbarall_8,countall_8
  integer(8),allocatable::iwlabel(:)
  character(8),allocatable::westaid(:)
  real(4),allocatable::wepres(:),wetime(:),wetype(:),weges(:)
  character(8) tail(100)
  real(4),allocatable::wgtsv(:,:)
  integer(4),allocatable::iwgtsv(:,:),kbeambot(:),kbeamtop(:)
  character*16 filename,filenameg
  real(4) xmsg
  character(10)eventfile

  real(4),allocatable::staelev(:)
  integer(4),allocatable::ireason(:)
  real(4),allocatable::erradar_inflate(:),stalat(:),stalon(:),staheight(:),tiltangle(:)
  integer(4),allocatable::idate5(:,:)

  call mpi_comm_rank(my_comm,mype,ierr)
  xmsg=9999.

!-------- open dataset to hold diagnostic statistics output

  iwrite=158
  if(mype.eq.0) open(iwrite,file='fitw',form='formatted')

!-------- bring in all winds from disk

  call count_winds(nwdata)
  call mpi_allreduce(nwdata,nwdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
              if(mype.eq.0) print *,' entering sortwinds, nwdataall=',nwdataall
  if(nwdataall.le.0) return    !  no data to process

  allocate(werr(max(1,nwdata))) ; allocate(wlon(max(1,nwdata)))
  allocate(wlat(max(1,nwdata)))
  allocate(wlong(max(1,nwdata))) ; allocate(wlatg(max(1,nwdata))) ; allocate(wrange(max(1,nwdata)))
  allocate(wpres(max(1,nwdata))) ; allocate(wobs(max(1,nwdata)))
  allocate(wletaobs(max(1,nwdata)))
  allocate(bighw(lbig3ges,max(1,nwdata)))
  allocate(ibighw(lbig3ges,max(1,nwdata)))
  allocate(wstaid(max(1,nwdata)))
  allocate(wtime(max(1,nwdata)))
  allocate(welev(max(1,nwdata))) ; allocate(wtype(max(1,nwdata)))
  allocate(wtypep(max(1,nwdata)))
  allocate(etheta(max(1,nwdata))) ; allocate(wqm(max(1,nwdata)))
  allocate(delta(max(1,nwdata))) ; allocate(epsilnw(max(1,nwdata)))
  allocate(wges(max(1,nwdata)))
  allocate(wges0(max(1,nwdata)))
  allocate(wepres(max(1,nwdata))); allocate(westaid(max(1,nwdata)))
  allocate(wetime(max(1,nwdata))); allocate(wetype(max(1,nwdata)))
  allocate(weges(max(1,nwdata)))
  allocate(iwlabel(max(1,nwdata)))
  allocate(kbeambot(max(1,nwdata)))
  allocate(kbeamtop(max(1,nwdata)))
  allocate(staelev(max(1,nwdata)))
  allocate(stalat(max(1,nwdata)))
  allocate(stalon(max(1,nwdata)))
  allocate(staheight(max(1,nwdata)))
  allocate(tiltangle(max(1,nwdata)))
  allocate(erradar_inflate(max(1,nwdata)))
  allocate(idate5(5,max(1,nwdata)))
  allocate(icode(max(1,nwdata)))
  icode=0
!     if(mype.eq.0) write(0,*)' at 1 in sortwinds'
  call rdwinds(werr,wlon,wlat,wlong,wlatg,wrange,wpres,etheta,delta,epsilnw,wobs,wges, &
               wletaobs,bighw,ibighw,kbeambot,kbeamtop, &
               wstaid,wtime,welev,wqm,wtype,iwlabel,nwdata,lbig3ges)
  if(nwdata.gt.0) then
   staelev=0.
   stalat=0.
   stalon=0.
   staheight=0.
   tiltangle=0.
   erradar_inflate=1.
   idate5=0
   do i=1,nwdata
    staelev(i)=kbeambot(i)
    stalat(i)=bighw(1,i)
    stalon(i)=bighw(2,i)
    staheight(i)=bighw(3,i)
    tiltangle(i)=bighw(4,i)
    erradar_inflate(i)=bighw(5,i)
    idate5(1:5,i)=ibighw(1:5,i)
   end do
  end if
  wtype = nint(wtype)
!     if(mype.eq.0) write(0,*)' at 2 in sortwinds'
!         wpresmax=-huge(wpresmax)
!         wpresmin=huge(wpresmax)
!         wtypemax=0
!         wtypemin=0
!         if(nwdata.gt.0) then
!          do i=1,nwdata
!                   if(mype.eq.26.and.i.eq.3389) print *,' after rdwinds, mype,i,wtype,elev=', &
!                                        mype,i,wtype(i),welev(i)
!           if(wpres(i).gt.wpresmax) then
!            wpresmax=wpres(i)
!            wtypemax=wtype(i)
!           end if
!           if(wpres(i).lt.wpresmin) then
!            wpresmin=wpres(i)
!            wtypemin=wtype(i)
!           end if
!          end do
!         end if
!           print *,' mype,wtypemax,wpresmax=',mype,wtypemax,wpresmax
!           print *,' mype,wtypemin,wpresmin=',mype,wtypemin,wpresmin

!-------- get delta, epsilnw (cos and sin of wind vector 
!-------- components on rotated grid)

!     if(mype.eq.0) write(0,*)' at 3 in sortwinds'
  if(nwdata.gt.0) then
   dg2rad=atan(1.)/45.
   cerlat0=cos(erlat0*dg2rad)
   serlat0=sin(erlat0*dg2rad)
      call getdelepsv(etheta,wlon,wlat,erlon0,dg2rad,cerlat0,serlat0, &
                      wlong,wlatg,delta,epsilnw,nwdata)
  end if
!     if(mype.eq.0) write(0,*)' at 4 in sortwinds'

!-------- evaluate guess wind at obs locations

  istaghglb=0 ; istagvglb=1
  allocate(wgtsv(max(1,nwdata),lbig3ges))
  allocate(iwgtsv(max(1,nwdata),lbig3ges))
!     if(mype.eq.0) write(0,*)' at 5 in sortwinds'
  call getwges(wges,wlong,wlatg,wpres,welev,wstaid,delta,epsilnw,nwdata, &
          iordges,lbig2ges,lbig3ges,lhalfges, &
          wletaobs,wgtsv,iwgtsv, &
          imeta,jmeta,lmetaex,pdres01,ugges,vgges, &
          etamex,ptop,lmh,wtype,wobs,hgges,etaiex,etheta,werr,wbglb,dlon0,sbglb,dlat0, &
          istaghglb,istagvglb,imetaglb,jmetaglb)
  wges0=wges
!     if(mype.eq.0) write(0,*)' at 6 in sortwinds'
  if(nwdata.gt.0) then
   do k=1,lbig3ges
    do i=1,nwdata
     bighw(k,i)=wgtsv(i,k)
     ibighw(k,i)=iwgtsv(i,k)
    end do
   end do
  end if
  deallocate(wgtsv)
  deallocate(iwgtsv)
!     if(mype.eq.0) write(0,*)' at 7 in sortwinds'


!-------- make sure all points outside domain are tagged

  wtimemax=-huge(wtimemax)
  wtimemin=huge(wtimemax)
  ibad=0
  if(nwdata.gt.0) then
   do i=1,nwdata
    if(wobs(i).gt.9.e4) then
     icode(i)=4
     wges(i)=1.e20
    end if
    if(werr(i).gt.1000.) then
     icode(i)=5
     wges(i)=1.e20
    end if
    iwtype=nint(wtype(i))
    if(iwtype.lt.0) wges(i)=1.e20
    if(wtime(i).lt.-delhour.or.wtime(i).gt.delhour) then
     icode(i)=3
     wges(i)=1.e20
    end if
    if(wtime(i).gt.1000.) ibad=ibad+1
    wtimemax=max(wtime(i),wtimemax)
    wtimemin=min(wtime(i),wtimemin)
   end do
  end if
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(wtimemax,wtimemaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(wtimemin,wtimeminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(ibad,ibadall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
      write(iwrite,*)' in sortwinds, wtimemax,min,delhour=',wtimemaxall,wtimeminall,delhour
      write(iwrite,*)' in sortwinds, ibad=',ibadall
  wtmax=-huge(wtmax)
  wtmin=huge(wtmax)
  wtrms_8=0._8
  wtbar_8=0._8
  count_8=0._8
  if(nwdata.gt.0) then
   do i=1,nwdata
    if(wges(i).lt.1.e19) then
     wtmax=max(wobs(i)-wges(i),wtmax)
     wtmin=min(wobs(i)-wges(i),wtmin)
     wtrms_8=wtrms_8+1._8*(wobs(i)-wges(i))**2
     wtbar_8=wtbar_8+1._8*(wobs(i)-wges(i))
     count_8=count_8+1._8
    end if
   end do
  end if

!----------------------- print out stats on fit of ges to data

  call reswind(wpres,etheta,wobs,wges,wtype,nwdata,iwrite)
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(wtmax,wtmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(wtmin,wtminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(wtrms_8,wtrmsall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(wtbar_8,wtbarall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(count_8,countall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0)   print *,' in sortwinds, count=',countall_8
  if(mype.eq.0.and.countall_8.gt.0._8) then
   wtrmsall_8=sqrt(wtrmsall_8/countall_8)
   wtbarall_8=wtbarall_8/countall_8
   write(iwrite,'('' wtmax,min='',2es14.5)')wtmaxall,wtminall
   write(iwrite,'('' wtbar,rms='',2es14.5)')wtbarall_8,wtrmsall_8
  end if
  call mpi_barrier(my_comm,ierror)

!-------- do gross check for really bad winds

!   rewind(20)
    ntail = 0
!   do i=1,100
!     read(20,505,err=200,end=200) tail(i)
!     if(mype.eq.0) print *,' sortwinds: tail(',i,'): ',tail(i)
!     ntail = ntail+1
!   enddo
!200 continue
!   close(20)
!   if(mype.eq.0) print *,' sortwinds: ntail = ',ntail
!505 format(1x,a8)

  wtmax=-huge(wtmax)
  wtmin=huge(wtmax)
  wtrms_8=0._8
  wtbar_8=0._8
  count_8=0._8
  ngrossw=0
  if(nwdata.gt.0) then
   allocate(jwrite(1000))
   do i=1,nwdata
!   if(wtype(i).eq.233.) print *,' sortwinds: wstaid: ',wstaid(i)
!   do j=1,ntail
!     if(wstaid(i)(1:6).eq.tail(j)(1:6)) then
!       if(mype.eq.0) print *,' keep tail no.: ',tail(j)
!       goto 201
!     endif
!   enddo
    if(wges(i).lt.1.e19) then
     if(abs(wobs(i)-wges(i)).gt.grossw) then
      icode(i)=2
      ngrossw=ngrossw+1
      if(ngrossw.lt.1000) then
       rwpres=exp(wpres(i))
       write(jwrite(ngrossw),347)wobs(i),wges(i),werr(i),wlat(i), &
        wlon(i),rwpres,wtime(i),wtype(i),etheta(i),wstaid(i)
347      format(1h ,2f8.1,f6.1,f7.2,f9.2,f8.1,f12.2,f6.0,f8.1,2x,a8)
      wepres(ngrossw) = min(1.e7,abs(exp(wpres(i))))
      westaid(ngrossw) = wstaid(i)
      wetime(ngrossw) = wtime(i)
      wetype(ngrossw) = wtype(i)
      if(abs(etheta(i)).gt..1) wetype(ngrossw) = -wetype(ngrossw)
      weges(ngrossw) = wges(i)
      end if
      wges(i)=1.e20
     else
      wtmax=max(wobs(i)-wges(i),wtmax)
      wtmin=min(wobs(i)-wges(i),wtmin)
      wtrms_8=wtrms_8+1._8*(wobs(i)-wges(i))**2
      wtbar_8=wtbar_8+1._8*(wobs(i)-wges(i))
      count_8=count_8+1._8
     end if
    end if
201 continue
   end do
  end if
  iwind = 6
  irc = 1
! if(mype.eq.0) write(0,*)' sortwinds--ngrossw,iwind: ',ngrossw,iwind
  iunit = iuniterr + mype
! write(filename,'(''qcerror'',i3.3)')mype
! open(iunit,file=filename,form='formatted')
!!if(ngrossw.gt.0) then
!!  write(iunit,500) ngrossw, iwind, irc
!!  write(iunit,501) (westaid(i),wepres(i),wetime(i),wetype(i),i=1,ngrossw)
!!endif
  if(mype.eq.0) write(6,500) ngrossw, iwind, irc
  if(mype.eq.0) write(6,501) (westaid(i),wepres(i),wetime(i),wetype(i),i=1,ngrossw)
500 format(1x,3i8)
501 format(1x,a10,f10.1,f10.2,f10.0)
502 format(1x,a10,f10.1,f10.2,f10.0,f10.1)

!--------write out guess values

    irc = 3
    if(writges .and. nwdata.gt.0) then
  !   do i=1,nwdata
  !     if(abs(etheta(i)).lt..1) then
  !       wtypep(i) = nint(wtype(i))
  !     else
  !       wtypep(i) = -nint(wtype(i))
  !     endif
  !     if(wges(i).lt.1.e19) then
  !      wges0(i)=wges(i)
  !     else
  !      wges0(i)=999999.9
  !     end if
  !   enddo
      iunitg = iuniterr + mype + npes + 1
!     write(iunitg,500) nwdata, iwind, irc
!     write(iunitg,502) (wstaid(i),min(1.e7,abs(exp(wpres(i)))),wtime(i),wtypep(i),wges0(i),i=1,nwdata)
    endif

!------------------ print out stats on fit of ges to data after gross
!-------------------             check

  call mpi_barrier(my_comm,ierror)
  call mpi_allreduce(ngrossw,ngrosswall,1,mpi_integer,mpi_sum,my_comm,ierror)
  if(ngrosswall.gt.0) then
   allocate(ngross_table(0:npes-1)) ; allocate(ngross_disp(0:npes-1))
   call mpi_gather(ngrossw,1,mpi_integer,ngross_table,1,mpi_integer,0,my_comm,ierror)
   ngross_disp=0
   if(mype.eq.0) then
    do ipe=0,npes-1
     if(ipe.gt.0) ngross_disp(ipe)=ngross_disp(ipe-1)+ngross_table(ipe-1)
    end do
   end if
   allocate(jwrite0(ngrosswall))
   call mpi_gatherv(jwrite,90*ngrossw,mpi_character, &
          jwrite0,90*ngross_table,90*ngross_disp,mpi_character,0,my_comm,ierror)
   deallocate(ngross_table) ; deallocate(ngross_disp)
   call mpi_barrier(my_comm,ierror)
   if(mype.eq.0) then
    write(iwrite,*)' winds which fail gross check follow:'
    write(iwrite,*)'  wobs, wges, werr, wlat,  wlon, wpres,',' wtime,  wtype,  azimuth '
    do i=1,ngrosswall
     write(iwrite,*)jwrite0(i)
    end do
    write(iwrite,*)' ngrossw=',ngrosswall
    write(iwrite,*)' fit to winds after gross errors removed follows:'
   end if
   deallocate(jwrite0)
   call mpi_barrier(my_comm,ierror)
   call reswind(wpres,etheta,wobs,wges,wtype,nwdata,iwrite)
   call mpi_barrier(my_comm,ierror)
   call mpi_reduce(wtmax,wtmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
   call mpi_reduce(wtmin,wtminall,1,mpi_real,mpi_min,0,my_comm,ierror)
   call mpi_reduce(wtrms_8,wtrmsall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   call mpi_reduce(wtbar_8,wtbarall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   call mpi_reduce(count_8,countall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   if(mype.eq.0.and.countall_8.gt.0._8) then
    wtrmsall_8=sqrt(wtrmsall_8/countall_8)
    wtbarall_8=wtbarall_8/countall_8
    write(iwrite,'('' wtmax,min='',2es14.5)')wtmaxall,wtminall
    write(iwrite,'('' wtbar,rms='',2es14.5)')wtbarall_8,wtrmsall_8
   end if
   call mpi_barrier(my_comm,ierror)
  end if

!   restore radar information for plotting purposes being transferred in wrong places

  if(nwdata.gt.0) then
   do i=1,nwdata
    if(wtype(i).gt.2269.) then
     bighw(1,i)=stalat(i)
     bighw(2,i)=stalon(i)
     bighw(3,i)=staheight(i)
     bighw(4,i)=tiltangle(i)
     bighw(5,i)=erradar_inflate(i)
     ibighw(1:5,i)=idate5(1:5,i)
    end if
   end do
  end if

!   before removing flagged obs, write to events file

 if(nwdata.gt.0) then
  write(eventfile,'("events",i4)')mype+9000
  ievout=4
  open(ievout,file=eventfile,form='formatted',position='append')
  do i=1,nwdata
   if(icode(i).gt.0)then
    if(abs(etheta(i)).lt..1) then
     write(ievout,'("UU.",i3.3,11e13.5,2x,a8)')icode(i), &
       wobs(i),wges0(i),xmsg,wlon(i),wlat(i),exp(wpres(i)),welev(i), &
       wtime(i),wqm(i),werr(i),wtype(i),wstaid(i)
    else
     write(ievout,'("VV.",i3.3,11e13.5,2x,a8)')icode(i), &
       wobs(i),wges0(i),xmsg,wlon(i),wlat(i),exp(wpres(i)),welev(i), &
       wtime(i),wqm(i),werr(i),wtype(i),wstaid(i)
    end if
   end if
  end do
  close(ievout)
 end if

!   remove flagged obs

  ii=0
  if(nwdata.gt.0) then
   do i=1,nwdata
    if(wges(i).lt.1.e19.or.wtype(i).gt.2269.) then  !  don't throw away any radar winds yet
     ii=ii+1
     werr(ii)=werr(i)
     wlon(ii)=wlon(i)
     wlat(ii)=wlat(i)
     wlong(ii)=wlong(i)
     wlatg(ii)=wlatg(i)
     wrange(ii)=wrange(i)
     wpres(ii)=wpres(i)
     etheta(ii)=etheta(i)
     delta(ii)=delta(i)
     epsilnw(ii)=epsilnw(i)
     wobs(ii)=wobs(i)
     wges(ii)=wges(i)
     wletaobs(ii)=wletaobs(i)
     bighw(1:lbig3ges,ii)=bighw(1:lbig3ges,i)
     ibighw(1:lbig3ges,ii)=ibighw(1:lbig3ges,i)
     kbeambot(ii)=kbeambot(i)
     kbeamtop(ii)=kbeamtop(i)
     wstaid(ii)=wstaid(i)
     wtime(ii)=wtime(i)
     welev(ii)=welev(i)
     wqm(ii)=wqm(i)
     wtype(ii)=wtype(i)
     iwlabel(ii)=iwlabel(i)
    end if
   end do
  end if
  mwdata=ii

  call wrwinds(werr,wlon,wlat,wlong,wlatg,wrange,wpres,etheta,delta,epsilnw,wobs,wges, &
              wletaobs,bighw,ibighw,kbeambot,kbeamtop, &
              wstaid,wtime,welev,wqm,wtype,iwlabel,mwdata,lbig3ges)

  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(nwdata,nwdataall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(mwdata,mwdataall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) then
   write(iwrite,*)' in sortwinds, nwdata,mwdata=',nwdataall,mwdataall
   close(iwrite)
  end if

   deallocate(werr)
   deallocate(wlon) ; deallocate(wlat)
   deallocate(wlong) ; deallocate(wlatg) ; deallocate(wrange)
   deallocate(wpres)
   deallocate(etheta)
   deallocate(delta) ; deallocate(epsilnw)
   deallocate(wobs)
   deallocate(wges)
   deallocate(wges0)
   deallocate(wstaid)
   deallocate(wtime)
   deallocate(welev)
   deallocate(wtype)
   deallocate(wtypep)
   deallocate(wqm)
   deallocate(wepres); deallocate(westaid)
   deallocate(wetime); deallocate(wetype)
   deallocate(weges)
   deallocate(iwlabel)
   deallocate(wletaobs)
   deallocate(bighw)
   deallocate(ibighw)
   deallocate(kbeambot)
   deallocate(kbeamtop)
   deallocate(staelev)
   deallocate(stalat)
   deallocate(stalon)
   deallocate(staheight)
   deallocate(tiltangle)
   deallocate(erradar_inflate)
   deallocate(idate5)
   deallocate(icode)

return
end subroutine sortwinds
