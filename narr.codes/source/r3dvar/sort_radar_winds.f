subroutine sort_radar_winds(erlon0,erlat0,iordges,lbig2ges,lbig3ges,lhalfges, &
        ugges,vgges,hgges,imeta,jmeta,lmetaex, & 
        lmh,lmv,delhour,grossw,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb,iuniterr,npes)

!-------- evaluate guess at each obs point. then  write out obs to temp
!-------- storage.
!--------

  include 'mpif.h'
      include "my_comm.h"
  include 'writges.h'

!-------- external arrays

  real(4) ugges(imeta*jmeta,lmetaex)
  real(4) vgges(imeta*jmeta,lmetaex),hgges(imeta*jmeta,lmetaex+1)
  integer(4) lmh(imeta*jmeta),lmv(imeta*jmeta)

!--------  internal work space

  real(4),allocatable::werr(:),wlon(:),wlat(:),wlong(:),wlatg(:)
  real(4),allocatable::wrange(:),wpres(:),wobs(:),wges(:)
  real(4),allocatable::wletaobs(:),bighw(:,:)
  integer(4),allocatable::ibighw(:,:)
  character(8),allocatable::wstaid(:)
  real(4),allocatable::wtime(:),welev(:),wtype(:),wqm(:)
  real(4),allocatable::etheta(:),delta(:),epsilnw(:)
  integer(4),allocatable::ngross_table(:),ngross_disp(:)
  character(90),allocatable::jwrite(:),jwrite0(:)
  real(8) wtrms_8,wtbar_8,count_8
  real(8) wtrmsall_8,wtbarall_8,countall_8
  integer(8),allocatable::iwlabel(:)
  real(4),allocatable::wgtsv(:,:)
  integer(4),allocatable::iwgtsv(:,:),kbeambot(:),kbeamtop(:)
  real(4),allocatable::staelev(:),beamdepth(:)
  integer(4),allocatable::ireason(:)
  real(4),allocatable::erradar_inflate(:),stalat(:),stalon(:),staheight(:),tiltangle(:)
  integer(4),allocatable::idate5(:,:)
  character(8) this_staid
  character(40) filename

  call mpi_comm_rank(my_comm,mype,ierr)

!-------- open dataset to hold diagnostic statistics output

  iwrite=158
  if(mype.eq.0) open(iwrite,file='fitradarw',form='formatted')

!-------- bring in all winds from disk

  call count_winds(nwdata)
  call mpi_allreduce(nwdata,nwdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
              if(mype.eq.0) print *,' entering sort_radar_winds, nwdataall=',nwdataall
  if(nwdataall.le.0) return    !  no data to process

  allocate(werr(max(1,nwdata))) ; allocate(wlon(max(1,nwdata)))
  allocate(wlat(max(1,nwdata)))
  allocate(wlong(max(1,nwdata))) ; allocate(wlatg(max(1,nwdata)))
  allocate(wrange(max(1,nwdata))) ; allocate(wpres(max(1,nwdata))) ; allocate(wobs(max(1,nwdata)))
  allocate(wletaobs(max(1,nwdata)))
  allocate(bighw(lbig3ges,max(1,nwdata)))
  allocate(ibighw(lbig3ges,max(1,nwdata)))
  allocate(wstaid(max(1,nwdata)))
  allocate(wtime(max(1,nwdata)))
  allocate(welev(max(1,nwdata))) ; allocate(wtype(max(1,nwdata)))
  allocate(etheta(max(1,nwdata))) ; allocate(wqm(max(1,nwdata)))
  allocate(delta(max(1,nwdata))) ; allocate(epsilnw(max(1,nwdata)))
  allocate(wges(max(1,nwdata)))
  allocate(iwlabel(max(1,nwdata)))
  allocate(kbeambot(max(1,nwdata)))
  allocate(kbeamtop(max(1,nwdata)))
  allocate(staelev(max(1,nwdata)))
  allocate(beamdepth(max(1,nwdata)))
  allocate(ireason(max(1,nwdata)))
  allocate(stalat(max(1,nwdata)))
  allocate(stalon(max(1,nwdata)))
  allocate(staheight(max(1,nwdata)))
  allocate(tiltangle(max(1,nwdata)))
  allocate(erradar_inflate(max(1,nwdata)))
  allocate(idate5(5,max(1,nwdata)))
!     if(mype.eq.0) write(0,*)' at 1 in sort_radar_winds'
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
!     if(mype.eq.0) write(0,*)' at 2 in sort_radar_winds'
          wrangemax=-huge(wrangemax)
          wrangemin=huge(wrangemax)
          wtypemax=-huge(wtypemax)
          wtypemin=huge(wtypemax)
          if(nwdata.gt.0) then
           do i=1,nwdata
            if(nint(wtype(i)).ge.2270) then
             if(wrange(i).gt.wrangemax) then
              wrangemax=wrange(i)
             end if
             if(wrange(i).lt.wrangemin) then
              wrangemin=wrange(i)
             end if
             if(wtype(i).lt.wtypemin) then
              wtypemin=wtype(i)
             end if
             if(wtype(i).gt.wtypemax) then
              wtypemax=wtype(i)
             end if
            end if
           end do
          end if
          call mpi_reduce(wrangemax,wrangemaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
          call mpi_reduce(wrangemin,wrangeminall,1,mpi_real,mpi_min,0,my_comm,ierror)
          call mpi_reduce(wtypemax,wtypemaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
          call mpi_reduce(wtypemin,wtypeminall,1,mpi_real,mpi_min,0,my_comm,ierror)
           if(mype.eq.0) then
            print *,' wtypemax,wrangemax=',wtypemaxall,wrangemaxall
            print *,' wtypemin,wrangemin=',wtypeminall,wrangeminall
           end if

!-------- get delta, epsilnw (cos and sin of wind vector 
!-------- components on rotated grid)

      if(mype.eq.0) write(0,*)' at 3 in sort_radar_winds'
  if(nwdata.gt.0) then
   dg2rad=atan(1.)/45.
   cerlat0=cos(erlat0*dg2rad)
   serlat0=sin(erlat0*dg2rad)
      call getdelepsv(etheta,wlon,wlat,erlon0,dg2rad,cerlat0,serlat0, &
                      wlong,wlatg,delta,epsilnw,nwdata)
  end if
      if(mype.eq.0) write(0,*)' at 4 in sort_radar_winds'

!-------- evaluate guess wind at obs locations

  istaghglb=0 ; istagvglb=1
  allocate(wgtsv(max(1,nwdata),lbig2ges))
  allocate(iwgtsv(max(1,nwdata),lbig2ges))
      if(mype.eq.0) write(0,*)' at 5 in sort_radar_winds'
  call get_radar_wges(wges,staelev,kbeambot,kbeamtop,wlong,wlatg,welev,wrange,wstaid,delta,epsilnw,nwdata, &
        iordges,lbig2ges,lhalfges,wgtsv,iwgtsv,imeta,jmeta,lmetaex,ugges,vgges, &
        lmh,lmv,wtype,wobs,hgges,etheta,werr,wbglb,dlon0,sbglb,dlat0,istaghglb,istagvglb,imetaglb,jmetaglb, &
        wlon,wlat,wtime,beamdepth,ireason)
!?????????????????????here--write get_radar_wges first, then continue
      if(mype.eq.0) write(0,*)' at 6 in sort_radar_winds'
  if(nwdata.gt.0) then
   do k=1,lbig2ges
    do i=1,nwdata
     if(wtype(i).gt.2269.) then
      bighw(k,i)=wgtsv(i,k)
      ibighw(k,i)=iwgtsv(i,k)
     end if
    end do
   end do
  end if
  deallocate(wgtsv)
  deallocate(iwgtsv)
      if(mype.eq.0) write(0,*)' at 7 in sort_radar_winds'

  wtmax=-huge(wtmax)
  wtmin=huge(wtmax)
  wtrms_8=0._8
  wtbar_8=0._8
  count_8=0._8
  if(nwdata.gt.0) then
   do i=1,nwdata
    if(wges(i).lt.1.e19.and.wtype(i).gt.2269.5) then
     wtmax=max(wobs(i)-wges(i),wtmax)
     wtmin=min(wobs(i)-wges(i),wtmin)
     wtrms_8=wtrms_8+1._8*(wobs(i)-wges(i))**2
     wtbar_8=wtbar_8+1._8*(wobs(i)-wges(i))
     count_8=count_8+1._8
    end if
   end do
  end if
      if(mype.eq.0) write(0,*)' at 8 in sort_radar_winds'

!-----------------   print out stats on fit of ges to data

  call reswind(wpres,etheta,wobs,wges,wtype,nwdata,iwrite)
  call mpi_barrier(my_comm,ierror)
  call mpi_reduce(wtmax,wtmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(wtmin,wtminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(wtrms_8,wtrmsall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(wtbar_8,wtbarall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(count_8,countall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0)   print *,' in sort_radar_winds, count=',countall_8
  if(mype.eq.0.and.countall_8.gt.0._8) then
   wtrmsall_8=sqrt(wtrmsall_8/countall_8)
   wtbarall_8=wtbarall_8/countall_8
   write(iwrite,'('' beam adjusted radar wtmax,min='',2es14.5)')wtmaxall,wtminall
   write(iwrite,'('' beam adjusted radar radar wtbar,rms='',2es14.5)')wtbarall_8,wtrmsall_8
  end if
  call mpi_barrier(my_comm,ierror)
      if(mype.eq.0) write(0,*)' at 9 in sort_radar_winds'

!-------- do gross check for really bad winds

  wtmax=-huge(wtmax)
  wtmin=huge(wtmax)
  wtrms_8=0._8
  wtbar_8=0._8
  count_8=0._8
  ngrossw=0
  if(nwdata.gt.0) then
   allocate(jwrite(1000))
   do i=1,nwdata
    if(wges(i).lt.1.e19.and.wtype(i).gt.2269.) then
     if(abs(wobs(i)-wges(i)).gt.grossw) then
      ngrossw=ngrossw+1
      ireason(i)=2
      if(ngrossw.lt.1000) then
       rwpres=exp(wpres(i))
       write(jwrite(ngrossw),347)wobs(i),wges(i),werr(i),wlat(i), &
        wlon(i),rwpres,wtime(i),wtype(i),etheta(i),wstaid(i)
347      format(1h ,2f8.1,f6.1,f7.2,f9.2,f8.1,f12.2,f6.0,f8.1,2x,a8)
      end if
      wges(i)=1.e20
     else
      ireason(i)=0
      wtmax=max(wobs(i)-wges(i),wtmax)
      wtmin=min(wobs(i)-wges(i),wtmin)
      wtrms_8=wtrms_8+1._8*(wobs(i)-wges(i))**2
      wtbar_8=wtbar_8+1._8*(wobs(i)-wges(i))
      count_8=count_8+1._8
     end if
    end if
   end do
  end if
  iwind = 6
! if(mype.eq.0) write(0,*)' sort_radar_winds--ngrossw,iwind: ',ngrossw,iwind

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
    write(iwrite,*)' beam adjusted radar winds which fail gross check follow:'
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

!      write out all radar obs for plotting purposes

  rmsgp=9999.99
  rmsgm=-9999.99
  if(nwdata.gt.0) then
   numallr=0
   do i=1,nwdata
    if(wtype(i).gt.2269.) numallr=numallr+1
   end do
   if(numallr.gt.0) then
    write(filename,'("radar_remaining_",i3.3)')mype
    open(9994,file=filename,form='formatted')
    rewind 9994
    istart=1
    do while (istart.le.nwdata)
     if(wtype(istart).gt.2269.) then
      numthis=0
      this_staid=wstaid(istart)
      this_tilt=tiltangle(istart)
      ithis1=idate5(1,istart)
      ithis2=idate5(2,istart)
      ithis3=idate5(3,istart)
      ithis4=idate5(4,istart)
      ithis5=idate5(5,istart)
      do i=istart,nwdata
       if(wtype(i).gt.2269.) then
        if(wstaid(i).ne.this_staid.or.tiltangle(i).ne.this_tilt.or. &
           idate5(1,i).ne.ithis1.or.idate5(2,i).ne.ithis2.or. &
           idate5(3,i).ne.ithis3.or.idate5(4,i).ne.ithis4.or. &
           idate5(5,i).ne.ithis5) exit
        numthis=numthis+1
       end if
      end do
      write(9994,'(a8,2f9.3,f9.2,f5.2,2x,i4.4,4i2.2,i6)') &
             this_staid,stalat(istart),stalon(istart),staheight(istart), &
             tiltangle(istart),idate5(1:5,istart),numthis
      j=0
      numtot=0
      do i=istart,nwdata
       numtot=numtot+1
       if(wtype(i).gt.2269.) then
        if(wstaid(i).ne.this_staid.or.tiltangle(i).ne.this_tilt.or. &
           idate5(1,i).ne.ithis1.or.idate5(2,i).ne.ithis2.or. &
           idate5(3,i).ne.ithis3.or.idate5(4,i).ne.ithis4.or. &
           idate5(5,i).ne.ithis5) exit
        j=j+1
        wgesout=wges(i)
        if(abs(wges(i)).gt.rmsgp) wgesout=rmsgp
        beamdepthout=beamdepth(i)
        if(abs(beamdepth(i)).gt.rmsgp) beamdepthout=rmsgm
        write(9994,'(2f9.3,f9.2,2f8.2,f7.1,f5.1,f8.2,f9.2,i4)') &
              wlat(i),wlon(i),welev(i),90.-etheta(i),wobs(i),wtime(i),werr(i)/erradar_inflate(i), &
              wgesout,beamdepthout,ireason(i)
        if(j.eq.numthis) exit
       end if
      end do
      istart=istart+numtot-1
     end if
     istart=istart+1
    end do
    close(9994)
   end if
  end if

!   remove flagged obs

  ii=0
  if(nwdata.gt.0) then
   do i=1,nwdata
    if(wges(i).lt.1.e19.or.wtype(i).lt.2269.) then  !  don't throw away all other processed winds
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
   write(iwrite,*)' in sort_radar_winds, nwdata,mwdata=',nwdataall,mwdataall
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
   deallocate(wstaid)
   deallocate(wtime)
   deallocate(welev)
   deallocate(wtype)
   deallocate(wqm)
   deallocate(iwlabel)
   deallocate(wletaobs)
   deallocate(bighw)
   deallocate(ibighw)
   deallocate(kbeambot)
   deallocate(kbeamtop)
   deallocate(staelev)
   deallocate(beamdepth)
   deallocate(ireason)
   deallocate(stalat)
   deallocate(stalon)
   deallocate(staheight)
   deallocate(tiltangle)
   deallocate(erradar_inflate)
   deallocate(idate5)

return
end subroutine sort_radar_winds
