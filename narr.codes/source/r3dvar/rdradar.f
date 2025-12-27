subroutine rdradar(inbufr,nbufdat,mbufdat, &
           iayear,iamonth,iaday,iahour,delhour,gsstm, &
           erlon0,erlat0,wbglb,sbglb,dlon0,dlat0,imetaglb,jmetaglb, &
           vadlat,vadlon,vadqm,nvad,erradar_inflate,npes)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rdprep     read in and reformat prepda data
!   prgmmr: parrish          org: w/nmc22    date: 90-10-07
!   prgmmr: woollen          org: w/nmc22    date: 93-12-07
!
! abstract: read in and reformat data.  count up and write out again
!              to work files
!
! program history log:
!   90-10-07  parrish
!   95-10-02  parrish: eliminate guess computation from inside here
!   96-06-23  parrish: add okmeso data through separate pipe
!   96-12-02  parrish: add precipitable water data, and turn on 
!                       over water tovs retrievals
!   99-08-23  parrish: create parallel version
!
! usage: call rdtest(mype,iroot,inbufr,alldata,nbufdat,mbufdat, &
!           iayear,iamonth,iaday,iahour,delhour, &
!           one_type,one_pres,one_lat,one_lon,one_error,one_obs,one_theta,one_sbot,one_stop)
!   input argument list:
!     mype:    - working pe number
!     iroot:   - number of root pe
!     npes:    - total number of pe's
!     inbufr   - unit number for bufr data file.
!     nbufdat  - size of array alldata
!     delhour  - time window in hours
!
!   output argument list:
!     alldata  - array containing all data
!     mbufdat  - used length of array alldata
!                     (can be greater than nbufdat, in which case some data is lost)
!     iayear,etc - reference time for observations
!
! attributes:
!   language: cft77
!   machine:  cray ymp
!
!$$$

  include 'mpif.h'
      include "my_comm.h"
  include 'types.h'

  type(general_obs),pointer::alldata(:)
  integer(2),pointer::lev_val(:),lev_max(:)
  common/databuf/lev_val,lev_max,alldata

  real(4),allocatable::erinwmax(:),erinwmin(:),erinwrms(:)
  real(4),allocatable::erinwmaxall(:),erinwminall(:),erinwrmsall(:)
  integer(4),allocatable::icountw(:),icountwall(:)
!---------
  CHARACTER(8)  SUBSET
  character(8) sidchr
  real(8) hdr(10),staid
  character(8) cchdr(5)
   character(8) cstaid
   equivalence (cstaid,staid)
   equivalence (sidchr,staid)
  real(8),allocatable::obs(:,:)
     character(1) chdr(8)
     equivalence (chdr(1),hdr(1))
     equivalence (cchdr(1),hdr(1))
  integer(4) idate5(5)
  logical good
  real(4) vadlat(500),vadlon(500),vadqm(500,60)
      integer(4) numhits(0:nvad),numhits0(0:nvad)
      integer(4) ierrdist(50),ierrdist0(50)
  character(40)filename
  real(4) cutlat(1500),cutlon(1500),cuthgt(1500),cutazm(1500),cutwspd(1500),cuttime(1500)
  real(4) cuterror(1500),cutreason(1500)


  call mpi_comm_rank(my_comm,mype,ierror)

!       open diagnostic output file to catalogue all rejected data by rdradar

  write(filename,'("radar_cut1_",i3.3)')mype
  open(9994,file=filename,form='formatted')
  rewind 9994
  if(mype.eq.0) then
   print *,' in rdradar, vad table follows, for nvad=',nvad
     do i=1,nvad
      print '(" n,lat,lon,qm=",i4,2f9.2,3x,25i3)',i,vadlat(i),vadlon(i),(nint(vadqm(i,k)),k=1,25)
     end do
  end if
  rmsgp=9999.99
  rmsgm=-9999.99

  dg2rad=atan(1.)/45.
  rad2dg=45./atan(1.)
  eradkm=conmc('rerth$')*.001
  cerlat0=cos(erlat0*dg2rad)
  serlat0=sin(erlat0*dg2rad)
!-------------------------------following limits guarantee that points kept can be interpolated
!------------------------------- to with 4-point interpolation in horizontal
  rlongmin=wbglb+3.1*dlon0
  rlongmax=wbglb+2.*dlon0*(imetaglb-1.)-3.1*dlon0
  rlatgmin=sbglb+3.1*dlat0
  rlatgmax=sbglb+dlat0*(jmetaglb-1.)-3.1*dlat0
  if(mype.eq.0) print *,' in rdradar, rlongmin,max=',rlongmin,rlongmax
  if(mype.eq.0) print *,' in rdradar, rlatgmin,max=',rlatgmin,rlatgmax

  allocate(erinwmax(6000)) ; allocate(erinwmin(6000))
  allocate(erinwrms(6000))
  allocate(icountw(6000))
  allocate(OBS(7,1500))

!  zero data counters
!  ------------------

!        if(mype.eq.24) print *,' at 1 in rdradar, mype,mbufdat,itype,lon,lat of 28 =', &
!              mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat
  erinwmax=-huge(erinwmax)
  erinwmin=huge(erinwmax)
  erinwrms=0.
  icountw=0
  werrmin=1.5

  levsmin=huge(levsmin)
  levsmax=-huge(levsmax)
  levszero=0
  nwdata=0
  nmrecs=0
  nmblocks=0
  ibadazm=0
  ibadwnd=0
  ibadlat=0
  ibadlon=0
  ibadlatd=0
  ibadlond=0
  ibaddist=0
  ibadheight=0
  ibadstaheight=0
  ibaderror=0
  ibadvad=0
  notgood=0
  numhits=0
  ierrdist=0
  errormax=-huge(errormax)
  errormin=huge(errormin)
  itypemax=-huge(itypemax)
  itypemin=huge(itypemin)


!   get eta model guess year, month, day, hour

  call eta_ymdh(ietay,ietam,ietad,ietah)

!  OPEN THEN READ THE BUFR DATA
!  ----------------------------
  lundx=inbufr
  call openbf(inbufr,'IN',lundx)
  CALL CLOSBF(INBUFR)

  CALL DATEBF(INBUFR,IY,IM,ID,IH,IDATE)
!        if(mype.eq.24) print *,' at 2 in rdradar, mype,mbufdat,itype,lon,lat of 28 =', &
!              mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat
  if(idate.lt.0) go to 1000
  
  if(mype.eq.0) print *,' radar date, iy,im,id,ih=',iy,im,id,ih

!------    iayear is only last 2 digits.  Eta model wants all 4.
!----------  This is just a temporary fix, only valid for 
!-----------   years 1950 to 2049.
!---------------
  if(iy.lt.100) iy2=iy+1900
  if(iy.lt.50) iy2=iy+2000
  if(iy2.ne.ietay.or.im.ne.ietam.or.id.ne.ietad.or.ih.ne.ietah) then
   if(mype.eq.0) then
    print *,' radar DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    print *,' radar DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    print *,' radar DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    print *,' radar DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    print *,' radar DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    print *,' radar DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    print *,' radar DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    print *,' radar DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    write(0,*)' radar DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    write(0,*)' radar DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    write(0,*)' radar DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    write(0,*)' radar DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    write(0,*)' radar DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    write(0,*)' radar DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    write(0,*)' radar DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    write(0,*)' radar DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
   end if
     stop 15
  end if

!  OPEN AGAIN THEN READ THE BUFR DATA
!  ----------------------------------

  call ufbmem(inbufr,0,kmsgs,iret)


  nchunk=1+(kmsgs-1)/npes
  do lchunk=1,nchunk

   kthis=mype+1+(lchunk-1)*npes
   if(kthis.gt.kmsgs) exit
   call rdmemm(kthis,subset,idate,iret)
   if(iret.ne.0) exit
   nmblocks=nmblocks+1
   nread=0
   DO WHILE(IREADSB(INBUFR).EQ.0)

!      READ THE HEADER
!      ---------------

!         if(mype.eq.24.and.mbufdat.ge.2638.and.mbufdat.le.2730)  &
!               print *,' at 4.1 in rdradar, lchunk,mype,mbufdat,itype,lon,lat of 28 =', &
!              lchunk,mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat
      CALL UFBINT(INBUFR,HDR,10,1,LEVS, &
              'RPID CLAT CLON SELV ANEL YEAR MNTH DAYS HOUR MINU')
!         if(mype.eq.24.and.mbufdat.ge.2638.and.mbufdat.le.2730)  &
!               print *,' at 4.2 in rdradar, levs,mype,mbufdat,itype,lon,lat of 28 =', &
!              levs,mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat
      STAID = HDR(1)
      RLAT  = HDR(2)  ! units are
      RLON  = HDR(3)  ! degrees
      staheight=hdr(4)
      tiltangle=hdr(5)
!                                      find vad wind match
                       ivad=0
                       do k=1,nvad
                        if(abs(modulo(rlon-vadlon(k),360.)).lt..2.and.abs(rlat-vadlat(k)).lt..2) then
                         ivad=k
                         exit
                        end if
                       end do
                       numhits(ivad)=numhits(ivad)+1
!         if(mype.eq.24.and.mbufdat.ge.2638.and.mbufdat.le.2730)  &
!               print *,' at 4.3 in rdradar, levs,mype,mbufdat,itype,lon,lat of 28 =', &
!              levs,mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat
      clon=cos(rlon*dg2rad)
      slon=sin(rlon*dg2rad)
      clat=cos(rlat*dg2rad)
      slat=sin(rlat*dg2rad)
      idate5(1)=nint(hdr(6))
      idate5(2)=nint(hdr(7))
      idate5(3)=nint(hdr(8))
      idate5(4)=nint(hdr(9))
      idate5(5)=nint(hdr(10))
      call w3fs21(idate5,nminobs)
      obstm=float(nminobs)

      NMRECS = NMRECS+1


!  GO THROUGH THE DATA LEVELS


!         if(mype.eq.24.and.mbufdat.ge.2638.and.mbufdat.le.2730)  &
!               print *,' at 4.4 in rdradar, levs,mype,mbufdat,itype,lon,lat of 28 =', &
!              levs,mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat
      CALL UFBINT(INBUFR,OBS,7,1500,LEVS, &
                 'STDM SUPLAT SUPLON HEIT RWND RWAZ RSTD')
               levsmin=min(levs,levsmin)
               levsmax=max(levs,levsmax)
               if(levs.eq.0) levszero=levszero+1
      if(levs.gt.0) then
             if(levs.gt.1500) then
                print *,' increase bufr size in rdradar, program stops'
                stop
             end if
!         if(mype.eq.24.and.mbufdat.ge.2638.and.mbufdat.le.2730)  &
!               print *,' at 4.5 in rdradar, levs,mype,mbufdat,itype,lon,lat of 28 =', &
!              levs,mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat

       kx0=2270
       numcut=0
       do k=1,levs
        time=(obstm+obs(1,k)-gsstm)/60.
        if(time.lt.-delhour.or.time.gt.delhour) then
         numcut=numcut+1
         cutlat(numcut)=obs(2,k)
         cutlon(numcut)=obs(3,k)
         cuthgt(numcut)=obs(4,k)
         cutazm(numcut)=obs(6,k)
         cutwspd(numcut)=obs(5,k)
         cuttime(numcut)=60.*time
         cuterror(numcut)=obs(7,k)
         cutreason(numcut)=4
         go to 8901
        end if
        rlond=obs(3,k)
        rlatd=obs(2,k)
        clonh=cos(rlond*dg2rad)
        slonh=sin(rlond*dg2rad)
        clath=cos(rlatd*dg2rad)
        slath=sin(rlatd*dg2rad)
        cdist=slat*slath+clat*clath*(slon*slonh+clon*clonh)
        cdist=max(-1.,min(cdist,1.))
        dist=eradkm*acos(cdist)
        kxadd=nint(dist*.1)
        kx=kx0+kxadd
        call tllv(rlond,rlatd,erlon0,dg2rad,cerlat0,serlat0,rlondg,rlatdg,1)
        if(rlondg.le.rlongmin.or.rlondg.ge.rlongmax.or. &
           rlatdg.le.rlatgmin.or.rlatdg.ge.rlatgmax)  then
         numcut=numcut+1
         cutlat(numcut)=obs(2,k)
         cutlon(numcut)=obs(3,k)
         cuthgt(numcut)=obs(4,k)
         cutazm(numcut)=obs(6,k)
         cutwspd(numcut)=obs(5,k)
         cuttime(numcut)=60.*time
         cuterror(numcut)=obs(7,k)
         cutreason(numcut)=5
         go to 8901
        end if

!  STORE radial WIND DATA
!  ---------------

        rwnd=obs(5,k)
        azm=90.-obs(6,k)
        good=.true.
        if(abs(azm).gt.400.) then
         ibadazm=ibadazm+1 ; good=.false. ; ireason=6
        end if
        if(abs(rwnd).gt.200.) then
         ibadwnd=ibadwnd+1 ; good=.false. ; ireason=6
        end if
        if(abs(rlat).gt.400.) then
         ibadlat=ibadlat+1 ; good=.false. ; ireason=6
        end if
        if(abs(rlon).gt.400.) then
         ibadlon=ibadlon+1 ; good=.false. ; ireason=6
        end if
        if(abs(rlatd).gt.400.) then
         ibadlatd=ibadlatd+1 ; good=.false. ; ireason=6
        end if
        if(abs(rlond).gt.400.) then
         ibadlond=ibadlond+1 ; good=.false. ; ireason=6
        end if
        if(dist.gt.400.) then
         ibaddist=ibaddist+1 ; good=.false. ; ireason=6
        end if
        height=obs(4,k)
        if(staheight.lt.-1000..or.staheight.gt.50000.) then
         ibadstaheight=ibadstaheight+1 ; good=.false. ; ireason=6
        end if
        if(height.lt.-1000..or.height.gt.50000.) then
         ibadheight=ibadheight+1 ; good=.false. ; ireason=6
        end if
        error=erradar_inflate*obs(7,k)
            ierrdist(max(1,min(nint(obs(7,k)),50)))=ierrdist(max(1,min(nint(obs(7,k)),50)))+1
        if(obs(7,k).gt.6.) then
         ibaderror=ibaderror+1 ; good=.false. ; ireason=3
        end if
                errormax=max(real(obs(7,k),kind(errormax)),errormax)
                errormin=min(real(obs(7,k),kind(errormin)),errormin)
              if(ivad.eq.0) then
               ibadvad=ibadvad+1 ; good=.false. ; ireason=7
              else
               ivadz=1.+height/500.
               ivadz=max(1,min(ivadz,60))
               if(vadqm(ivad,ivadz).gt.3.5.or.vadqm(ivad,ivadz).lt.-1.) then
                ibadvad=ibadvad+1 ; good=.false. ; ireason=1
               end if
              end if
        if(.not.good) then
         notgood=notgood+1
!        if(notgood.le.30.and.mype.eq.0) &
!          print *,' bad radar ob, kx,staid,rlon,rlat,rlond,rlatd,dist,rwnd,azm,height=', &
!                  kx,cstaid,rlon,rlat,rlond,rlatd,dist,rwnd,azm,height
         numcut=numcut+1
         cutlat(numcut)=obs(2,k)
         cutlon(numcut)=obs(3,k)
         cuthgt(numcut)=obs(4,k)
         cutazm(numcut)=obs(6,k)
         cutwspd(numcut)=obs(5,k)
         cuttime(numcut)=60.*time
         cuterror(numcut)=obs(7,k)
         cutreason(numcut)=ireason
        end if
        wqm=0

        if(good) then
         if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
         mbufdat=mbufdat+1
         nwdata=nwdata+1
         if(mbufdat.le.nbufdat) then
          alldata(mbufdat)%type=kx
!            if(mbufdat.eq.3865.and.mype.eq.6)  &
!                    print *,' radar wind,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
!                                                      kx,cstaid,rlond,rlatd
             itypemax=max(itypemax,kx)
             itypemin=min(itypemin,kx)
          alldata(mbufdat)%staid=cstaid
          alldata(mbufdat)%error=error
          alldata(mbufdat)%lon=rlond
          alldata(mbufdat)%lat=rlatd
          alldata(mbufdat)%long=rlondg
          alldata(mbufdat)%latg=rlatdg
          alldata(mbufdat)%pressure=0.
          alldata(mbufdat)%wobs=rwnd
          alldata(mbufdat)%theta=azm
          alldata(mbufdat)%tobs=dist   ! stick range into tobs--will use range to estimate beam spread
          alldata(mbufdat)%delta=0.
          alldata(mbufdat)%epsilnw=0.
          alldata(mbufdat)%time=time
          alldata(mbufdat)%elevobs=height
          alldata(mbufdat)%qobs=staheight  !  stick sta elevation into qobs (will later contain kbeambot)
          alldata(mbufdat)%group=2002
          alldata(mbufdat)%label=kthis-1_8+kmsgs*(nwdata-1_8)
          alldata(mbufdat)%wqm=wqm
          alldata(mbufdat)%bigh(1)=rlat
          alldata(mbufdat)%bigh(2)=rlon
          alldata(mbufdat)%bigh(3)=staheight
          alldata(mbufdat)%bigh(4)=tiltangle
          alldata(mbufdat)%bigh(5)=erradar_inflate
          alldata(mbufdat)%ibigh(1:5)=idate5(1:5)
          if(kx.le.6000) then
           icountw(kx)=icountw(kx)+1
           erinwmax(kx)=max(erinwmax(kx),error)
           erinwmin(kx)=min(erinwmin(kx),error)
           erinwrms(kx)=erinwrms(kx)+error**2
          end if
!                      if(modulo(abs(rlond-220.41),360.).lt..05.and.abs(rlatd-59.55).lt..05) then
!                          print *,' in rdradar,  type,elev,lat,lon,id=',kx,height,rlatd,rlond,cstaid
!                      end if
         end if
        end if


8901   continue       !  go here if obs outside space/time range of model domain

       end do       !  end loop over levels of non-sat data
       if(numcut.gt.0) then
        write(9994,'(a8,2f9.3,f9.2,f5.2,2x,i4.4,4i2.2,i6)') &
            cstaid,rlat,rlon,staheight,tiltangle,idate5,numcut
        do k=1,numcut
         write(9994,'(2f9.3,f9.2,2f8.2,f7.1,f5.1,f8.2,f9.2,i4)') &
             cutlat(k),cutlon(k),cuthgt(k),cutazm(k),cutwspd(k),cuttime(k),cuterror(k), &
             rmsgp,rmsgm,nint(cutreason(k))
        end do
       end if

      end if       ! end if on test of levs > 0

   end do         !  end of do while

  end do         !  end of do loop for partition of work across pe's and/or threads



1000  if(mype.eq.0) print *,' reached eof on radar file.  exit rdradar'
!        if(mype.eq.24) print *,' at 5 in rdradar, mype,mbufdat,itype,lon,lat of 28 =', &
!              mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat

  CALL CLOSBF(INBUFR)

!  normal exit

  nreduce=1
  call mpi_reduce(nmrecs,nmrecsall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' number of radar dta records read=',nmrecsall
  call mpi_reduce(nmblocks,nmblocksall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' number of radar data blocks read=',nmblocksall
  call mpi_reduce(mbufdat,mbufdatmax,nreduce,mpi_integer,mpi_max,0,my_comm,ierror)
  call mpi_reduce(mbufdat,mbufdatmin,nreduce,mpi_integer,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) print *,' in rdradar, mbufdatmin,max,nbufdat=',mbufdatmin,mbufdatmax,nbufdat
  call mpi_reduce(nwdata,nwdataall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' in rdradar, nwdata=',nwdataall
  call mpi_reduce(levsmin,levsminall,1,mpi_integer4,mpi_min,0,my_comm,ierror)
  call mpi_reduce(levsmax,levsmaxall,1,mpi_integer4,mpi_max,0,my_comm,ierror)
  call mpi_reduce(levszero,levszeroall,1,mpi_integer4,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' in rdradar, levsmin,max=',levsminall,levsmaxall
  if(mype.eq.0) print *,' in rdradar, num levs=0 =',levszeroall

!--------   summary of input errors

  nreduce=6000

  allocate(icountwall(6000))
  call mpi_reduce(icountw,icountwall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  deallocate(icountw)
  allocate(erinwmaxall(6000))
  call mpi_reduce(erinwmax,erinwmaxall,nreduce,mpi_real,mpi_max,0,my_comm,ierror)
  deallocate(erinwmax)
  allocate(erinwminall(6000))
  call mpi_reduce(erinwmin,erinwminall,nreduce,mpi_real,mpi_min,0,my_comm,ierror)
  deallocate(erinwmin)
  allocate(erinwrmsall(6000))
  call mpi_reduce(erinwrms,erinwrmsall,nreduce,mpi_real,mpi_sum,0,my_comm,ierror)
  deallocate(erinwrms)
  if(mype.eq.0) then
         do itype=1,6000
          if(icountwall(itype).gt.0) print *,' for type=',itype, &
             ' num=',icountwall(itype), &
            ' erinwmax,min,rms=',erinwmaxall(itype),erinwminall(itype), &
                  sqrt(erinwrmsall(itype)/icountwall(itype))
         end do
  end if
  deallocate(icountwall)
  deallocate(erinwmaxall)
  deallocate(erinwminall)
  deallocate(erinwrmsall)
  deallocate(obs)
  call mpi_reduce(ibadazm,ibadazm0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, number bad azimuths=',ibadazm0
  call mpi_reduce(ibadwnd,ibadwnd0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, number bad winds=',ibadwnd0
  call mpi_reduce(ibadlat,ibadlat0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, number bad lats=',ibadlat0
  call mpi_reduce(ibadlon,ibadlon0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, number bad lons=',ibadlon0
  call mpi_reduce(ibadlatd,ibadlatd0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, number bad latds=',ibadlatd0
  call mpi_reduce(ibadlond,ibadlond0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, number bad londs=',ibadlond0
  call mpi_reduce(ibaddist,ibaddist0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, number bad dists=',ibaddist0
  call mpi_reduce(ibadstaheight,ibadstaheight0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, number bad staheights=',ibadstaheight0
  call mpi_reduce(ibadheight,ibadheight0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, number bad heights=',ibadheight0
  call mpi_reduce(ibaderror,ibaderror0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, number bad errors=',ibaderror0
  call mpi_reduce(ibadvad,ibadvad0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, number tossed because of bad vads=',ibadvad0
  call mpi_reduce(notgood,notgood0,1,mpi_integer,mpi_sum,0, &
                  my_comm,ierror)
     if(mype.eq.0) print *,' in rdradar, total number bad radar obs in rdradar=',notgood0
  call mpi_reduce(numhits,numhits0,nvad+1,mpi_integer,mpi_sum,0,my_comm,ierror)
     if(mype.eq.0) then
      do i=0,nvad
       print *,' in rdradar, numhits for vad(',i,') = ',numhits0(i)
      end do
     end if
  call mpi_reduce(ierrdist,ierrdist0,50,mpi_integer,mpi_sum,0,my_comm,ierror)
     if(mype.eq.0) then
      do i=1,50
       print *,' in rdradar, number of winds with wind error = ',i,' m/sec is ',ierrdist0(i)
      end do
     end if
  call mpi_reduce(errormax,errormax0,1,mpi_real4,mpi_max,0,my_comm,ierror)
  call mpi_reduce(errormin,errormin0,1,mpi_real4,mpi_min,0,my_comm,ierror)
     if(mype.eq.0) then
      print *,' min,max radar error = ',errormin0,errormax0
     end if
  call mpi_reduce(itypemax,itypemax0,1,mpi_integer4,mpi_max,0,my_comm,ierror)
  call mpi_reduce(itypemin,itypemin0,1,mpi_integer4,mpi_min,0,my_comm,ierror)
     if(mype.eq.0) then
      print *,' min,max type in rdradar=',itypemin0,itypemax0
     end if
!        if(mype.eq.24) print *,' at 3 in rdradar, mype,mbufdat,itype,lon,lat of 28 =', &
!              mype,mbufdat,alldata(28)%type,alldata(28)%lon,alldata(28)%lat

  close(9994)

return
end subroutine rdradar
