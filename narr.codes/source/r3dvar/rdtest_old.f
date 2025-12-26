subroutine rdtest(inbufr,nbufdat,mbufdat,nbufrad,mbufrad, &
           iayear,iamonth,iaday,iahour,delhour,userad,drift, &
           erlon0,erlat0,wbglb,sbglb,dlon0,dlat0,imetaglb,jmetaglb, &
          one_type,one_pres,one_lat,one_lon,one_error,one_obs,one_theta,one_sbot,one_stop, &
          t_on,p_on,pw_on,w_on,q_on,bayes,ithin_goes, &
          vadlat,vadlon,vadqm,nvad,npes)
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
!-----------------------------------------------------------------
!--special info on data: (7/24/96)      ---------------------------
!-----------------------------------------------------------------
!----    data type 183 has q obtained from std atm pressure, obs rh and
!---------- and obs temp.  here we try to get back original rh and then
!---------- further along in the program (sortqs) convert
!---------- back to q based on guess surface pressure, temperature
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!-----------------------------------------------------------------

  include 'mpif.h'
      include "my_comm.h"
  include 'types.h'

  real(4) vadlat(500),vadlon(500),vadqm(500,60)
  type(general_obs),pointer::alldata(:)
  integer(2),pointer::lev_val(:),lev_max(:)
  common/databuf/lev_val,lev_max,alldata
  real(4),pointer::allrad(:,:)
  common/radbuf/allrad
  integer(8) labelrad
  real(4) rlabelrad(2)
  equivalence (labelrad,rlabelrad(1))
  character(8) tail(100)
  logical userad,drift,keeptail
  logical t_on,p_on,pw_on,w_on,q_on,bayes

  integer(4) one_type
  real(4),allocatable::erinpmax(:),erinpmin(:)
  real(4),allocatable::erinpmaxall(:),erinpminall(:)
  integer(4),allocatable::icountp(:),icountpall(:)
  real(4),allocatable::erinqmax(:),erinqmin(:)
  real(4),allocatable::erinqmaxall(:),erinqminall(:)
  integer(4),allocatable::icountq(:),icountqall(:)
  real(4),allocatable::erintmax(:),erintmin(:)
  real(4),allocatable::erintmaxall(:),erintminall(:)
  integer(4),allocatable::icountt(:),icounttall(:)
  real(4),allocatable::erinwmax(:),erinwmin(:)
  real(4),allocatable::erinwmaxall(:),erinwminall(:)
  integer(4),allocatable::icountw(:),icountwall(:)
  real(4),allocatable::erinpwmax(:),erinpwmin(:)
  real(4),allocatable::erinpwmaxall(:),erinpwminall(:)
  integer(4),allocatable::icountpw(:),icountpwall(:)
!---------
  CHARACTER(60) HDSTR,OBSTR,QMSTR,OESTR,rbstr,gostr
  CHARACTER(8)  SUBSET,DATE
  character(8) sidchr
  real(8) hdr(5),staid
  character(8) cchdr(5)
  real(8) grad(18),arad(7)
   character(8) cstaid
   equivalence (cstaid,staid)
   equivalence (sidchr,staid)
  real(8),allocatable::obs(:,:),qms(:,:)
     character(1) chdr(8)
     equivalence (chdr(1),hdr(1))
     equivalence (cchdr(1),hdr(1))
     character(1) dollar
     data dollar/'$'/
  DIMENSION    PSF(0:3),QMF(0:3),QQF(0:3),TMF(0:3),WDF(0:3)
     dimension pwf(0:3)
  LOGICAL      SAT,QMP,QPS,QQQ,QTM,QTS,QWD,QPW,msgpsf
     logical qpw1,qpw2,qpw3,qpw4
  logical goessat

  DATA HDSTR /'SID XOB YOB DHR TYP             '/
  DATA OBSTR /'POB QOB TOB ZOB UOB VOB PWO PW1O PW2O PW3O PW4O CAT XDR YDR'/
  DATA QMSTR /'PQM QQM TQM ZQM WQM NUL PWQ PW1Q PW2Q PW3Q PW4Q     '/
  DATA OESTR /'POE QOE TOE NUL WOE NUL PWE PW1E PW2E PW3E PW4E     '/
  data gostr/'ELEV SOEL TMSK CLAM ACAV        '/
  data rbstr/'TMBR                                      '/
  DATA PSF   /  .9 , 1 , 1 , 1.2 /
  DATA QMF   / 1.5 , 1 , 1 ,  .7 /
  DATA QQF   /  .9 , 1 , 1 , 1.7 /
  DATA TMF   /  .9 , 1 , 1 , 1.2 /
  DATA WDF   /  .9 , 1 , 1 , 1.2 /
  DATA EMERR /  .2 /
     data pwf/1.,1.,1.,2./

  real(4),allocatable::etabl(:,:,:)

  call mpi_comm_rank(my_comm,mype,ierror)
  dg2rad=atan(1.)/45.
  rad2dg=45./atan(1.)
  cerlat0=cos(erlat0*dg2rad)
  serlat0=sin(erlat0*dg2rad)
!-------------------------------following limits guarantee that points kept can be interpolated
!------------------------------- to with 4-point interpolation in horizontal
! rlongmin=wbglb+1.0001*dlon0
! rlongmax=wbglb+2.*dlon0*(imetaglb-1.)-1.0001*dlon0
! rlatgmin=sbglb+1.0001*dlat0
! rlatgmax=sbglb+dlat0*(jmetaglb-1.)-1.0001*dlat0
  rlongmin=wbglb+3.1*dlon0
  rlongmax=wbglb+2.*dlon0*(imetaglb-1.)-3.1*dlon0
  rlatgmin=sbglb+3.1*dlat0
  rlatgmax=sbglb+dlat0*(jmetaglb-1.)-3.1*dlat0
  if(mype.eq.0) print *,' in rdtest, rlongmin,max=',rlongmin,rlongmax
  if(mype.eq.0) print *,' in rdtest, rlatgmin,max=',rlatgmin,rlatgmax

!-------- read in error table

  allocate(etabl(300,33,6))
  if(mype.eq.0) then
   ietabl=19
   rewind ietabl
   etabl=1.e9
   lcount=0
   do l=1,300
    read(ietabl,1002,end=501,err=501)itype
    1002 format(1x,i3)
    lcount=lcount+1
    do k=1,33
     read(ietabl,1001)(etabl(itype,k,m),m=1,6)
     1001 format(1x,6e12.5)
    end do
   end do
   501 continue
   if(lcount.le.0) then
     print *,' OBS ERROR TABLE NOT AVAILABLE TO 3DVAR.'
     print *,' OBS ERROR TABLE NOT AVAILABLE TO 3DVAR.'
     print *,' OBS ERROR TABLE NOT AVAILABLE TO 3DVAR.'
     print *,' OBS ERROR TABLE NOT AVAILABLE TO 3DVAR.'
     print *,' ERROR EXIT'
     call mpi_abort(my_comm,ierrcode,ierror)
   end if
   close(ietabl)
  end if
  
  numbcast=300*33*6
  call mpi_barrier(my_comm,ierror)
  call mpi_bcast(etabl,numbcast,mpi_real,0,my_comm,ierror)

!  read list of aircraft tail nos. to keep

!   rewind(20)
    ntail = 0
!   do i=1,100
!     read(20,505,err=200,end=200) tail(i)
!     if(mype.eq.0) print *,' rdtest: tail(',i,'): ',tail(i)
!     ntail = ntail+1
!   enddo
!200 continue
!   close(20)
!   if(mype.eq.0) print *,' rdtest: ntail = ',ntail
!505 format(1x,a8)

  allocate(erinpmax(300)) ; allocate(erinpmin(300))
  allocate(icountp(300))
  allocate(erinqmax(300)) ; allocate(erinqmin(300))
  allocate(icountq(300))
  allocate(erintmax(300)) ; allocate(erintmin(300))
  allocate(icountt(300))
  allocate(erinwmax(300)) ; allocate(erinwmin(300))
  allocate(icountw(300))
  allocate(erinpwmax(300)) ; allocate(erinpwmin(300))
  allocate(icountpw(300))
  allocate(OBS(14,1500)) ; allocate(QMS(12,1500))

!  zero data counters
!  ------------------

  erinpmax=-huge(erinpmax)
  erinpmin=huge(erinpmin)
  icountp=0
  erinqmax=-huge(erinqmax)
  erinqmin=huge(erinqmax)
  icountq=0
  erintmax=-huge(erinqmax)
  erintmin=huge(erinqmax)
  icountt=0
  erinwmax=-huge(erinqmax)
  erinwmin=huge(erinqmax)
  icountw=0
  erinpwmax=-huge(erinqmax)
  erinpwmin=huge(erinqmax)
  icountpw=0
! jcountpw=0
  terrmin=.8
  werrmin=1.5
  perrmin=1.
  qerrmin=.10
  pwerrmin=1.
  pw1errmin=1.
  pw2errmin=1.
  pw3errmin=.5
  pw4errmin=.5

  nvad=0
  vadlon=0.
  vadlat=0.
  vadqm=-99999
  vadqmmax=-huge(vadqmmax)
  vadqmmin=huge(vadqmmin)
  distmax=0.
  missing_level_latlon=0
  mbufdat=0
  mbufrad=0
  ntdata=0
  npdata=0
  nwdata=0
  nqdata=0
  npwdata=0
  nqtdata=0
  ngdat8=0
  ngdat10=0
  nchanl = 18
! ithin_goes=3
  nmrecs=0
  nmblocks=0
  itypemax=-huge(itypemax)
  itypemin=huge(itypemin)
  NSPROF  = 0
             numssmi=0

  if(mype.eq.0.and.one_type.eq.1001.or.one_type.eq.1002) then

!             this is test run, where only one artificial height observation is returned

   rlon=one_lon
   rlat=one_lat
   call tllv(rlon,rlat,erlon0,dg2rad,cerlat0,serlat0,rlong,rlatg,1)
   if(rlong.gt.rlongmin.and.rlong.lt.rlongmax.and. &
      rlatg.gt.rlatgmin.and.rlatg.lt.rlatgmax) then
    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
    mbufdat=mbufdat+1
    npdata=1
    if(mbufdat.le.nbufdat) then
     kx=one_type
     cstaid='testp'
     poe=one_error
     ppb=one_pres
     pob=log(ppb)
     zob=one_obs
     tob=0.
     time=0.
     alldata(mbufdat)%type=kx
      if(mbufdat.eq.3865.and.mype.eq.6) print *,' 1001,1002,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
                                                       kx,cstaid,rlon,rlat
     alldata(mbufdat)%staid=cstaid
     alldata(mbufdat)%error=poe
     alldata(mbufdat)%lon=rlon
     alldata(mbufdat)%lat=rlat
     alldata(mbufdat)%long=rlong
     alldata(mbufdat)%latg=rlatg
     alldata(mbufdat)%pressure=pob
     alldata(mbufdat)%elevobs=zob
     alldata(mbufdat)%tobs=tob
     alldata(mbufdat)%time=time
     alldata(mbufdat)%group=1002
     alldata(mbufdat)%label=npdata
     icountp(kx)=icountp(kx)+1
     erinpmax(kx)=max(erinpmax(kx),poe)
     erinpmin(kx)=min(erinpmin(kx),poe)
    end if
   end if
   print *,' in rdtest, produce only one height obs as a test'
   return

  else if(mype.ne.0.and.one_type.eq.1001.or.one_type.eq.1002) then
   return
  end if

  if(mype.eq.min(1,npes-1).and.one_type.eq.1003.or.one_type.eq.1004) then

!             this is test run, where only one artificial temperature observation is returned

   rlon=one_lon
   rlat=one_lat
   call tllv(rlon,rlat,erlon0,dg2rad,cerlat0,serlat0,rlong,rlatg,1)
   if(rlong.gt.rlongmin.and.rlong.lt.rlongmax.and. &
      rlatg.gt.rlatgmin.and.rlatg.lt.rlatgmax) then
    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
    mbufdat=mbufdat+1
    ntdata=1
    if(mbufdat.le.nbufdat) then
     kx=one_type
     cstaid='testt'
     toe=one_error
     ppb=one_pres
     pob=log(ppb)
     zob=0.
     tob=one_obs
     time=0.
     alldata(mbufdat)%type=kx
      if(mbufdat.eq.3865.and.mype.eq.6) print *,' 1003,1004,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
                                                       kx,cstaid,rlon,rlat
     alldata(mbufdat)%staid=cstaid
     alldata(mbufdat)%error=toe
     alldata(mbufdat)%lon=rlon
     alldata(mbufdat)%lat=rlat
     alldata(mbufdat)%long=rlong
     alldata(mbufdat)%latg=rlatg
     alldata(mbufdat)%pressure=pob
     alldata(mbufdat)%tobs=tob
     alldata(mbufdat)%time=time
     alldata(mbufdat)%elevobs=zob
     alldata(mbufdat)%qtflag=0
     alldata(mbufdat)%group=1004
     alldata(mbufdat)%label=ntdata
     icountt(kx)=icountt(kx)+1
     erintmax(kx)=max(erintmax(kx),toe)
     erintmin(kx)=min(erintmin(kx),toe)
    end if
   end if
   print *,' in rdtest, produce only one temperature obs as a test'
   return

  else if(mype.ne.min(1,npes-1).and.one_type.eq.1003.or.one_type.eq.1004) then
   return
  end if

  if(mype.eq.min(2,npes-1).and.one_type.eq.1005.or.one_type.eq.1006) then

!             this is test run, where only one artificial moisture observation is returned

   rlon=one_lon
   rlat=one_lat
   call tllv(rlon,rlat,erlon0,dg2rad,cerlat0,serlat0,rlong,rlatg,1)
   if(rlong.gt.rlongmin.and.rlong.lt.rlongmax.and. &
      rlatg.gt.rlatgmin.and.rlatg.lt.rlatgmax) then
    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
    mbufdat=mbufdat+1
    nqdata=1
    if(mbufdat.le.nbufdat) then
     kx=one_type
     cstaid='testq'
     qoe=one_error
     ppb=one_pres
     pob=log(ppb)
     zob=0.
     tob=273.16
     qob=one_obs
     time=0.
     alldata(mbufdat)%type=kx
      if(mbufdat.eq.3865.and.mype.eq.6) print *,' 1005,1006,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
                                                       kx,cstaid,rlon,rlat
     alldata(mbufdat)%staid=cstaid
     alldata(mbufdat)%error=qoe
     alldata(mbufdat)%lon=rlon
     alldata(mbufdat)%lat=rlat
     alldata(mbufdat)%long=rlong
     alldata(mbufdat)%latg=rlatg
     alldata(mbufdat)%pressure=pob
     alldata(mbufdat)%qobs=qob
     alldata(mbufdat)%time=time
     alldata(mbufdat)%tobs=tob
     alldata(mbufdat)%elevobs=zob
     alldata(mbufdat)%maxerror=emerr
     alldata(mbufdat)%group=1006
     alldata(mbufdat)%label=nqdata
     icountq(kx)=icountq(kx)+1
     erinqmax(kx)=max(erinqmax(kx),qoe)
     erinqmin(kx)=min(erinqmin(kx),qoe)
    end if
   end if
   print *,' in rdtest, produce only one moisture obs as a test'
   return

  else if(mype.ne.min(2,npes-1).and.one_type.eq.1005.or.one_type.eq.1006) then
   return
  end if

  if(mype.eq.min(3,npes-1).and.one_type.eq.1007.or.one_type.eq.1008) then

!             this is test run, where only one artificial precip water obs is returned

   rlon=one_lon
   rlat=one_lat
   call tllv(rlon,rlat,erlon0,dg2rad,cerlat0,serlat0,rlong,rlatg,1)
   if(rlong.gt.rlongmin.and.rlong.lt.rlongmax.and. &
      rlatg.gt.rlatgmin.and.rlatg.lt.rlatgmax) then
    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
    mbufdat=mbufdat+1
    npwdata=1
    if(mbufdat.le.nbufdat) then
     kx=one_type
     cstaid='testpw'
     pwe=one_error
     pwo=one_obs
     time=0.
     alldata(mbufdat)%type=kx
      if(mbufdat.eq.3865.and.mype.eq.6) print *,' 1007,1008,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
                                                       kx,cstaid,rlon,rlat
     alldata(mbufdat)%staid=cstaid
     alldata(mbufdat)%error=pwe
     alldata(mbufdat)%lon=rlon
     alldata(mbufdat)%lat=rlat
     alldata(mbufdat)%long=rlong
     alldata(mbufdat)%latg=rlatg
     alldata(mbufdat)%time=time
     alldata(mbufdat)%pwobs=pwo
     alldata(mbufdat)%pressure=one_sbot
     alldata(mbufdat)%ptop=one_stop
     alldata(mbufdat)%group=1008
     alldata(mbufdat)%label=npwdata
     icountpw(kx)=icountpw(kx)+1
     erinpwmax(kx)=max(erinpwmax(kx),pwe)
     erinpwmin(kx)=min(erinpwmin(kx),pwe)
    end if
   end if
   print *,' in rdtest, produce only one pw obs as a test'
   return

  else if(mype.ne.min(3,npes-1).and.one_type.eq.1007.or.one_type.eq.1008) then
   return
  end if

  if(mype.eq.min(4,npes-1).and.one_type.eq.2001.or.one_type.eq.2002) then

!             this is test run, where only one artificial wind obs is returned

   rlon=one_lon
   rlat=one_lat
   call tllv(rlon,rlat,erlon0,dg2rad,cerlat0,serlat0,rlong,rlatg,1)
   if(rlong.gt.rlongmin.and.rlong.lt.rlongmax.and. &
      rlatg.gt.rlatgmin.and.rlatg.lt.rlatgmax) then
    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
    mbufdat=mbufdat+1
    nwdata=1
    if(mbufdat.le.nbufdat) then
     kx=one_type
     cstaid='testw'
     woe=one_error
     uob=one_obs
     time=0.
     ppb=one_pres
     pob=log(ppb)
     zob=0.
     alldata(mbufdat)%type=kx
      if(mbufdat.eq.3865.and.mype.eq.6) print *,' 2001,2002,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
                                                       kx,cstaid,rlon,rlat
     alldata(mbufdat)%staid=cstaid
     alldata(mbufdat)%error=woe
     alldata(mbufdat)%lon=rlon
     alldata(mbufdat)%lat=rlat
     alldata(mbufdat)%long=rlong
     alldata(mbufdat)%latg=rlatg
     alldata(mbufdat)%pressure=pob
     alldata(mbufdat)%wobs=uob
     alldata(mbufdat)%theta=one_theta
     alldata(mbufdat)%delta=0.
     alldata(mbufdat)%epsilnw=0.
     alldata(mbufdat)%time=time
     alldata(mbufdat)%elevobs=zob
     alldata(mbufdat)%group=2002
     alldata(mbufdat)%label=nwdata
     icountw(kx)=icountw(kx)+1
     erinwmax(kx)=max(erinwmax(kx),woe)
     erinwmin(kx)=min(erinwmin(kx),woe)
    end if
   end if
   print *,' in rdtest, produce only one wind obs as a test'
   return

  else if(mype.ne.min(4,npes-1).and.one_type.eq.2001.or.one_type.eq.2002) then
   return
  end if

!   get eta model guess year, month, day, hour

  call eta_ymdh(ietay,ietam,ietad,ietah)

!  OPEN THEN READ THE BUFR DATA
!  ----------------------------
  lundx=inbufr
  call openbf(inbufr,'IN',lundx)
  CALL CLOSBF(INBUFR)

  CALL DATEBF(INBUFR,IY,IM,ID,IH,IDATE)
  if(idate.lt.0) go to 1000
  
  if(mype.eq.0) print *,' prepda date, iy,im,id,ih=',iy,im,id,ih
  iayear=iy
  iamonth=im
  iaday=id
  iahour=ih
!------    iayear is only last 2 digits.  Eta model wants all 4.
!----------  This is just a temporary fix, only valid for 
!-----------   years 1950 to 2049.
!---------------
  if(iy.lt.100) iayear=iy+1900
  if(iy.lt.50) iayear=iy+2000
  if(mype.eq.0) print *,' prepda date, iy,im,id,ih=',iayear,iamonth,iaday,iahour
  if(iayear.ne.ietay.or.iamonth.ne.ietam.or.iaday.ne.ietad.or.iahour.ne.ietah) then
   if(mype.eq.0) then
    print *,' DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    print *,' DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    print *,' DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    print *,' DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    print *,' DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    print *,' DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    print *,' DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    print *,' DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    write(0,*)' DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    write(0,*)' DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    write(0,*)' DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    write(0,*)' DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    write(0,*)' DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    write(0,*)' DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
    write(0,*)' DATE CHECK FAILURE--guess ymdh=',ietay,ietam,ietad,ietah
    write(0,*)' DATE CHECK FAILURE--bufr  ymdh=',iayear,iamonth,iaday,iahour
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

!       READ THE HEADER
!       ---------------

      CALL UFBINT(INBUFR,HDR,5,1,LEVS,HDSTR)
      STAID = HDR(1)
      RLON  = HDR(2)  ! units are
      RLAT  = HDR(3)  ! degrees
      TIME  = HDR(4)
      if(time.lt.-delhour.or.time.gt.delhour) go to 9901
      clon=cos(rlon*dg2rad)
      slon=sin(rlon*dg2rad)
      clat=cos(rlat*dg2rad)
      slat=sin(rlat*dg2rad)
!        obtain eta coordinate lon and lat also, storing in rlong,rlatg
      call tllv(rlon,rlat,erlon0,dg2rad,cerlat0,serlat0,rlong,rlatg,1)
      if(rlong.le.rlongmin.or.rlong.ge.rlongmax.or. &
       rlatg.le.rlatgmin.or.rlatg.ge.rlatgmax) go to 9901
      msgpsf = chdr(6).eq.dollar
      KX    = NINT(HDR(5))
                 itypemax=max(itypemax,kx)
                 itypemin=min(itypemin,kx)
!----------------------------------create table of vad lat-lons and quality marks in 500m increments
!------------------------------------- for cross-referencing bird qc against radar winds
      if(kx.eq.224) then
       ivad=0
       if(nvad.gt.0) then
        do i=1,nvad
         if(abs(rlon-vadlon(i)).lt..1.and.abs(rlat-vadlat(i)).lt..1) then
          ivad=i
          exit
         end if
        end do
       end if
       if(ivad.eq.0) then
        nvad=nvad+1
        if(nvad.gt.500) then
         print *,' IN RDTEST, MORE THAN 500 RADARS:  PROGRAM STOPS'
         stop
        end if
        ivad=nvad
        vadlon(ivad)=rlon
        vadlat(ivad)=rlat
       end if
      end if

             if(bayes.and.kx.ne.120.and.kx.ne.220) go to 9901

!-------- 160 - 169 are overland TOVS retrievals

      SAT   = KX.GE.160 .AND. KX.LE.169       ! use overwater retrievals only
      if(userad) sat=kx.ge.160.and.kx.le.179  ! use radiances
      goessat=kx.eq.164.or.kx.eq.165.or.kx.eq.174.or.kx.eq.175


      IF(kx.ge.170.and.kx.le.179) NSPROF = NSPROF+1
      NMRECS = NMRECS+1

      if(sat.and.userad) then

!---------------process satellite data

       if(goessat) then
        call ufbint(inbufr,arad,5,1,kret,gostr)
        call ufbint(inbufr,grad,1,18,levs,rbstr)

!      do we want to keep this observation?  (don't keep if over land and/or num fov's <=3 )

        landsea=kx
        ncount=nint(arad(5))   ! number of fov's averaged
        if(landsea.ne.164.and.landsea.ne.165.and.ncount.gt.3) then
           nread=nread+1
         if(mod(nread,ithin_goes).eq.0) then
!         yes, keep observation
          isat=-1
          if(sidchr(6:6).eq.'I'.or.sidchr(6:6).eq.'J'.or. &
             sidchr(8:8).eq.'I'.or.sidchr(8:8).eq.'J') then        ! goes8 radiances
           isat=8
           ngdat8=ngdat8+1
          end if
          if(sidchr(6:6).eq.'O'.or.sidchr(6:6).eq.'P'.or. &
             sidchr(8:8).eq.'O'.or.sidchr(8:8).eq.'P') then       ! goes10 radiances
           isat=9
           ngdat10=ngdat10+1
          end if
          if(isat.gt.0) then
           if(mbufrad+1.gt.nbufrad) call increase_allrad(mbufrad,nbufrad)
           mbufrad=mbufrad+1
     
           if(mbufrad.le.nbufrad) then
            allrad(:,mbufrad)=0.
            allrad(1,mbufrad)=kx                  ! type
            allrad(2,mbufrad)=time*60.            ! time     (minutes, relative to analysis time)
            allrad(3,mbufrad)=rlat                ! lat (degrees)
            allrad(4,mbufrad)=rlon                ! lon (degrees)
            allrad(5,mbufrad)=arad(1)*dg2rad      ! satellite elevation angle (radians)
            allrad(6,mbufrad)=0.                  ! solar elevation angle
            allrad(7,mbufrad)=nint(arad(1))+.001  ! nadir step
            allrad(8,mbufrad)=arad(2)             ! solar zenith angle
            allrad(9,mbufrad)=0.                  ! station height
            lndsea=0
            allrad(10,mbufrad)=lndsea+.001        ! land-sea indicator  (0 for sea, 1 for land)
            do l=1,nchanl
             allrad(l+10,mbufrad)=grad(l)
            end do
            allrad(43,mbufrad)=isat
            allrad(44,mbufrad)=rlatg
            allrad(45,mbufrad)=rlong
            labelrad=kthis-1_8+kmsgs*(mbufrad-1_8)
            allrad(46:47,mbufrad)=rlabelrad(1:2)
           end if
          end if
         end if
        end if
       end if

      else

!---------------process non-satellite data


!  GO THROUGH THE DATA LEVELS


       CALL UFBINT(INBUFR,OBS,14,1500,LEVS,OBSTR)
       CALL UFBINT(INBUFR,QMS,12,1500,LEVS,QMSTR)
!      CALL UFBINT(INBUFR,OES,12,1500,LEVS,OESTR)    !  errors are read in externally from table
             if(levs.gt.1500) then
                print *,' increase bufr size in rdradar, program stops'
                stop
             end if

       do k=1,levs
        rlond=obs(13,k)
        rlatd=obs(14,k)
        if(drift) then
         clonh=cos(rlond*dg2rad)
         slonh=sin(rlond*dg2rad)
         clath=cos(rlatd*dg2rad)
         slath=sin(rlatd*dg2rad)
         cdist=slat*slath+clat*clath*(slon*slonh+clon*clonh)
         cdist=max(-1.,min(cdist,1.))
         dist=rad2dg*acos(cdist)
         if(dist.lt.10..and.abs(rlat-rlatd).lt.10.) then
          distmax=max(dist,distmax)
          call tllv(rlond,rlatd,erlon0,dg2rad,cerlat0,serlat0,rlondg,rlatdg,1)
          if(rlondg.le.rlongmin.or.rlondg.ge.rlongmax.or. &
             rlatdg.le.rlatgmin.or.rlatdg.ge.rlatgmax) go to 8901
         else
          rlond=rlon
          rlatd=rlat
          rlondg=rlong
          rlatdg=rlatg
          missing_level_latlon=missing_level_latlon+1
         end if
        else
         rlond=rlon
         rlatd=rlat
         rlondg=rlong
         rlatdg=rlatg
        end if
        ppb = obs(1,k)
        if(ppb.gt.0.) then
         pob = log(obs(1,k)) ! log pressure, in mb, used i
        end if
        qob = obs(2,k)*1e-6
        tob = obs(3,k)+273.15
        zob = obs(4,k)
        uob = obs(5,k)
        vob = obs(6,k)
        pwo = obs(7,k)
        pw1o= obs(8,k)
        pw2o= obs(9,k)
        pw3o= obs(10,k)
        pw4o= obs(11,k)
        cat = obs(12,k)

        pqm = qms(1,k)
        qqm = qms(2,k)
        tqm = qms(3,k)
        zqm = qms(4,k)
        wqm = qms(5,k)
        pwq = qms(7,k)
        pw1q= qms(8,k)
        pw2q= qms(9,k)
        pw3q= qms(10,k)
        pw4q= qms(11,k)
             if(kx.eq.224) then
!-------------------------------update vadqm table
              ivadz=1.+zob/500.
              ivadz=max(1,min(ivadz,60))
              vadqm(ivad,ivadz)=wqm
                  vadqmmax=max(vadqmmax,wqm)
                  vadqmmin=min(vadqmmin,wqm)
             end if
            

!------- get errors from error table
!-------

        itype=kx
        p00=max(0.,min(ppb,2000.))
        if(p00.ge.etabl(itype,1,1)) k1=1
        do kl=1,32
         if(p00.ge.etabl(itype,kl+1,1).and.p00.le.etabl(itype,kl,1)) k1=kl
        end do
        if(p00.le.etabl(itype,33,1)) k1=5
        k2=k1+1
         del=(p00-etabl(itype,k1,1))/(etabl(itype,k2,1)-etabl(itype,k1,1))
         del=max(0.,min(del,1.))
        toe=(1.-del)*etabl(itype,k1,2)+del*etabl(itype,k2,2)
        qoe=(1.-del)*etabl(itype,k1,3)+del*etabl(itype,k2,3)
        qoe=.1*qoe
        woe=(1.-del)*etabl(itype,k1,4)+del*etabl(itype,k2,4)
        poe=(1.-del)*etabl(itype,k1,5)+del*etabl(itype,k2,5)
        pwe=(1.-del)*etabl(itype,k1,6)+del*etabl(itype,k2,6)
        pw1e=pwe
        pw2e=pwe
        pw3e=pwe
        pw4e=pwe

        poe0=poe
        qoe0=qoe
        toe0=toe
        woe0=woe
        pwe0=pwe
        pw1e0=pw1e
        pw2e0=pw2e
        pw3e0=pw3e
        pw4e0=pw4e
         poe=max(poe,perrmin)
         qoe=max(qoe,qerrmin)
         toe=max(toe,terrmin)
         woe=max(woe,werrmin)
         pwe=max(pwe,pwerrmin)
         pw1e=max(pw1e,pw1errmin)
         pw2e=max(pw2e,pw2errmin)
         pw3e=max(pw3e,pw3errmin)
         pw4e=max(pw4e,pw4errmin)

!  SEE WHICH VARIABLES WE HAVE HERE

        keeptail = .false.
!       do n=1,ntail
!         if(cchdr(1)(1:6).eq.tail(n)(1:6)) keeptail = .true.
!       enddo
        if(keeptail) then
         pqm=0
         qqm=0
         tqm=0
         qqm=0
         wqm=0
        end if

        QMP = (PQM.LT.4 .or. keeptail) .AND. (PPB.GT.0.or.msgpsf)
         qmp = qmp .and. (ppb.gt.0..and.ppb.lt.2000.)      ! sanity check
        QPS = (ZQM.LT.4 .or. keeptail)  .AND. QMP .AND. (CAT.EQ.0.or.cat.eq.1)
         qps = qps .and. ppb.gt.95.
         qps = qps .and. (zob.lt.1.e5.and.zob.gt.-1.e4)  !sanity check
        QTM = (TQM.LT.4 .or. keeptail) .AND. QMP .AND. .NOT.SAT
         qtm = qtm .and. (tob.gt.50..and.tob.lt.360.)   ! sanity check
        QQQ = (QQM.LT.4 .or. keeptail) .AND. QMP
         qqq = qqq .and. qtm
         qqq = qqq .and. (qob.gt.0..and.qob.lt..1)  ! sanity check
        QTS = (TQM.LT.4 .or. keeptail) .AND. QMP .AND.  SAT
        QWD = (WQM.LT.4 .or. keeptail) .AND. QMP
         qwd = qwd .and. (abs(uob).lt.500..and.abs(vob).lt.500.)  ! sanity check
         qwd = qwd .and. (kx.le.299.and.kx.ge.200)     !   sanity check on type
         qwd = qwd .and. (kx.ne.227)                   !  exclude radar winds--processed later

!         qwd = (wqm.lt.4 .or. keeptail) .and. qmp .and. (kx.lt.230.or.kx.gt.239) ! exclude aircraft
!         qwd = (wqm.lt.4 .or. keeptail) .and. qmp .and. kx.ne.283  ! all but ssmi winds
        QPW = PWQ.LE.9.and.(kx.ge.151.and.kx.le.155)
         qpw= qpw .and. (pwo.gt.0..and.pwo.lt.500.)  ! sanity check
        qpw1= pw1q.lt.4.and.(kx.ge.156.and.kx.le.159)   
         qpw1= qpw1 .and. (pw1o.gt.0..and.pw1o.lt.300.)  ! sanity check
        qpw2= pw2q.lt.4.and.(kx.ge.156.and.kx.le.159)
         qpw2= qpw2 .and. (pw2o.gt.0..and.pw2o.lt.300.)  ! sanity check
        qpw3= pw3q.lt.4.and.(kx.ge.156.and.kx.le.159)
          qpw3= qpw3 .and. (pw3o.gt.0..and.pw3o.lt.200.)  ! sanity check
!         qpw4= pw4q.lt.4.and.(kx.ge.156.and.kx.le.159)
!          qpw4= qpw4 .and. (pw4o.gt.0..and.pw4o.lt.50.)  ! sanity check
!       4-15-99:  exclude GOES PW top level data (.1mm is not enough precision)
         qpw4=.false.


!  store surface pressure (and other geopotential) data

        if(p_on.and.qps.and.(kx.ge.180.or.cat.eq.0)) then  !  change to exclude radiosondes
                                               !   (except surface report)--don't use geopotential data 

    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
         mbufdat=mbufdat+1
         npdata=npdata+1
         if(mbufdat.le.nbufdat) then
          alldata(mbufdat)%type=kx
!          if(mbufdat.eq.3865.and.mype.eq.6) print *,' sfcp,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
!                                                      kx,cstaid,rlond,rlatd
          alldata(mbufdat)%staid=cstaid
          alldata(mbufdat)%error=poe
          alldata(mbufdat)%lon=rlond
          alldata(mbufdat)%lat=rlatd
          alldata(mbufdat)%long=rlondg
          alldata(mbufdat)%latg=rlatdg
          alldata(mbufdat)%pressure=pob
          alldata(mbufdat)%elevobs=zob
          alldata(mbufdat)%tobs=tob
          alldata(mbufdat)%time=time
          alldata(mbufdat)%group=1002
          alldata(mbufdat)%label=kthis-1_8+kmsgs*(npdata-1_8)
          alldata(mbufdat)%zqm=zqm
          lev_val(mbufdat)=k
          lev_max(mbufdat)=levs
          icountp(kx)=icountp(kx)+1
          erinpmax(kx)=max(erinpmax(kx),poe0)
          erinpmin(kx)=min(erinpmin(kx),poe0)
!                      if(modulo(abs(rlond-220.41),360.).lt..05.and.abs(rlatd-59.55).lt..05) then
!                          print *,' in rdtest, sfcp, mype,mbufdat,type,elev,lat,lon,id=', &
!                                 mype,mbufdat,kx,zob,rlatd,rlond,cstaid
!                      end if
         end if
        end if

!  STORE SPECIFIC HUMIDITY DATA
!  ----------------------------


        if(q_on.and.qqq) then

    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
         mbufdat=mbufdat+1
         nqdata=nqdata+1
         if(mbufdat.le.nbufdat) then
          alldata(mbufdat)%type=kx
!        if(mbufdat.eq.3865.and.mype.eq.6) print *,' q,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
!                                                      kx,cstaid,rlond,rlatd
          alldata(mbufdat)%staid=cstaid
          alldata(mbufdat)%error=qoe*qqf(nint(qqm))
          alldata(mbufdat)%lon=rlond
          alldata(mbufdat)%lat=rlatd
          alldata(mbufdat)%long=rlondg
          alldata(mbufdat)%latg=rlatdg
          alldata(mbufdat)%pressure=pob
          alldata(mbufdat)%qobs=qob
          alldata(mbufdat)%time=time
          alldata(mbufdat)%tobs=tob
          alldata(mbufdat)%elevobs=zob
          alldata(mbufdat)%maxerror=emerr*qmf(nint(qqm))
          alldata(mbufdat)%group=1006
          alldata(mbufdat)%label=kthis-1_8+kmsgs*(nqdata-1_8)
          alldata(mbufdat)%qqm=qqm
          lev_val(mbufdat)=k
          lev_max(mbufdat)=levs
          icountq(kx)=icountq(kx)+1
          erinqmax(kx)=max(erinqmax(kx),qoe0)
          erinqmin(kx)=min(erinqmin(kx),qoe0)
!                      if(modulo(abs(rlond-220.41),360.).lt..05.and.abs(rlatd-59.55).lt..05) then
!                          print *,' in rdtest, q, mype,mbufdat,type,elev,lat,lon,id=', &
!                                 mype,mbufdat,kx,zob,rlatd,rlond,cstaid
!                      end if
         end if
        end if

!  STORE NON-SATEM TEMPERATURE DATA
!  --------------------------------

        if(t_on.and.qtm.and.pob.lt.1.e8.and.tob.lt.1.e8) then

    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
         mbufdat=mbufdat+1
         ntdata=ntdata+1
         if(mbufdat.le.nbufdat) then
          if(qqm.ge.4 .and. ppb .gt. 300.)then
           nqtdata=nqtdata+1
           iqtflg=1
          else
           iqtflg=0
          end if
          alldata(mbufdat)%type=kx
!          if(mbufdat.eq.3865.and.mype.eq.6) print *,' temp,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
!                                                      kx,cstaid,rlond,rlatd
          alldata(mbufdat)%staid=cstaid
          alldata(mbufdat)%error=toe*tmf(nint(tqm))
          alldata(mbufdat)%lon=rlond
          alldata(mbufdat)%lat=rlatd
          alldata(mbufdat)%long=rlondg
          alldata(mbufdat)%latg=rlatdg
          alldata(mbufdat)%pressure=pob
          alldata(mbufdat)%tobs=tob
          alldata(mbufdat)%time=time
          alldata(mbufdat)%elevobs=zob
          alldata(mbufdat)%qtflag=iqtflg
          alldata(mbufdat)%group=1004
          alldata(mbufdat)%label=kthis-1_8+kmsgs*(ntdata-1_8)
          alldata(mbufdat)%tqm=tqm
          lev_val(mbufdat)=k
          lev_max(mbufdat)=levs
          icountt(kx)=icountt(kx)+1
          erintmax(kx)=max(erintmax(kx),toe0)
          erintmin(kx)=min(erintmin(kx),toe0)
!                      if(modulo(abs(rlond-220.41),360.).lt..05.and.abs(rlatd-59.55).lt..05) then
!                          print *,' in rdtest, temp, mype,mbufdat,type,elev,lat,lon,id=', &
!                                 mype,mbufdat,kx,zob,rlatd,rlond,cstaid
!                      end if
         end if
        end if

!  STORE WIND DATA
!  ---------------

        if(w_on.and.qwd.and.pob.lt.1.e8.and.uob.lt.1.e8.and.vob.lt.1.e8) then

    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
         mbufdat=mbufdat+1
         nwdata=nwdata+1
         if(mbufdat.le.nbufdat) then
          alldata(mbufdat)%type=kx
!          if(mbufdat.eq.3865.and.mype.eq.6) print *,' wind,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
!                                                      kx,cstaid,rlond,rlatd
             if(kx.eq.283) numssmi=numssmi+1
          alldata(mbufdat)%staid=cstaid
          alldata(mbufdat)%error=woe*wdf(nint(wqm))
          alldata(mbufdat)%lon=rlond
          alldata(mbufdat)%lat=rlatd
          alldata(mbufdat)%long=rlondg
          alldata(mbufdat)%latg=rlatdg
          alldata(mbufdat)%pressure=pob
          alldata(mbufdat)%wobs=uob
          alldata(mbufdat)%theta=0.
          alldata(mbufdat)%delta=0.
          alldata(mbufdat)%epsilnw=0.
          alldata(mbufdat)%time=time
          alldata(mbufdat)%elevobs=zob
          alldata(mbufdat)%group=2002
          alldata(mbufdat)%label=kthis-1_8+kmsgs*(nwdata-1_8)
          alldata(mbufdat)%wqm=wqm
          lev_val(mbufdat)=k
          lev_max(mbufdat)=levs
          icountw(kx)=icountw(kx)+1
          erinwmax(kx)=max(erinwmax(kx),woe0)
          erinwmin(kx)=min(erinwmin(kx),woe0)
!                      if(modulo(abs(rlond-220.41),360.).lt..05.and.abs(rlatd-59.55).lt..05) then
!                          print *,' in rdtest, wind, mype,mbufdat,type,elev,lat,lon,id=', &
!                                 mype,mbufdat,kx,zob,rlatd,rlond,cstaid
!                      end if
         end if
    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
         mbufdat=mbufdat+1
         nwdata=nwdata+1
         if(mbufdat.le.nbufdat) then
          alldata(mbufdat)%type=kx
          alldata(mbufdat)%staid=cstaid
          alldata(mbufdat)%error=woe*wdf(nint(wqm))
          alldata(mbufdat)%lon=rlond
          alldata(mbufdat)%lat=rlatd
          alldata(mbufdat)%long=rlondg
          alldata(mbufdat)%latg=rlatdg
          alldata(mbufdat)%pressure=pob
          alldata(mbufdat)%wobs=vob
          alldata(mbufdat)%theta=90.
          alldata(mbufdat)%delta=0.
          alldata(mbufdat)%epsilnw=0.
          alldata(mbufdat)%time=time
          alldata(mbufdat)%elevobs=zob
          alldata(mbufdat)%group=2002
          alldata(mbufdat)%label=kthis-1_8+kmsgs*(nwdata-1_8)
          alldata(mbufdat)%wqm=wqm
          lev_val(mbufdat)=k
          lev_max(mbufdat)=levs
         end if
        end if

!  STORE total PRECIP WATER DATA
!  ---------------

        if(qpw.or.qpw1.or.qpw2.or.qpw3.or.qpw4) then

         if(pw_on.and.qpw) then

    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
          mbufdat=mbufdat+1
          npwdata=npwdata+1
          if(mbufdat.le.nbufdat) then
           alldata(mbufdat)%type=10*kx
!          if(mbufdat.eq.3865.and.mype.eq.6) print *,' pw,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
!                                                      10*kx,cstaid,rlond,rlatd
           alldata(mbufdat)%staid=cstaid
           alldata(mbufdat)%error=pwe
           alldata(mbufdat)%lon=rlond
           alldata(mbufdat)%lat=rlatd
           alldata(mbufdat)%long=rlondg
           alldata(mbufdat)%latg=rlatdg
           alldata(mbufdat)%time=time
           alldata(mbufdat)%pwobs=pwo
           alldata(mbufdat)%pressure=1.
           alldata(mbufdat)%ptop=0.
           alldata(mbufdat)%group=1008
           alldata(mbufdat)%label=kthis-1_8+kmsgs*(npwdata-1_8)
           alldata(mbufdat)%pwq=pwq
!                      if(modulo(abs(rlond-220.41),360.).lt..05.and.abs(rlatd-59.55).lt..05) then
!                          print *,' in rdtest, pw, mype,mbufdat,type,elev,lat,lon,id=', &
!                                 mype,mbufdat,10*kx,zob,rlatd,rlond,cstaid
!                      end if
          lev_val(mbufdat)=k
          lev_max(mbufdat)=levs
           icountpw(kx)=icountpw(kx)+1
           erinpwmax(kx)=max(erinpwmax(kx),pwe)
           erinpwmin(kx)=min(erinpwmin(kx),pwe)
          end if
         end if

!  STORE layer1 PRECIP WATER DATA
!  ---------------

         if(pw_on.and.qpw1) then
 
    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
          mbufdat=mbufdat+1
          npwdata=npwdata+1
          if(mbufdat.le.nbufdat) then
           alldata(mbufdat)%type=10*kx+1
!          if(mbufdat.eq.3865.and.mype.eq.6) print *,' pw1,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
!                                                      10*kx+1,cstaid,rlond,rlatd
           alldata(mbufdat)%staid=cstaid
           alldata(mbufdat)%error=pw1e*pwf(nint(pw1q))
           alldata(mbufdat)%lon=rlond
           alldata(mbufdat)%lat=rlatd
           alldata(mbufdat)%long=rlondg
           alldata(mbufdat)%latg=rlatdg
           alldata(mbufdat)%time=time
           alldata(mbufdat)%pwobs=pw1o
           alldata(mbufdat)%pressure=1.
           alldata(mbufdat)%ptop=.9
           alldata(mbufdat)%group=1008
           alldata(mbufdat)%label=kthis-1_8+kmsgs*(npwdata-1_8)
           alldata(mbufdat)%pwq=pw1q
           lev_val(mbufdat)=k
           lev_max(mbufdat)=levs
           icountpw(kx)=icountpw(kx)+1
           erinpwmax(kx)=max(erinpwmax(kx),pw1e)
           erinpwmin(kx)=min(erinpwmin(kx),pw1e)
!                      if(modulo(abs(rlond-220.41),360.).lt..05.and.abs(rlatd-59.55).lt..05) then
!                          print *,' in rdtest, pw1, mype,mbufdat,type,elev,lat,lon,id=', &
!                                 mype,mbufdat,10*kx+1,zob,rlatd,rlond,cstaid
!                      end if
          end if
         end if

!  STORE layer2 PRECIP WATER DATA
!  ---------------

         if(pw_on.and.qpw2) then

    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
          mbufdat=mbufdat+1
          npwdata=npwdata+1
          if(mbufdat.le.nbufdat) then
           alldata(mbufdat)%type=10*kx+2
!          if(mbufdat.eq.3865.and.mype.eq.6) print *,' pw2,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
!                                                      10*kx+2,cstaid,rlond,rlatd
           alldata(mbufdat)%staid=cstaid
           alldata(mbufdat)%error=pw2e*pwf(nint(pw2q))
           alldata(mbufdat)%lon=rlond
           alldata(mbufdat)%lat=rlatd
           alldata(mbufdat)%long=rlondg
           alldata(mbufdat)%latg=rlatdg
           alldata(mbufdat)%time=time
           alldata(mbufdat)%pwobs=pw2o
           alldata(mbufdat)%pressure=.9
           alldata(mbufdat)%ptop=.7
           alldata(mbufdat)%group=1008
           alldata(mbufdat)%label=kthis-1_8+kmsgs*(npwdata-1_8)
           alldata(mbufdat)%pwq=pw2q
           lev_val(mbufdat)=k
           lev_max(mbufdat)=levs
           icountpw(kx)=icountpw(kx)+1
           erinpwmax(kx)=max(erinpwmax(kx),pw2e)
           erinpwmin(kx)=min(erinpwmin(kx),pw2e)
!                      if(modulo(abs(rlond-220.41),360.).lt..05.and.abs(rlatd-59.55).lt..05) then
!                          print *,' in rdtest, pw2, mype,mbufdat,type,elev,lat,lon,id=', &
!                                 mype,mbufdat,10*kx+2,zob,rlatd,rlond,cstaid
!                      end if
          end if
         end if

!  STORE layer3 PRECIP WATER DATA
!  ---------------

         if(pw_on.and.qpw3) then

    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
          mbufdat=mbufdat+1
          npwdata=npwdata+1
          if(mbufdat.le.nbufdat) then
           alldata(mbufdat)%type=10*kx+3
!          if(mbufdat.eq.3865.and.mype.eq.6) print *,' pw3,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
!                                                      10*kx+3,cstaid,rlond,rlatd
           alldata(mbufdat)%staid=cstaid
           alldata(mbufdat)%error=pw3e*pwf(nint(pw3q))
           alldata(mbufdat)%lon=rlond
           alldata(mbufdat)%lat=rlatd
           alldata(mbufdat)%long=rlondg
           alldata(mbufdat)%latg=rlatdg
           alldata(mbufdat)%time=time
           alldata(mbufdat)%pwobs=pw3o
           alldata(mbufdat)%pressure=.7
           alldata(mbufdat)%ptop=.3
           alldata(mbufdat)%group=1008
           alldata(mbufdat)%label=kthis-1_8+kmsgs*(npwdata-1_8)
           alldata(mbufdat)%pwq=pw3q
           lev_val(mbufdat)=k
           lev_max(mbufdat)=levs
           icountpw(kx)=icountpw(kx)+1
           erinpwmax(kx)=max(erinpwmax(kx),pw3e)
           erinpwmin(kx)=min(erinpwmin(kx),pw3e)
!                      if(modulo(abs(rlond-220.41),360.).lt..05.and.abs(rlatd-59.55).lt..05) then
!                          print *,' in rdtest, pw3, mype,mbufdat,type,elev,lat,lon,id=', &
!                                 mype,mbufdat,10*kx+3,zob,rlatd,rlond,cstaid
!                      end if
          end if
         end if

!  STORE layer4 PRECIP WATER DATA
!  ---------------

         if(pw_on.and.qpw4) then

    if(mbufdat+1.gt.nbufdat) call increase_alldata(mbufdat,nbufdat)
          mbufdat=mbufdat+1
          npwdata=npwdata+1
          if(mbufdat.le.nbufdat) then
           alldata(mbufdat)%type=10*kx+4
!          if(mbufdat.eq.3865.and.mype.eq.6) print *,' pw4,for mype=6,mbufdat=3865, type,staid,lon,lat=', &
!                                                      10*kx+4,cstaid,rlond,rlatd
           alldata(mbufdat)%staid=cstaid
           alldata(mbufdat)%error=pw4e*pwf(nint(pw4q))
           alldata(mbufdat)%lon=rlond
           alldata(mbufdat)%lat=rlatd
           alldata(mbufdat)%long=rlondg
           alldata(mbufdat)%latg=rlatdg
           alldata(mbufdat)%time=time
           alldata(mbufdat)%pwobs=pw4o
           alldata(mbufdat)%pressure=.3
           alldata(mbufdat)%ptop=0.
           alldata(mbufdat)%group=1008
           alldata(mbufdat)%label=kthis-1_8+kmsgs*(npwdata-1_8)
           alldata(mbufdat)%pwq=pw4q
           lev_val(mbufdat)=k
           lev_max(mbufdat)=levs
           icountpw(kx)=icountpw(kx)+1
           erinpwmax(kx)=max(erinpwmax(kx),pw4e)
           erinpwmin(kx)=min(erinpwmin(kx),pw4e)
!                      if(modulo(abs(rlond-220.41),360.).lt..05.and.abs(rlatd-59.55).lt..05) then
!                          print *,' in rdtest, pw4, mype,mbufdat,type,elev,lat,lon,id=', &
!                                 mype,mbufdat,10*kx+4,zob,rlatd,rlond,cstaid
!                      end if
          end if
         end if

        end if

8901    continue         !  go here if obs outside range of model domain

       end do           !  end loop over levels of non-sat data

      end if           !  end if for test of sat or nosat data

9901   continue       !  go here if obs outside space/time range of model domain

   end do          !  end of do while

  end do          !  end of do loop for partition of work across pe's and/or threads


1000  if(mype.eq.0) print *,' reached eof on prepda file.  exit rdprep'

  CALL CLOSBF(INBUFR)
  deallocate(etabl)
  deallocate( OBS) ; deallocate(QMS)
! deallocate(OES)

  call mpi_barrier(my_comm,ierror)

!  normal exit


  nreduce=1
  call mpi_reduce(nmrecs,nmrecsall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' number of prepda dta records read=',nmrecsall
  call mpi_reduce(nmblocks,nmblocksall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' number of prepda data blocks read=',nmblocksall
  call mpi_reduce(mbufdat,mbufdatmax,nreduce,mpi_integer,mpi_max,0,my_comm,ierror)
  call mpi_reduce(mbufdat,mbufdatmin,nreduce,mpi_integer,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) print *,' mbufdatmin,max,nbufdat=',mbufdatmin,mbufdatmax,nbufdat
  call mpi_reduce(ntdata,ntdataall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(nqtdata,nqtdataall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' ntdata,nqtdata=',ntdataall,nqtdataall
  call mpi_reduce(nwdata,nwdataall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' nwdata=',nwdataall
  call mpi_reduce(npdata,npdataall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' npdata=',npdataall
  call mpi_reduce(nqdata,nqdataall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' nqdata=',nqdataall
  call mpi_reduce(npwdata,npwdataall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' npwdata=',npwdataall
  call mpi_reduce(nsprof,nsprofall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' number of overland retrieved sattem profiles (not used) =',nsprofall
  call mpi_reduce(ngdat8,ngdat8all,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' ngdat8=',ngdat8all
  call mpi_reduce(ngdat10,ngdat10all,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) print *,' ngdat10=',ngdat10all

!--------   summary of input errors by data type

  nreduce=300
  allocate(icountpall(300))
  call mpi_reduce(icountp,icountpall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  deallocate(icountp)
  allocate(erinpmaxall(300))
  call mpi_reduce(erinpmax,erinpmaxall,nreduce,mpi_real,mpi_max,0,my_comm,ierror)
  deallocate(erinpmax)
  allocate(erinpminall(300))
  call mpi_reduce(erinpmin,erinpminall,nreduce,mpi_real,mpi_min,0,my_comm,ierror)
  deallocate(erinpmin)
  if(mype.eq.0) then
     do itype=1,300
          if(icountpall(itype).gt.0) print *,' for type=',itype, &
             ' num=',icountpall(itype), &
            ' erinpmax,min=',erinpmaxall(itype),erinpminall(itype)
     end do
  end if
  deallocate(icountpall)
  deallocate(erinpmaxall)
  deallocate(erinpminall)
  call mpi_barrier(my_comm,ierror)

  allocate(icountqall(300))
  call mpi_reduce(icountq,icountqall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  deallocate(icountq)
  allocate(erinqmaxall(300))
  call mpi_reduce(erinqmax,erinqmaxall,nreduce,mpi_real,mpi_max,0,my_comm,ierror)
  deallocate(erinqmax)
  allocate(erinqminall(300))
  call mpi_reduce(erinqmin,erinqminall,nreduce,mpi_real,mpi_min,0,my_comm,ierror)
  deallocate(erinqmin)
  if(mype.eq.0) then
         do itype=1,300
          if(icountqall(itype).gt.0) print *,' for type=',itype, &
             ' num=',icountqall(itype), &
            ' erinqmax,min=',erinqmaxall(itype),erinqminall(itype)
         end do
  end if
  deallocate(icountqall)
  deallocate(erinqmaxall)
  deallocate(erinqminall)
  call mpi_barrier(my_comm,ierror)

  allocate(icounttall(300))
  call mpi_reduce(icountt,icounttall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  deallocate(icountt)
  allocate(erintmaxall(300))
  call mpi_reduce(erintmax,erintmaxall,nreduce,mpi_real,mpi_max,0,my_comm,ierror)
  deallocate(erintmax)
  allocate(erintminall(300))
  call mpi_reduce(erintmin,erintminall,nreduce,mpi_real,mpi_min,0,my_comm,ierror)
  deallocate(erintmin)
  if(mype.eq.0) then
         do itype=1,300
          if(icounttall(itype).gt.0) print *,' for type=',itype, &
             ' num=',icounttall(itype), &
            ' erintmax,min=',erintmaxall(itype),erintminall(itype)
         end do
  end if
  deallocate(icounttall)
  deallocate(erintmaxall)
  deallocate(erintminall)
  call mpi_barrier(my_comm,ierror)

  allocate(icountwall(300))
  call mpi_reduce(icountw,icountwall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  deallocate(icountw)
  allocate(erinwmaxall(300))
  call mpi_reduce(erinwmax,erinwmaxall,nreduce,mpi_real,mpi_max,0,my_comm,ierror)
  deallocate(erinwmax)
  allocate(erinwminall(300))
  call mpi_reduce(erinwmin,erinwminall,nreduce,mpi_real,mpi_min,0,my_comm,ierror)
  deallocate(erinwmin)
  if(mype.eq.0) then
         do itype=1,300
          if(icountwall(itype).gt.0) print *,' for type=',itype, &
             ' num=',icountwall(itype), &
            ' erinwmax,min=',erinwmaxall(itype),erinwminall(itype)
         end do
  end if
  deallocate(icountwall)
  deallocate(erinwmaxall)
  deallocate(erinwminall)
  call mpi_barrier(my_comm,ierror)

  allocate(icountpwall(300))
  call mpi_reduce(icountpw,icountpwall,nreduce,mpi_integer,mpi_sum,0,my_comm,ierror)
  deallocate(icountpw)
  allocate(erinpwmaxall(300))
  call mpi_reduce(erinpwmax,erinpwmaxall,nreduce,mpi_real,mpi_max,0,my_comm,ierror)
  deallocate(erinpwmax)
  allocate(erinpwminall(300))
  call mpi_reduce(erinpwmin,erinpwminall,nreduce,mpi_real,mpi_min,0,my_comm,ierror)
  deallocate(erinpwmin)
  if(mype.eq.0) then
         do itype=1,300
          if(icountpwall(itype).gt.0) print *,' for type=',itype, &
             ' num=',icountpwall(itype), &
            ' erinpwmax,min=',erinpwmaxall(itype),erinpwminall(itype)
         end do
  end if
  deallocate(icountpwall)
  deallocate(erinpwmaxall)
  deallocate(erinpwminall)
  call mpi_barrier(my_comm,ierror)
          call mpi_reduce(numssmi,numssmiall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
             if(mype.eq.0)  print *,' in rdtest, numssmi=',numssmiall
  if(drift) then
   call mpi_reduce(missing_level_latlon,missing_level_latlon_all,1,mpi_integer,mpi_sum,0, &
                                     my_comm,ierror)
   call mpi_reduce(distmax,distmax_all,1,mpi_real,mpi_max,0,my_comm,ierror)
   if(mype.eq.0) print *,' in rdtest, missing_level_latlon=',missing_level_latlon_all
   if(mype.eq.0) print *,' in rdtest, distmax=',distmax_all
  end if
  call mpi_reduce(vadqmmax,vadqmmax0,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(vadqmmin,vadqmmin0,1,mpi_real,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) print *,' in rdtest, vadqmmin,max=',vadqmmin0,vadqmmax0

  call mpi_reduce(itypemax,itypemax0,1,mpi_integer4,mpi_max,0,my_comm,ierror)
  call mpi_reduce(itypemin,itypemin0,1,mpi_integer4,mpi_min,0,my_comm,ierror)
     if(mype.eq.0) then
      print *,' min,max type in rdtest=',itypemin0,itypemax0
     end if
  call consolidate_vadqm(vadlon,vadlat,vadqm,nvad,mype,npes)

return
end subroutine rdtest
subroutine consolidate_vadqm(vadlon,vadlat,vadqm,nvad,mype,npes)

!  make common vad table across processors

  include 'mpif.h'
      include "my_comm.h"

  real(4) vadlon(500),vadlat(500),vadqm(500,60)

  integer(4) nvad0(npes)
  
  real(4),allocatable::vadlon0(:,:),vadlat0(:,:),vadqm0(:,:,:)
  logical new

  allocate(vadlon0(500,npes))
  allocate(vadlat0(500,npes))
  allocate(vadqm0(500,60,npes))
  call mpi_gather(nvad,1,mpi_integer4,nvad0,1,mpi_integer4,0,my_comm,ierror)
  call mpi_gather(vadlon,500,mpi_real4,vadlon0,500,mpi_real4,0,my_comm,ierror)
  call mpi_gather(vadlat,500,mpi_real4,vadlat0,500,mpi_real4,0,my_comm,ierror)
  call mpi_gather(vadqm,500*60,mpi_real4,vadqm0,500*60,mpi_real4,0,my_comm,ierror)

  if(mype.eq.0) then
   nvad=0
   vadlon=0.
   vadlat=0.
   vadqm=-99999.
   do n=1,npes
    if(nvad0(n).gt.0) then
     do i=1,nvad0(n)
      new=.true.
      if(nvad.gt.0) then
       do j=1,nvad
        if(abs(vadlon0(i,n)-vadlon(j)).lt..1.and.abs(vadlat0(i,n)-vadlat(j)).lt..1) then
         new=.false.
         do k=1,60
          vadqm(j,k)=max(vadqm(j,k),vadqm0(i,k,n))
         end do
        end if
       end do
      end if
      if(new) then
       nvad=nvad+1
       vadlon(nvad)=vadlon0(i,n)
       vadlat(nvad)=vadlat0(i,n)
       do k=1,60
        vadqm(nvad,k)=max(vadqm(nvad,k),vadqm0(i,k,n))
       end do
      end if
     end do
    end if
   end do
  end if
  deallocate(vadlon0)
  deallocate(vadlat0)
  deallocate(vadqm0)
  call mpi_bcast(vadlon,500,mpi_real4,0,my_comm,ierror)
  call mpi_bcast(vadlat,500,mpi_real4,0,my_comm,ierror)
  call mpi_bcast(vadqm,500*60,mpi_real4,0,my_comm,ierror)
  call mpi_bcast(nvad,1,mpi_integer4,0,my_comm,ierror)
  if(mype.eq.0) then
   print *,' vad table follows:'
     do i=1,nvad
      print '(" n,lat,lon,qm=",i4,2f9.2,3x,25i3)',i,vadlat(i),vadlon(i),(nint(vadqm(i,k)),k=1,25)
     end do
  end if

return
end subroutine consolidate_vadqm
subroutine increase_alldata(mbufdat,nbufdat)

  include 'types.h'

  type(general_obs),pointer::alldata(:)
  integer(2),pointer::lev_val(:),lev_max(:)
  common/databuf/lev_val,lev_max,alldata

  type(general_obs),allocatable::alldata0(:)
  integer(2),allocatable::lev_val0(:),lev_max0(:)

  allocate(alldata0(mbufdat))
  allocate(lev_val0(mbufdat))
  allocate(lev_max0(mbufdat))
  alldata0(1:mbufdat)=alldata(1:mbufdat)
  lev_val0(1:mbufdat)=lev_val(1:mbufdat)
  lev_max0(1:mbufdat)=lev_max(1:mbufdat)
  deallocate(alldata)
  deallocate(lev_val)
  deallocate(lev_max)
  nbufdat=nbufdat+10000
  allocate(alldata(nbufdat))
  allocate(lev_val(nbufdat))
  allocate(lev_max(nbufdat))
  alldata(1:mbufdat)=alldata0(1:mbufdat)
  lev_val(1:mbufdat)=lev_val0(1:mbufdat)
  lev_max(1:mbufdat)=lev_max0(1:mbufdat)
  deallocate(alldata0)
  deallocate(lev_val0)
  deallocate(lev_max0)
          write(0,*)' at 20 in increase_alldata, mbufdat,nbufdat=',mbufdat,nbufdat

return
end subroutine increase_alldata
subroutine increase_allrad(mbufrad,nbufrad)

  real(4),pointer::allrad(:,:)
  common/radbuf/allrad

  real(4),allocatable::allrad0(:,:)

  allocate(allrad0(47,mbufrad))
  do k=1,mbufrad
   allrad0(1:47,k)=allrad(1:47,k)
  end do
  deallocate(allrad)
  nbufrad=nbufrad+10000
  allocate(allrad(47,nbufrad))
  do k=1,mbufrad
   allrad(1:47,k)=allrad0(1:47,k)
  end do
  deallocate(allrad0)
          write(0,*)' at 20 in increase_allrad, mbufrad,nbufrad=',mbufrad,nbufrad

return
end subroutine increase_allrad
