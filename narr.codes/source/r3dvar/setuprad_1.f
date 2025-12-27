subroutine setuprad_1(rad_dat0,nrad_dat,mrad_dat,tgese,qgese,ozgese,pgese,pgesei,kpgesbot,w10ges, &
     sfct,sm,sice,sno,zsfcges,veg_type,veg_frac,soil_type,soil_temp,soil_moi,imeta,jmeta,nsig, &
     npred,mype,npes,nsat,sattype,isatid, &
     iordges,lbig2ges,lhalfges,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb, &
     myis2,myie2,myjs2,myje2,jppf,jpch,jpchus, &
     nusat,nuchan,iuse,varch0)


!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuprad_1  1st step in processing radiances
!   prgmmr: parrish         org: w/nmc22    date: 02-08-11
!
! abstract: read in data, reorganize, get guess, do initial qc
!
! program history log:
!   95-07-06  derber
!   96-11     wu  data from prepbufr file
!   96-12-    mcnally -changes for diagnostic file and bugfix
!   98-04-30  weiyu yang    mpi version
!   99-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   99-11-24  parrish,  modify extensively for use in eta 3dvar analysis
!   01-06-05  parrish,  bring eta version up to date with global version (noaa16 + new predictors,qc)
!   01-10-12  parrish,  bring eta version up to date with global version (Van Delst radiance + some qc mods)
!   02-08-11  parrish,  rearrange for improved efficiency
!
!   input argument list:
!     nrad_dat - size of rad_dat0, equal to max number of satellite observations,
!                   as counted up earlier during distribution to processors
!     tgese,qgese,ozgese - ges t,q,ozone from eta model
!                         (modified near surface and above top--see note)
!     pgese    - pressure at each ges grid point
!     pgesei   - pressure at each interface point
!     kpgesbot - k index of first layer above ground
!     w10ges   - ges 10m wind speed (on eta v grid)
!     sfct     - ges skin t
!     sm,sice,sno - land-sea mask, ice mask, snow depth for eta model
!     zsfcges  - eta model surface elevation (m)
!     veg_type - veg type
!     veg_frac - veg fraction
!     soil_type- soil type
!     soil_temp- soil temp (first layer)
!     soil_moi - soil moisture (first layer)
!     imeta,jmeta,nsig - grid dimensions for eta grid
!     npred    - number of predictors
!     mype     - current pe number
!     npes     _ total number of pes
!     nsat     - number of satellites
!     iout_rad -  unit number to write out some info about radiances processed
!     sattype  - list of satellite names
!     isatid   - internal numerical id of each satellite
!     nelesat  - max number of elements in each satellite report (currently bypassed--internally set to 45)
!     tlapmean -     for used in constructing
!     predx    -       bias correctors
!     iordges,lbig2ges,lhalfges - parameters for interpolation from eta grid to obs locations
!     wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb - more stuff for interpolation from eta grid to obs locations
!     myis2,myie2,myjs2,myje2 - still more interpolation stuff
!     jppf     -      max no. profiles to process at once
!     jpch     -      no. of channels*(number of sats)
!     jpchus   -      max. no. of channels used        ! tovs
!
!   output argument list:
!     rad_dat0  - radiance obs data structure
!     mrad_dat - number of radiance obs kept
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use type_kinds, only: fp_kind
  use error_handler
  use initialize
  use spectral_coefficients

! Declare include files

  include 'mpif.h'
      include "my_comm.h"
  include 'types.h'
  include 'types0.h'

! Declare input/output arrays

  type(rad_obs0) rad_dat0(max(1,nrad_dat))
  real(4) tgese(imeta*jmeta,nsig),qgese(imeta*jmeta,nsig)
  real(4) ozgese(imeta*jmeta,nsig),w10ges(imeta*jmeta)
  real(4) pgese(imeta,jmeta,nsig),pgesei(imeta,jmeta,nsig+1)
  integer(4) kpgesbot(imeta,jmeta)
  real(4) sfct(imeta*jmeta),sm(imeta*jmeta),sice(imeta,jmeta),sno(imeta*jmeta)
  real(4) zsfcges(imeta*jmeta)
  real(4),dimension(imeta*jmeta)::veg_type,veg_frac,soil_type,soil_temp,soil_moi
  integer(4) isatid(50)
  character(10) sattype(50)

! Declare local arrays

  integer(4) mype_rad
  real(8),allocatable::pred(:,:)
  real(8),allocatable::varinv(:,:),btm(:,:),tbcnob(:,:),tlapchn(:,:),emissav(:,:)
  real(8),allocatable::pemsout(:,:)
  real(8),allocatable::var(:),tnoise(:)
  real(8),allocatable::prsr5(:,:),temp5(:,:),wmix5(:,:),wmix5_lim(:,:)
  real(8),allocatable::omix5(:,:),pin5(:,:),qvp(:,:),tvp(:,:),poz(:,:)
  real(8),allocatable::ts5(:),soilt5(:),vegf5(:),vegtype5(:),soilm5(:),soiltype5(:),snow5(:)

!     xpath: secant of viewing path angle at surface
!     xpaths: secant of solar zenith angle at surface

  real(8),allocatable::xpath(:),xpaths(:),pangs(:),panglr(:),cld(:),cosza(:)
  real(8),allocatable::wsp10(:),stfact(:),zz(:),sfchgt(:),zasat(:)
  real(4),allocatable::cenlat(:),cenlon(:),cenlatg(:),cenlong(:)
  real(8),allocatable::pems5(:),ts(:),pems(:),ptbcl5(:)
  real(8),allocatable::wmix(:,:),temp(:,:),omix(:,:),ptau5(:,:)
  real(8),allocatable::htlto(:,:)
  real(8),allocatable::c1(:),c2(:),c3(:),c4(:)

!     varch: rms error for each channel
!     polar: channel polarization (0=vertical, 1=horizontal, or
!                                  0 to 1=mix of V and H)
!     nuchan: satellite channel numbers
!     nusat:  satellite numbers
!     itype:  0=IR channel, 1=microwave channel
!     wvnum: wavenumber in cm**-1

  real(4) varch0(jpch)
  real(8),allocatable::varch(:),polar(:),wvnum(:)

  integer(4),allocatable::kchan(:),kochan(:),kprof(:)
  integer(4),allocatable::indexn(:,:)
  integer(4),allocatable::isflg(:),icst(:)
  integer(4),allocatable::lndsea(:),nadir(:),knchan(:)
  integer,dimension(55):: kidsat,kidsat1,nsat1d
  integer(4) nusat(jpch),nuchan(jpch)
  integer(4),allocatable::itype(:),mchannel(:)
  integer(4) iuse(0:jpch)
  integer(4),allocatable::iouse(:)
  integer(4),allocatable::icx(:),iochan(:)

  character*10 obstype

  real(8),allocatable::pvp(:,:),pvpi(:,:)
  integer(4),allocatable::kpvpbot(:),iwgts(:,:)
  real(4),allocatable::wgts(:,:)
  integer(8),allocatable:: labelrad(:)
  integer(8) labelmax,labelmin
  integer(8),allocatable::labrad(:)
  real(8),allocatable::obsbt(:),gesbt(:),emissav0(:),tlapchn0(:)
  real(8),allocatable::gespems(:)


  logical hirs2,msu,goes,hirs3,amsua,amsub,airs,eos_amsua,hsb,goesimg
  logical,allocatable::kuse(:,:)
  logical,allocatable::keep(:),coast(:),land(:),ice(:)

  real(4),allocatable:: data_s(:,:)
  integer(4),allocatable::insane(:,:)
  integer(4),allocatable::insane0(:,:)
  integer(4),allocatable::itotal_in(:)
  integer(4),allocatable::itotal_in0(:)
  logical sice_missing

!   allocate all temporary space

  allocate(pred(npred+jpchus-1,jppf))
  allocate(varinv(jpchus,jppf))
  allocate(btm(jpchus,jppf))
  allocate(tbcnob(jpchus,jppf))
  allocate(pemsout(jpchus,jppf))
  allocate(tlapchn(jpchus,jppf))
  allocate(emissav(jpchus,jppf))
  allocate(var(jpchus))
  allocate(tnoise(jpchus))
  allocate(prsr5(nsig,jppf))
  allocate(temp5(nsig,jppf))
  allocate(wmix5(nsig,jppf))
  allocate(wmix5_lim(nsig,jppf))
  allocate(omix5(nsig,jppf))
  allocate(pin5(nsig,jppf))
  allocate(qvp(nsig,jppf))
  allocate(tvp(nsig,jppf))
  allocate(poz(nsig,jppf))
  allocate(ts5(jppf))
  allocate(soilt5(jppf))
  allocate(vegf5(jppf))
  allocate(vegtype5(jppf))
  allocate(soilm5(jppf))
  allocate(soiltype5(jppf))
  allocate(snow5(jppf))
  allocate(xpath(jppf))
  allocate(xpaths(jppf))
  allocate(pangs(jppf))
  allocate(panglr(jppf))
  allocate(cld(jppf))
  allocate(cosza(jppf))
  allocate(wsp10(jppf))
  allocate(stfact(jppf))
  allocate(zz(jppf))
  allocate(sfchgt(jppf))
  allocate(zasat(jppf))
  allocate(cenlat(jppf))
  allocate(cenlon(jppf))
  allocate(cenlatg(jppf))
  allocate(cenlong(jppf))
  allocate(pems5(jppf*jpchus))
  allocate(ts(jppf*jpchus))
  allocate(pems(jppf*jpchus))
  allocate(ptbcl5(jppf*jpchus))
  allocate(wmix(nsig,jpchus*jppf))
  allocate(temp(nsig,jpchus*jppf))
  allocate(omix(nsig,jpchus*jppf))
  allocate(ptau5(nsig,jpchus*jppf))
  allocate(htlto(3*nsig+1,jpchus))
  allocate(c1(nsig))
  allocate(c2(nsig))
  allocate(c3(nsig))
  allocate(c4(nsig))
  allocate(varch(jpch))
  allocate(polar(jpch))
  allocate(wvnum(jpch))
  allocate(kchan(jppf*jpchus))
  allocate(kochan(jppf*jpchus))
  allocate(kprof(jppf*jpchus))
  allocate(indexn(jpchus,jppf))
  allocate(isflg(jppf))
  allocate(icst(jppf))
  allocate(lndsea(jppf))
  allocate(nadir(jppf))
  allocate(knchan(jppf))
  allocate(itype(jpch))
  allocate(mchannel(jpch))
  allocate(iouse(0:jpch))
  allocate(icx(jpchus))
  allocate(iochan(jpchus))
  allocate(pvp(nsig,jppf))
  allocate(pvpi(nsig+1,jppf))
  allocate(kpvpbot(jppf))
  allocate(iwgts(lbig2ges,jppf))
  allocate(wgts(lbig2ges,jppf))
  allocate(labrad(jppf))
  allocate(obsbt(jpchus))
  allocate(gesbt(jpchus))
  allocate(gespems(jpchus))
  allocate(emissav0(jpchus))
  allocate(tlapchn0(jpchus))
  allocate(kuse(jpchus,jppf))
  allocate(keep(jppf))
  allocate(coast(jppf))
  allocate(land(jppf))
  allocate(ice(jppf))
  allocate(insane(jpchus,nsat))
  allocate(insane0(jpchus,nsat))
  allocate(itotal_in(jpch))
  allocate(itotal_in0(jpch))

  sicemax=0.
  do j=myjs2,myje2
   do i=myis2,myie2
    sicemax=max(sice(i,j),sicemax)
   end do
  end do
  call mpi_allreduce(sicemax,sicemaxall,1,mpi_real4,mpi_max,my_comm,ierr)
  sice_missing=.false.
  if(sicemaxall.lt..1) sice_missing=.true.
      if(sice_missing.and.mype.eq.0) then
           print *,' SEA ICE NOT DEFINED--ASSUME ICE NORTH OF 60N'
           print *,' SEA ICE NOT DEFINED--ASSUME ICE NORTH OF 60N'
           print *,' SEA ICE NOT DEFINED--ASSUME ICE NORTH OF 60N'
           print *,' SEA ICE NOT DEFINED--ASSUME ICE NORTH OF 60N'
           print *,' SEA ICE NOT DEFINED--ASSUME ICE NORTH OF 60N'
      end if

! Get satellite information

  mype_rad=0
  iuse(0)=-999
  call satinfo(nusat,nuchan,itype,iuse(1),mchannel,varch,polar,wavenumber, &
      frequency,jpch,mype,mype_rad)
  varch0=varch

!
!
! Initialize variables and constants.

       deltat=1.
  nsigx    = 3*nsig
  nsigx1   = 3*nsig+1
  dg2rad = atan(1.0)/45.0
  rad2dg = 1.0/dg2rad
  d1=.754
  d2=-2.265
  constoz  = 10.*604229. * 9.80665 * 21.4e-9       !  extra factor of 10, for mb instead of cb, i think
  imjmeta=imeta*jmeta
  iordcheck=1
  lbig2check=4
  lhalfcheck=1
!x!
!x! Scale sigma layer 1 wind by 10m wind factor.  Currently, only use
!x! 6hr guess winds.  These winds are used in the emissivity calculation.
!x! At a later date, obtain 10m guess wind from time interpolated 3,6,9
!x! hour surface fields
!x  do j = 1,lon1+2
!x    do i = 1,lat1+2
!x!ibm* prefetch_for_store(w10fact(i,j))
!x     w10fact(i,j)=fact10(i,j)*sqrt(ggrid_g31(1,i,j,1)**2+& ! u.
!x                                   ggrid_g31(2,i,j,1)**2) ! v.
!x    end do
!x  end do
  
  kidsat=0
  nsat1d = 0

!
!
!****************************************************************************
! MAIN LOOP OVER SATELLITES (ONE SATELLITE PLATFORM PER INPUT FILE)
!

  mrad_dat=0
  mradall=0
  insane=0
  itotal_in=0
  do is = 6,nsat+5
     isz=is-5
     obstype=sattype(isz)
     isat=isatid(isz)
     nele=45
     n1=0
!
!    Load data array for current satellite
     call count_rads(isz,nsat1)
     if(nsat1.le.0) go to 135

!    Initialize logical flags for satellite platform

     hirs2      = obstype == 'hirs/2' ; hirs3      = obstype == 'hirs/3'
     msu        = obstype == 'msu' ; goes       = obstype == 'goes'
     amsua      = obstype == 'amsua' ; amsub      = obstype == 'amsub'
     airs       = obstype == 'airs' ; hsb        = obstype == 'hsb'
     eos_amsua  = obstype == 'eos_amsua' ; goesimg    = obstype == 'goesimg'

     if(.not. (amsua     .or. amsub  .or. msu       .or. &
               hirs2     .or. hirs3  .or. goes      .or. &
               airs      .or. hsb    .or. eos_amsua .or. goesimg)) go to 135
!
!    Based on isat, set new satellite number
!
     kidsat(isz)=isat
     if(goes)kidsat(isz)=isat+50 ; if(msu)kidsat(isz)=isat+200
     if(goesimg)kidsat(isz)=isat+250 ; if(amsua)kidsat(isz)=isat+300
     if(eos_amsua)kidsat(isz)=isat+300 ; if(amsub)kidsat(isz)=isat+400
     if(hsb)kidsat(isz)=isat+400 ; if(airs)kidsat(isz)=isat+500

     nchan=0

     if (hirs2 .or. hirs3) nchan  = 19
     if (msu)nchan  = 4
     if (goes)nchan  = 18
     if (amsua .or. eos_amsua)nchan  = 15
     if (amsub)nchan  = 5
     if (airs) then
        if(mype.eq.0) print *,' airs data currently not set up'
        nchan  = 19
!       nchan  = 228
        go to 135
     end if
     if (hsb)nchan  = 5

     tnoise = 1.e10
     itoss = 1
     do jc=1,nchan
!
!    Load channel numbers into local array based on satellite type

        iochan(jc)=newchn(kidsat(isz),jc,nusat,nuchan,jpch)
!
!          Temporary fix for problems with NOAA 11 channel 9 coefficients
!          Use the coefficients from NOAA 14, channel 9.
!       if ( (kidsat(isz).eq.11) .and. (ichan(jc).eq.9) ) &    !??????? commented out in new operational
!            iochan(jc) = newchn(14,9,nusat,nuchan,jpch)       !??????? but do we need for reanalysis???
!
!    Set error instrument channels

        iouse(jc)=iuse(iochan(jc))
        if(iouse(jc) < -1) then
          tnoise(jc)=1.e10
        else
          tnoise(jc) = varch(iochan(jc))
        end if
        if (tnoise(jc).lt.1e4) itoss = 0
     end do
     if (itoss.eq.1) then
        if(mype.eq.0) write(6,*)'SETUPRAD:  all obs variance > 1e4.  do not use ',&
             'data from satellite is=',isat,obstype
        goto 135
     endif

     pred=0.0

!    Load data array for current satellite

     deallocate(data_s,stat=ierr)
     deallocate(labelrad,stat=ierr)
     allocate(labelrad(nsat1))
     allocate(data_s(nele,nsat1))
     call rdrads(data_s,nele,isz,nsat1,labelrad)
     nblktot=(nsat1-1)/jppf+1
 
!
!
!*****
!    PROCESS DATA IN BLOCKS OF JPPF

     do nblk = 1,nblktot

!
!        determine block of data to use

      nobs1  = 1 + (nblk-1)*jppf
      nobs2  = min(nobs1+jppf-1,nsat1)
      if (nobs2-nobs1+1 <= 0) then
        write(6,*)'setuprad:  nblk not consistent with nblktot ',nblk,nblktot
        goto 134
      end if

!
!        Initialize variables/arrays.

      tbcnob  = 0.
      emissav = 0.
      pems5 = 1.
      cld     = 0.
         

!    INITIAL PROCESSING OF SATELLITE DATA
!*****
!
!    Process data

     nsdata = 0
     do n = nobs1,nobs2

       nsdata = nsdata + 1

       if (nsdata>jppf .or. nsdata>nsat1) then
         write(6,*)' setuprad:  ***error*** nsdata,jppf,nsat1=', &
            nsdata,jppf,nsat1,is,n,nobs1,nobs2,' ',obstype,' ',&
            kidsat(isz)
         call mpi_abort(my_comm,100,ierror)
         stop
       end if

       labrad(nsdata)=labelrad(n)

!      Extract analysis relative observation time.
       stfact(nsdata) = data_s(2,n)/deltat
!  temporary
       if (airs .or. eos_amsua .or. hsb) stfact(n) = 0.
!      Extract lon and lat.
       cenlat(nsdata) = data_s(3,n)
       cenlon(nsdata) = data_s(4,n)
       cenlatg(nsdata) = data_s(44,n)
       cenlong(nsdata) = data_s(45,n)
       zasat(nsdata)  = data_s(5,n)
       cosza(nsdata)  = cos(zasat(nsdata))

       xpath(nsdata)     = 1./cosza(nsdata) ! view angle path factor

       if(goesimg)then
         panglr(nsdata) = 0.
         cld(nsdata) = data_s(6,n)
       else
         panglr(nsdata) = data_s(6,n)
       end if
!      Extract nadir (scan step position)
       nadir(nsdata) = nint(data_s(7,n))
       pangs(nsdata)  = data_s(8,n)

       if (pangs(nsdata).le. 89.) then
          xpaths(nsdata) = 1./cos(pangs(nsdata)*dg2rad) ! solar path factor
       else             ! if sun near/below horizon,
          xpaths(nsdata) = 1.0e6 ! set to very large value
       endif

       sfchgt(nsdata) = data_s(9,n)

!      Set land/sea flag.  (sea=0, land=1)

       lndsea(nsdata)=nint(data_s(10,n))


!          Load channel data into work array.
       do i = 1,nchan
          btm(i,nsdata) = data_s(i+10,n)
          ichanthis=newchn(kidsat(isz),i,nusat,nuchan,jpch)
          itotal_in(ichanthis)=itotal_in(ichanthis)+1
       end do

!    Construct predictors for 1B radiance bias correction.

       pred(1,nsdata) = 0.01
       pred(2,nsdata) = .1*(xpath(nsdata)-1.)**2-.015
       pred(3,nsdata) = 0.0

!       End of loop over obs
     end do
!

!
!
!  interpolate required guess variables to obs locations


     istaghglb=0 ; istagvglb=1
     call getradges(isz,obstype,sfct,w10ges,tgese,qgese,pgese,pgesei, &
                    kpgesbot,ozgese,zsfcges,veg_type,veg_frac,soil_type,soil_temp, &
                  soil_moi,sno,tvp,qvp,pvp,pvpi,poz,zz,ts5,wsp10,vegtype5,vegf5,soiltype5, &
                  soilt5,soilm5,snow5, &
                 cenlong,cenlatg,nsdata, &
                 iordges,lbig2ges,lhalfges,imeta,jmeta,nsig, &
                 iwgts,wgts,kpvpbot, &
                 wbglb,dlon0,sbglb,dlat0,istaghglb,istagvglb,imetaglb,jmetaglb)
     if (goes .or. airs .or. hsb .or. eos_amsua .or. goesimg) then
           do n = 1,nsdata
               sfchgt(n) = zz(n)
            end do
     endif

!
     icst  = 0
     isflg = 0
     keep = .true.
!
!    Do some prelimiary qc of the data.  Check for points covered
!        with ice/snow, land points, and coastal points.

     call coast_ice_check(lndsea,isflg,icst,sm,sice,sice_missing,sno, &
              cenlong,cenlatg,cenlat,nsdata, &
              iordcheck,lbig2check,lhalfcheck,imeta,jmeta, &
              wbglb,dlon0,sbglb,dlat0,istaghglb,imetaglb,jmetaglb)
     land = lndsea .eq. 1
     ice  = isflg .eq. 1
     coast= icst .eq. 1

     do n = 1,nsdata

!
!    Initial QC of data.  Set keep to false if we don't want the data.
!    The qc tests screen data over land, snow/ice, and coastal points.

!       Toss obs over land
        if (land(n)) then
           keep(n) = .false.
        end if
        if (ice(n)) then
!
!       Toss obs over snow or ice
           keep(n) = .false.
        end if
        if (coast(n)) then
!
!       Toss obs over coastal points
           keep(n) = .false.
        end if
        if (hirs2 .or. goes .or. hirs3) then
!
!    GROSS QC Check.  Compare HIRS and GOES channel 8 with sst.  If the
!    difference is too large, flag the observation as "bad".

           ch8flg = btm(8,n) - ts5(n)
           ch8ch10= btm(8,n) - btm(10,n)
           if(kidsat(isz) .eq. 11)ch8flg=ch8flg-2.19955+1.28578*ch8ch10
           if(kidsat(isz) .eq. 14)ch8flg=ch8flg-2.61089+1.32519*ch8ch10
           if(kidsat(isz) .eq. 15)ch8flg=ch8flg-1.85483+1.30573*ch8ch10
           if(kidsat(isz) .eq. 16)ch8flg=ch8flg-1.85483+1.30573*ch8ch10
           if(kidsat(isz) .eq. 58)ch8flg=ch8flg-2.09303+.277606*ch8ch10
           if(kidsat(isz) .eq. 60)ch8flg=ch8flg-4.22641+.331097*ch8ch10
           if (abs(ch8flg).gt.8.) then
              keep(n) = .false.
           endif
        endif
!
!    Now load/compute values for model levels

        snow5(n)=min(snow5(n),50._8)
        do l = 1,nsig
           pin5(l,n)  = pvpi(nsig-l+1,n)
           temp5(l,n) = tvp(nsig-l+1,n)/(1.+.61*qvp(nsig-l+1,n)) !????check if this is right
           sph             = max(0._8,qvp(nsig-l+1,n))
           wmix5(l,n) = 1000.*sph/(1.-sph)
           prsr5(l,n) = pvp(nsig-l+1,n)
           wmix5_lim(l,n)=wmix5(l,n)
           if(prsr5(l,n).lt.250.) wmix5_lim(l,n)=max(.01_8,wmix5(l,n))
           omix5(l,n) = max(0._8,poz(nsig-l+1,n)*604229.)
         end do
!
!
!    End loop over obs
     end do

!
!
!
!*****
!    IDENTIFY CHANNELS TO PROCESS
!*****
!     
!    Load arrays used by forward model to indicate which 
!    profiles and which channels to compute radiances for.
!
!    The tests below screen from the dataset those profiles/
!    channels which are not used (as of 3 Dec 01).
!
     kchan  = 0
     kochan = 0
     kprof  = 0
     knchan = 0
     varinv = 2.e-12
     kuse=.false.
!
!    1B HIRS and GOES:  retain all channels over water
!                over land, retain channels 2-5, 12.
!
     if (hirs2 .or. hirs3 .or. goes) then
       do j = 1,nsdata
         do jc = 1,nchan
           kuse(jc,j)=(keep(j) .or. jc.le.5 &
            .or. jc .eq. 12) .and. tnoise(jc).lt.1.e4
         end do
       end do
     elseif(msu .or. amsua .or. eos_amsua .or. amsub .or. hsb .or. goesimg)then
       do j = 1,nsdata
          do jc = 1,nchan
!
!    retain all data, provided assigned channel error
!           is "small"

           kuse(jc,j)= tnoise(jc) < 1.e4
          end do
       end do
     endif
     knchpf = 0
     indexn = 0
     do j = 1,nsdata
       kcount = 0
       do jc = 1,nchan
!        if(kuse(jc,j))then
            kcount  = kcount+1
            knchpf  = knchpf+1
            kchan(knchpf)  = iochan(jc)
            kochan(knchpf) = jc
            kprof(knchpf)  = j
            varinv(jc,j)     = 1.0/tnoise(jc)**2
            indexn(kcount,j) = knchpf
!        endif
       end do
       knchan(j) = kcount
!        if(kcount.ne.nchan) write(0,*)' in setuprad_1, mype,kcount,nchan=',mype,kcount,nchan
     end do
     if (knchpf.lt.1) go to 130

!
!    Compute surface emissivity for observation
!

     call emiss(knchpf,kprof,kchan,kochan,wsp10,zasat,panglr,&
          isflg,lndsea,emissav,pems5,ts5,soiltype5,soilt5, &
          soilm5,vegtype5,vegf5,snow5,polar,frequency,mchannel, &
          itype,nsdata,jppf,jpch,jpchus)

!
!
!
!    call radiative transfer (forward) model
!

     call rad_tran_k(pin5,prsr5,temp5,wmix5_lim,omix5,ts5,&
          pems5,xpath,xpaths,knchan,kchan,ptau5,ptbcl5, &
          temp,wmix,omix,ts,pems, &
          itype,nsig,jpch,knchpf,nsdata)

!   compute tlapchn, and load up tbcnob

     do i=1,knchpf
       n=kprof(i) ; m=kochan(i) ; mm=kchan(i)
       tbcnob(m,n) = ptbcl5(i)
       pemsout(m,n)= pems(i)
       tlapchn(m,n)= .01*(ptau5(nsig-1,i)-ptau5(nsig,i))* &
           (ts5(n)-temp5(nsig-1,n))
       do l=2,nsig-1
         tlapchn(m,n)=tlapchn(m,n)+&
             .01*(ptau5(l-1,i)-ptau5(l,i))*(temp5(l+1,n)-temp5(l-1,n))
       end do
     end do


!                      sanity check
          do k=1,knchpf
           if(btm(kochan(k),kprof(k)).lt.50..or.btm(kochan(k),kprof(k)).gt.450.) then
                    insane(kochan(k),isz)=insane(kochan(k),isz)+1
                    varinv(kochan(k),kprof(k))=2.e-12
           end if
          end do
!
!
!******
!     CONSTRUCT SENSITIVITY VECTORS.  WRITE TO OUTPUT FILE.
!******
!
!    Generate o3, t, q, and ts sensitivity arrays.

     do n = 1,nsdata
        icc = 0

        do i=1,nsig
           c1(i)=constoz/(pvpi(i,n)-pvpi(i+1,n))
           c2(i)=1./(1.+.61*qvp(i,n))
           c3(i)=1000./(1.-qvp(i,n))**2
           c4(i)=.61*tvp(i,n)*c2(i)*c2(i)
        end do
        do k = 1,knchan(n)
           kk = indexn(k,n)
           j  = kochan(kk)
           jo = kchan(kk)

           icc      = icc+1
           icx(icc) = jo
           var(icc) = varinv(j,n)
           obsbt(icc) = btm(j,n)
           gesbt(icc) = tbcnob(j,n)
           gespems(icc)=pemsout(j,n)
           tlapchn0(icc)=tlapchn(j,n)
           emissav0(icc)=emissav(j,n)

           do i = 1,nsig
             htlto(i,icc) = temp(nsig-i+1,kk)*c2(i)
             htlto(nsig+i,icc) = c3(i)*min(0._8,wmix(nsig-i+1,kk)) - c4(i)*temp(nsig-i+1,kk)
             htlto(2*nsig+i,icc) = omix(nsig-i+1,kk)*c1(i)
           end do
           htlto(nsigx+1,icc) = ts(kk)


!       End loop over channels.
        end do
!
!   Load data into output arrays
        if (icc>0) then
           n1  = n1+1
           mradall=mradall+1
           mrad_dat=min(mrad_dat+1,nrad_dat)

!          transfer data to output holding array

           ncc = icc
              if(ncc.ne.nchan) write(0,*)' in setuprad_1, mype,ncc,nchan=',mype,ncc,nchan
           rad_dat0(mrad_dat)%type=nint(data_s(1,nobs1+n-1))
           rad_dat0(mrad_dat)%group=nint(data_s(43,nobs1+n-1))
           rad_dat0(mrad_dat)%lon=cenlon(n)
           rad_dat0(mrad_dat)%lat=cenlat(n)
           rad_dat0(mrad_dat)%long=cenlong(n)
           rad_dat0(mrad_dat)%latg=cenlatg(n)
           rad_dat0(mrad_dat)%time=data_s(2,nobs1+n-1)
           rad_dat0(mrad_dat)%nsig=nsig
           rad_dat0(mrad_dat)%nsigx1=2*nsig+1
           rad_dat0(mrad_dat)%nadir=nadir(n)
           rad_dat0(mrad_dat)%ncc=ncc
           rad_dat0(mrad_dat)%npred=npred
           rad_dat0(mrad_dat)%label=labrad(n)
           rad_dat0(mrad_dat)%kpbot=kpvpbot(n)
           rad_dat0(mrad_dat)%obstype=obstype
           rad_dat0(mrad_dat)%isat=isat
           rad_dat0(mrad_dat)%isz=isz
           rad_dat0(mrad_dat)%nele=nele
           rad_dat0(mrad_dat)%zasat=zasat(n)
           rad_dat0(mrad_dat)%panglr=panglr(n)
           rad_dat0(mrad_dat)%cld=cld(n)
           rad_dat0(mrad_dat)%pangs=pangs(n)
           rad_dat0(mrad_dat)%xpaths=xpaths(n)
           rad_dat0(mrad_dat)%sfchgt=sfchgt(n)
           rad_dat0(mrad_dat)%zz=zz(n)
           rad_dat0(mrad_dat)%lndsea=lndsea(n)
           rad_dat0(mrad_dat)%isflg=isflg(n)
           rad_dat0(mrad_dat)%icst=icst(n)
           rad_dat0(mrad_dat)%ts5=ts5(n)
           labelmax=max(labrad(n),labelmax)
           labelmin=min(labrad(n),labelmin)

           rad_dat0(mrad_dat)%icx(1:ncc)=icx(1:ncc)
           rad_dat0(mrad_dat)%var(1:ncc)=var(1:ncc)
           rad_dat0(mrad_dat)%pred(1:npred+ncc-1)=pred(1:npred+ncc-1,n)
           rad_dat0(mrad_dat)%obsbt(1:ncc)=obsbt(1:ncc)
           rad_dat0(mrad_dat)%gesbt(1:ncc)=gesbt(1:ncc)
           rad_dat0(mrad_dat)%emissav(1:ncc)=emissav0(1:ncc)
           rad_dat0(mrad_dat)%tlapchn(1:ncc)=tlapchn0(1:ncc)
           rad_dat0(mrad_dat)%pems(1:ncc)=gespems(1:ncc)

           rad_dat0(mrad_dat)%pressure(:nsig)=pvp(:nsig,n)
           rad_dat0(mrad_dat)%iwgts(:)=iwgts(:,n)
           rad_dat0(mrad_dat)%wgts(:)=wgts(:,n)
           rad_dat0(mrad_dat)%htlto(:nsigx1,:ncc)=htlto(:nsigx1,:ncc)
!
        end if
!
!    End of loop over profiles
     end do

!
!
!    End of nblk loop
130  continue
     end do
!
!
!    Jump here when there is no more data to process for current satellite
 134 continue
     deallocate(data_s)
 135 continue
     nsat1d(is)=n1
!
!
!
!*****
!    END MAIN LOOP OVER SATELLITES
!*****

  end do

!     deallocate all temporary space

  deallocate(pred)
  deallocate(varinv)
  deallocate(btm)
  deallocate(tbcnob)
  deallocate(pemsout)
  deallocate(tlapchn)
  deallocate(emissav)
  deallocate(var)
  deallocate(tnoise)
  deallocate(prsr5)
  deallocate(temp5)
  deallocate(wmix5)
  deallocate(wmix5_lim)
  deallocate(omix5)
  deallocate(pin5)
  deallocate(qvp)
  deallocate(tvp)
  deallocate(poz)
  deallocate(ts5)
  deallocate(soilt5)
  deallocate(vegf5)
  deallocate(vegtype5)
  deallocate(soilm5)
  deallocate(soiltype5)
  deallocate(snow5)
  deallocate(xpath)
  deallocate(xpaths)
  deallocate(pangs)
  deallocate(panglr)
  deallocate(cld)
  deallocate(cosza)
  deallocate(wsp10)
  deallocate(stfact)
  deallocate(zz)
  deallocate(sfchgt)
  deallocate(zasat)
  deallocate(cenlat)
  deallocate(cenlon)
  deallocate(cenlatg)
  deallocate(cenlong)
  deallocate(pems5)
  deallocate(ts)
  deallocate(pems)
  deallocate(ptbcl5)
  deallocate(wmix)
  deallocate(temp)
  deallocate(omix)
  deallocate(ptau5)
  deallocate(htlto)
  deallocate(c1)
  deallocate(c2)
  deallocate(c3)
  deallocate(c4)
  deallocate(varch)
  deallocate(polar)
  deallocate(wvnum)
  deallocate(kchan)
  deallocate(kochan)
  deallocate(kprof)
  deallocate(indexn)
  deallocate(isflg)
  deallocate(icst)
  deallocate(lndsea)
  deallocate(nadir)
  deallocate(knchan)
  deallocate(itype)
  deallocate(mchannel)
  deallocate(iouse)
  deallocate(icx)
  deallocate(iochan)
  deallocate(pvp)
  deallocate(pvpi)
  deallocate(kpvpbot)
  deallocate(iwgts)
  deallocate(wgts)
  deallocate(labrad)
  deallocate(obsbt)
  deallocate(gesbt)
  deallocate(gespems)
  deallocate(emissav0)
  deallocate(tlapchn0)
  deallocate(kuse)
  deallocate(keep)
  deallocate(coast)
  deallocate(land)
  deallocate(ice)
  deallocate(insane)
  deallocate(insane0)
  deallocate(itotal_in)
  deallocate(itotal_in0)
        write(0,*)' mype,mradall,nrad_dat=',mype,mradall,nrad_dat

return
end subroutine setuprad_1
