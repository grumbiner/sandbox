      program gribmain
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
c --- hycom/micom to 3-d z-level diagnostic field extractor
c
      real,    allocatable, dimension (:)     ::
     &   zz
      integer, allocatable, dimension (:,:)   ::
     &   kpu,kpv
      real,    allocatable, dimension (:,:)   ::
     &   uflux,vflux, depthu,depthv,dpu,dpv, util1,work
      real,    allocatable, dimension (:,:,:) ::
     &   utilz,utilk,utilk1,pw
c
      common/conrng/ amn,amx
c
      character flnm*240,frmt*80
      logical   ltheta,smooth,icegln,lperiod
c
      logical plot4(4)
      real    qq4(4),qc4(4)
c
      integer          artype,iexpt,iversn,kkin,yrflag,mxlflg
      double precision time3(3)
c
      real, parameter :: flag = 2.0**100
c
c --- 'lhycom' -- hycom (vs micom) input file
c --- 'trcout' -- tracer input
      logical   lhycom,trcout
      data      lhycom/.true. /, trcout/.false./
c
      character cmonth(12)*3
      data      cmonth/'jan','feb','mar','apr','may','jun',
     &                 'jul','aug','sep','oct','nov','dec'/
c
      real      tenm,onem,temcm,onecm,onemm
      data      tenm/10./,onem/1./,tencm/.1/,onecm/.01/,onemm/.001/
c
      logical   initl
      data      initl /.true. /
      real      thref,spcifh
      data      thref/1.e-3/,spcifh/3990./
      character blank*40
      data      blank/'                                        '/
C
C     Variables for gribbing
      INCLUDE "grib_parms.h"
      LOGICAL doing_grib
      INTEGER l, ierr, iogrbin
      CHARACTER *1 grib  !dummy here, needed for grib file opening
      CHARACTER*80 fname
      INTEGER identity
c
      call xcspmd
      call zaiost
      lp=6
c
c --- read model data
c ---   'flnm  ' = name of file containing the actual data
c ---   'frmt  ' = output format or type(HYCOM, BIN, BINDEP,netCDF,REVERSE)
c ---                see horout for more details on frmt
c ---   'iexpt ' = experiment number x10  (000=from archive file)
c ---   'yrflag' = days in year flag (0=360J16,1=366J16,2=366J01,3=actual)
c ---   'idm   ' = longitudinal array size
c ---   'jdm   ' = latitudinal  array size
c ---   'kdm   ' = number of layers
        read (*,'(a)') flnm
        write (lp,'(2a)') ' input file: ',trim(flnm)
        call flush(lp)
        read (*,'(a)') frmt
        write (lp,'(2a)') 'output type: ',trim(frmt)
        call flush(lp)
        call blkini(iexpt, 'iexpt ')
        call blkini(yrflag,'yrflag')
        call blkini(ii,    'idm   ')
        call blkini(jj,    'jdm   ')
        call blkini(kk,    'kdm   ')
        if     (ii.ne.idm .or. jj.ne.jdm) then
          write(lp,*)
          write(lp,*) 'error - wrong idm or jdm (should be:',
     &                                           idm,jdm,')'
          write(lp,*)
          call flush(lp)
          stop
        endif
c
c ---   'thbase' = reference density (sigma units)
        call blkinr(thbase,
     &             'thbase','("blkinr: ",a6," =",f11.4," sig")')
c
c ---   'smooth' = smooth the layered fields
        call blkinl(smooth,'smooth')
c
c ---   'iorign' = i-origin of sampled subregion
c ---   'jorign' = j-origin of sampled subregion
c ---   'idmp  ' = i-extent of sampled subregion (<=idm; 0 implies idm)
c ---   'jdmp  ' = j-extent of sampled subregion (<=jdm; 0 implies jdm)
        call blkini(iorign,'iorign')
        call blkini(jorign,'jorign')
        call blkini(ii,    'idmp  ')
        call blkini(jj,    'jdmp  ')
        if     (ii.eq.0) then
          ii=idm
        endif
        if     (jj.eq.0) then
          jj=jdm
        endif
c ---   'iorign,jorign' denote the origin of the subgrid to be extracted 
c ---   from the full history grid (dimensioned idm x jdm). 
c ---   The size of the subgrid is determined by ii,jj.
        write (lp,'(/ 2(a,i5),9x,2(a,i5) /)') 'extracting i =',iorign,
     &    ' ...',iorign+ii-1,'j =',jorign,' ...',jorign+jj-1
        call flush(lp)
c
c --- 'itype ' = interpolation type (0=sample,1=linear)
      call blkini(itype,'itype ')
      if     (itype.lt.0 .or. itype.gt.1) then
        write(lp,*)
        write(lp,*) 'error - unknown itype'
        write(lp,*)
        call flush(lp)
        stop
      endif
c
c --- 'kz    ' = number of depths to sample
      call blkini(kz,'kz    ')
      allocate( zz(kz) )
      do k= 1,kz
c ---   'z     ' = sample depth
        call blkinr(zz(k),
     &             'z     ','("blkinr: ",a6," =",f11.4," m")')
        if     (k.gt.1 .and. zz(k).le.zz(k-1)) then
          write(lp,*)
          write(lp,*) 'error - current z shallower than last z'
          write(lp,*)
          stop
        endif
      enddo
      write(lp,*)
      call flush(lp)
c
c --- 'grbio ' = grib unit no     I/O unit (0 no I/O)
c --- 'botio ' = bathymetry       I/O unit (0 no I/O)
c --- 'mltio ' = mix. lay. thick. I/O unit (0 no I/O)
c --- 'infio ' = interface depths I/O unit (0 no I/O, <0 label with layer #)
c --- 'wviio ' = intf. w-velocity I/O unit (0 no I/O)
c --- 'wvlio ' = w-velocity       I/O unit (0 no I/O)
c --- 'uvlio ' = u-velocity       I/O unit (0 no I/O)
c --- 'vvlio ' = v-velocity       I/O unit (0 no I/O)
c --- 'splio ' = speed            I/O unit (0 no I/O)
c --- 'temio ' = temperature      I/O unit (0 no I/O)
c --- 'salio ' = salinity         I/O unit (0 no I/O)
c --- 'tthio ' = density          I/O unit (0 no I/O)
      call blkini(iobotin,'botio ')
      call blkini(iomltin,'mltio ')
      call blkini(ioinfin,'infio ')
      call blkini(iowviin,'wviio ')
      call blkini(iowvlin,'wvlio ')
      call blkini(iouvlin,'uvlio ')
      call blkini(iovvlin,'vvlio ')
      call blkini(iosplin,'splio ')
      call blkini(iotemin,'temio ')
      call blkini(iosalin,'salio ')
      call blkini(iotthin,'tthio ')
      call blkini(iogrbin,'grbio ')
c
      if (lhycom) then
        call getartype(flnm,artype)
      else
        artype=1
      endif
      if     (artype.eq.2) then  ! mean archive
c ---   'keio  ' = kinetic energy I/O unit (0 no I/O)
        call blkini(iokein, 'keio  ')
      elseif (artype.eq.3) then  ! std. archive
        write(lp,*)
        write(lp,*) 'error - std.dev. archives are not supported'
        write(lp,*)
        stop
      endif
c
c --- array allocation
c
      call plot_alloc
c
      allocate(  uflux(ii,jj) )
      allocate(  vflux(ii,jj) )
      allocate(  util1(ii,jj) )
      allocate(   work(ii,jj) )
      if     (iowviin.ne.0 .or. iowvlin.ne.0) then
        allocate( depthu(ii,jj) )
        allocate( depthv(ii,jj) )
        allocate(    dpu(ii,jj) )
        allocate(    dpv(ii,jj) )
      endif
c
      if     (iowvlin.ne.0) then
        allocate(    kpu(ii,jj) )
        allocate(    kpv(ii,jj) )
      endif
c
      if     (iowviin.ne.0) then
        allocate(     pw(ii,jj,kk) )
      endif
c
      allocate(  utilk(ii,jj,kk+1) )
      allocate(  utilk1(ii,jj,kk+1) )
      allocate(  utilz(ii,jj,kz) )
c
      dpthfil = 'regional.depth'
c
      do j=1,jj
        do i=1,ii
          p(i,j,1)=0.
        enddo
      enddo
c
c --- read the archive file.
c
      if (lhycom) then
        call getdat(flnm,time3,artype,initl,icegln,trcout,
     &              iexpt,iversn,yrflag,kkin)       ! hycom input
        if (kkin.ne.kk) then
          write(lp,*)
          write(lp,*) 'error - kkin must be kdm'
          write(lp,*)
          stop
        endif
      else
        call getdtm(flnm,time,initl, thbase)        ! micom input
        artype = 1
        iversn = 10
      endif
c
      if     (yrflag.eq.0) then
        year  = 360.0d0
      elseif (yrflag.lt.3) then
        year  = 366.0d0
      else
        year  = 365.25d0
      endif
c
c --- define grid scale
      write(lp,'(/a,2f8.2/a,2f8.2)') 
     &     'sub-domain longitude range = ',
     &    minval(plon(:,:)),maxval(plon(:,:)),
     &     'sub-domain latitude  range = ',
     &    minval(plat(:,:)),maxval(plat(:,:))
c
      lperiod = maxval(plon(:,:))-minval(plon(:,:)) .gt. 350.0
      if     (lperiod) then
        write(lp,'(/a/)') 'sub-domain assumed to be periodic'
      else
        write(lp,'(/a/)') 'sub-domain assumed to be non-periodic'
      endif
c
      call bigrid(depths)
      call flush(lp)
c
c --- check that bathymetry is consistent with this archive.
c --- only possible with hycom .[ab] file input.
c
      if     (iversn.ge.20) then
        ibad = 0
        do j= 1,jj
          do i= 1,ii
            if     (ip(i,j).eq.1) then
              if     (srfht(i,j).gt.2.0**99) then
                ibad = ibad + 1   ! topo sea, srfht land
              endif
            else
              if     (srfht(i,j).lt.2.0**99) then
                ibad = ibad + 1   ! topo land, srfht sea
              endif
            endif
          enddo !i
        enddo !j
        if     (ibad.ne.0) then
          write(lp,*)
          write(lp,*) 'error - wrong bathymetry for this archive file'
          write(lp,*) 'number of mismatches = ',ibad
          write(lp,*)
          call flush(lp)
          stop
        endif !ibad.ne.0
      endif !iversn.ge.20
c
      do 3 k=1,kkin
      do 3 j=1,jj
      do 3 i=1,ii
c
c --- convert baroclinic to total velocities by adding barotropic component
      if     (iu(i,j).eq.1 .and. artype.eq.1) then
        u(i,j,2*k)=u(i,j,2*k)+ubaro(i,j)
      elseif (iu(i,j).ne.1) then
        u(i,j,2*k)=0.
      end if
      if     (iv(i,j).eq.1 .and. artype.eq.1) then
        v(i,j,2*k)=v(i,j,2*k)+vbaro(i,j)
      elseif (iv(i,j).ne.1) then
        v(i,j,2*k)=0.
      end if
c
c --- convert layer thickness to meters
      if (depths(i,j).gt.0.) then
        dp(i,j,k)=dp(i,j,k)/9806.
        p(i,j,k+1)=p(i,j,k)+dp(i,j,k)
        th3d(i,j,2*k)=th3d(i,j,2*k)+thbase
      else
        saln(i,j,2*k)=flag
        temp(i,j,2*k)=flag
        th3d(i,j,2*k)=flag
        ke(  i,j,2*k)=flag
        dp(i,j,k)=flag
        p(i,j,k+1)=flag
      endif
 3    continue
c
c --- z-level vertical velocity from continuity equation
      if     (iowvlin.ne.0) then
      do j= 1,jj
        do i= 1,ii
          depthu(i,j) = min(depths(i,j),depths(i-1,j))  ! depths(0,j) is ok
          depthv(i,j) = min(depths(i,j),depths(i,j-1))  ! depths(i,0) is ok
c
          kpu(i,j)  = 1  ! surface is in layer 1
          kpv(i,j)  = 1  ! surface is in layer 1
        enddo
      enddo
      do j= 1,jj-1
        do i= 1,ii-1
          if     (ip(i,j).eq.1) then
            pwk = (ubaro(i+1,j)*depthu(i+1,j)-
     &             ubaro(i  ,j)*depthu(i,  j) )/scpx(i,j) +
     &            (vbaro(i,j+1)*depthv(i,j+1)-
     &             vbaro(i,j  )*depthv(i,j  ) )/scpy(i,j)
            util1(i,j) = pwk
          endif
        enddo
      enddo
      if     (zz(1).gt.0.0) then
        call layer2w(u,v,p,depthu,depthv,scpx,scpy,ip,iu,iv,
     &               kpu,kpv,ii,jj,kk, uflux,vflux,
     &               0.0,zz(1),flag, utilz(1,1,1))
        do j= 1,jj
          do i= 1,ii
            if     (utilz(i,j,1).ne.flag) then
              utilz(i,j,1) = 86400.0*
     &                       (utilz(i,j,1) - 
     &                        util1(i,j)*
     &                         min(zz(1),depths(i,j))/depths(i,j))
            endif
          enddo
        enddo
      else  ! zz(1)==0
        do j= 1,jj
          do i= 1,ii
            utilz(i,j,1) = 0.0
          enddo
        enddo
      endif
      do k= 2,kz
        call layer2w(u,v,p,depthu,depthv,scpx,scpy,ip,iu,iv,
     &               kpu,kpv,ii,jj,kk, uflux,vflux,
     &               zz(k-1),zz(k),flag, utilz(1,1,k))
        do j= 1,jj
          do i= 1,ii
            if     (utilz(i,j,k).ne.flag) then
              utilz(i,j,k) = utilz(i,j,k-1) + 
     &                       86400.0*
     &                       (utilz(i,j,k) - 
     &                        util1(i,j)*
     &                        (min(zz(k),  depths(i,j))-
     &                         min(zz(k-1),depths(i,j)) )/depths(i,j))
            endif
          enddo
        enddo
      enddo
      if     (smooth) then
        do k= 1,kz
          call psmoo(utilz(1,1,k),work)
        enddo
      endif
      identity = 0 ! 0 -> non-grib variable, currently RG
      call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,lhycom,
     &              ' w-veloc.',                ! plot name
     &              'w_velocity',               ! ncdf name
     &              'm/day',                    ! units
     &              kz, frmt,iowvlin, identity)
      endif !iowvlin
c
c --- interface vertical velocity from continuity equation
      if     (iowviin.ne.0) then
      do j= 1,jj
        do i= 1,ii
          depthu(i,j) = min(depths(i,j),depths(i-1,j))  ! depths(0,j) is ok
          depthv(i,j) = min(depths(i,j),depths(i,j-1))  ! depths(i,0) is ok
        enddo
      enddo
      do j= 1,jj-1
        do i= 1,ii-1
          if     (ip(i,j).eq.1) then
            pwk = (ubaro(i+1,j)*depthu(i+1,j)-
     &             ubaro(i  ,j)*depthu(i,  j) )/scpx(i,j) +
     &            (vbaro(i,j+1)*depthv(i,j+1)-
     &             vbaro(i,j  )*depthv(i,j  ) )/scpy(i,j)
            pw(i,j,kkin) = pwk
          endif
        enddo
      enddo
      do k= 1,kkin
        do j= 1,jj
          do i= 1,ii
            if     (iu(i,j).eq.1 .and. i.gt.1) then
              dpu(i,j)=max(0.,
     &           min(depthu(i,j),.5*(p(i,j,k+1)+p(i-1,j,k+1)))-
     &           min(depthu(i,j),.5*(p(i,j,k  )+p(i-1,j,k  ))))
            else
              dpu(i,j) = 0.0
            endif
            if     (iv(i,j).eq.1 .and. j.gt.1) then
              dpv(i,j)=max(0.,
     &             min(depthv(i,j),.5*(p(i,j,k+1)+p(i,j-1,k+1)))-
     &             min(depthv(i,j),.5*(p(i,j,k  )+p(i,j-1,k  ))))
            else
              dpv(i,j) = 0.0
            endif
          enddo
        enddo
        do j= 2,jj-1
          pw(1, j,k) = flag
          do i= 2,ii-1
            if     (ip(i,j).eq.1) then
              pwk = (u(i+1,j,2*k)*dpu(i+1,j)-
     &               u(i  ,j,2*k)*dpu(i,  j) )/scpx(i,j) +
     &              (v(i,j+1,2*k)*dpv(i,j+1)-
     &               v(i,j  ,2*k)*dpv(i,j  ) )/scpy(i,j)
              if     (k.eq.1) then
                pw(i,j,k) =               pwk -
     &                      pw(i,j,kkin)*dp(i,j,k)/depths(i,j)
              else
                pw(i,j,k) = pw(i,j,k-1) + pwk -
     &                      pw(i,j,kkin)*dp(i,j,k)/depths(i,j)
              endif
            else
              pw(i,j,k) = flag
            endif
          enddo
          pw(ii,j,k) = flag
        enddo
        do i= 1,ii
          pw(i, 1,k) = flag
          pw(i,jj,k) = flag
        enddo
      enddo
      if     (smooth) then
        do k= 1,kkin
          call psmoo(pw(1,1,k),work)
        enddo
      endif
      endif !iowviin
c
      do 7 j=1,jj
      do 7 i=1,ii
      if (depths(i,j).gt.0.) then
        dpmixl(i,j)=dpmixl(i,j)/9806.  ! m
      else
        dpmixl(i,j)=flag
      end if
 7    continue
c
      dpth=0.5*onecm
c
c --- put vertically averaged t,s values into massless layers
c
      do 70 k=2,kkin
c
      do 70 j=1,jj
      do 70 i=1,ii
c
      if (depths(i,j).gt.0.) then
        temp(i,j,2*k-1)=0.
        saln(i,j,2*k-1)=0.
        th3d(i,j,2*k-1)=0.
        pmid=.5*(p(i,j,k)+p(i,j,k+1))
        phi=pmid+dpth
        plo=pmid-dpth
c
        sum=0.
        do 71 k1=1,kkin
        delp=max(0.,min(p(i,j,k1+1),phi)-max(p(i,j,k1),plo))
        sum=sum+delp
        temp(i,j,2*k-1)=temp(i,j,2*k-1)+temp(i,j,2*k1)*delp
        saln(i,j,2*k-1)=saln(i,j,2*k-1)+saln(i,j,2*k1)*delp
 71     th3d(i,j,2*k-1)=th3d(i,j,2*k-1)+th3d(i,j,2*k1)*delp
c
        temp(i,j,2*k)=temp(i,j,2*k-1)/sum
        saln(i,j,2*k)=saln(i,j,2*k-1)/sum
        th3d(i,j,2*k)=th3d(i,j,2*k-1)/sum
      end if
 70   continue
c
      if (smooth) then
c
c --- smooth mass field variables
c
      call psmoo(temp(1,1,2),work)
      call psmoo(saln(1,1,2),work)
      call psmoo(th3d(1,1,2),work)
c
      do 38 k=2,kkin
c
      do 76 j=1,jj
      do 76 i=1,ii
      if (depths(i,j).gt.0.) then
        util1(i,j)=max(onemm,dp(i,j,k))
        temp(i,j,2*k-1)=temp(i,j,2*k)*util1(i,j)
        saln(i,j,2*k-1)=saln(i,j,2*k)*util1(i,j)
        th3d(i,j,2*k-1)=th3d(i,j,2*k)*util1(i,j)
      else
        temp(i,j,2*k-1)=flag
        saln(i,j,2*k-1)=flag
        th3d(i,j,2*k-1)=flag
      end if
 76   continue
c
      call psmoo(util1,work)
      call psmoo(temp(1,1,2*k-1),work)
      call psmoo(saln(1,1,2*k-1),work)
      call psmoo(th3d(1,1,2*k-1),work)
c
      do 38 j=1,jj
      do 38 i=1,ii
      if (depths(i,j).gt.0.) then
        temp(i,j,2*k)=temp(i,j,2*k-1)/util1(i,j)
        saln(i,j,2*k)=saln(i,j,2*k-1)/util1(i,j)
        th3d(i,j,2*k)=th3d(i,j,2*k-1)/util1(i,j)
      end if
 38   continue
c
c --- smooth velocity and layer thickness fields
c
      do 30 k=1,kkin
c
      do 31 j=1,jj1
      do 31 i=2,ii1
 31   uflux(i,j)=u(i,j,2*k)*max(onecm,dp(i,j,k)+dp(i-1,j,k))
c
      do 32 j=2,jj1
      do 32 i=1,ii1
 32   vflux(i,j)=v(i,j,2*k)*max(onecm,dp(i,j,k)+dp(i,j-1,k))
c
      call usmoo(uflux,work)
      call vsmoo(vflux,work)
      call psmoo(dp(1,1,k),work)
c --- (warning: smoothed -dp- field unsuitable for deriving interface depths)
c
      do 33 j=1,jj1
      do 33 i=2,ii1
 33   u(i,j,2*k)=uflux(i,j)/max(onecm,dp(i,j,k)+dp(i-1,j,k))
c
      do 34 j=2,jj1
      do 34 i=1,ii1
 34   v(i,j,2*k)=vflux(i,j)/max(onecm,dp(i,j,k)+dp(i,j-1,k))
c
c --- now smooth layer interfaces and find corresponding -dp- field
      if (k.lt.kkin) call psmo1(p(1,1,k+1),work,p(1,1,kk+1))
c --- now smooth mixed layer base
      if (k.eq.1) then
        call psmo1(dpmixl,work,p(1,1,kk+1))
      end if
c
      do 35 j=1,jj1
      do 35 i=1,ii1
      if (depths(i,j).gt.0.) dp(i,j,k)=p(i,j,k+1)-p(i,j,k)
 35   continue
c
 30   continue
c
      end if			!  smooth = .true.
c
c --- put vertically averaged u,v values into massless layers
c
      do 74 k=2,kkin
c
      do 72 j=1,jj
      do 72 i=2,ii
      if (min(depths(i,j),depths(i-1,j)).gt.0.) then
c
        u(i,j,2*k-1)=0.
        pmid=.25*(p(i,j,k)+p(i-1,j,k)+p(i,j,k+1)+p(i-1,j,k+1))
        plo=pmid-dpth
        phi=pmid+dpth
c
        sum=0.
        do 73 k1=1,kkin
        delp=max(0.,min(.5*(p(i,j,k1+1)+p(i-1,j,k1+1)),phi)
     &             -max(.5*(p(i,j,k1  )+p(i-1,j,k1  )),plo))
        sum=sum+delp
 73     u(i,j,2*k-1)=u(i,j,2*k-1)+u(i,j,2*k1)*delp
c
        u(i,j,2*k)=u(i,j,2*k-1)/sum
      endif
 72   continue
c
      do 74 j=2,jj
      do 74 i=1,ii
      if (min(depths(i,j),depths(i,j-1)).gt.0.) then
c
        v(i,j,2*k-1)=0.
        pmid=.25*(p(i,j,k)+p(i,j-1,k)+p(i,j,k+1)+p(i,j-1,k+1))
        plo=pmid-dpth
        phi=pmid+dpth
c
        sum=0.
        do 75 k1=1,kkin
        delp=max(0.,min(.5*(p(i,j,k1+1)+p(i,j-1,k1+1)),phi)
     &             -max(.5*(p(i,j,k1  )+p(i,j-1,k1  )),plo))
        sum=sum+delp
 75     v(i,j,2*k-1)=v(i,j,2*k-1)+v(i,j,2*k1)*delp
c
        v(i,j,2*k)=v(i,j,2*k-1)/sum
      endif
 74   continue
C --- ------------------------------------------------------------------
C  Begin grib-able output section
C --- ------------------------------------------------------------------
      l = len_trim(frmt)
      IF (frmt(1:l) .EQ. 'GRIB') THEN
        doing_grib = .TRUE.
        WRITE (fname, 9002) iogrbin
        ierr = bacio(BAOPEN_WONLY, start, newpos, SIZEOF_CHARACTER, 0,
     1                   nactual, fdes, fname, grib)
        IF (ierr .NE. 0) THEN
          PRINT *,'Failed to open the grib file, unit = ',iogrbin
          STOP
        ENDIF
 9002   FORMAT ("fort.",I2)
      ELSE
        doing_grib = .FALSE.
      ENDIF

c
c --- --------------------
c --- bathymetry
c --- --------------------
c
c --- 'botio ' = bathymetry I/O unit (0 no I/O)
      ioin=iobotin
      if (ioin.gt.0) then
        k=0
        ltheta=.false.
        do j=1,jj
          do i=1,ii
            util1(i,j)=p(i,j,kk+1)
          enddo
        enddo
        IF (doing_grib) ioin = iogrbin
        identity = 0 ! 0 -> non-grib variable, currently RG
        call horout(util1, artype,yrflag,time3,iexpt,lhycom,
     &              ' bathymetry       ',       ! plot name
     &              'bathymetry',               ! ncdf name
     &              'm',                        ! units
     &              k,ltheta, frmt,ioin, identity)
      endif
c
c --- --------------------
c --- mixed layer depth
c --- --------------------
c
c --- 'mltio ' = mix. lay. thick. I/O unit (0 no I/O)
      ioin=iomltin
      if (ioin.gt.0) then
        k=0
        ltheta=.false.
        IF (doing_grib) THEN
          ioin = iogrbin
        ENDIF
        identity = parm_mix_dpth ! 0 -> non-grib variable, currently RG
        call horout(dpmixl,artype,yrflag,time3,iexpt,lhycom,
     &              'mix.layr.thickness',       ! plot name
     &              'mixed_layer_thickness',    ! ncdf name
     &              'm',                        ! units
     &              k,ltheta, frmt,ioin, identity)
      endif
c
c --- --------------------
c --- interface depth
c --- --------------------
c
c --- 'infio ' = interface depths I/O unit (0 no I/O, <0 label with layer #)
      ioin=ioinfin
      if (ioin.ne.0) then
        ltheta = ioin .gt. 0
        ioin   = abs(ioin)
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0) then
                utilk(i,j,k)=p(i,j,k+1)
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        IF (doing_grib) ioin = iogrbin
        identity = 0 ! 0 -> non-grib variable, currently RG
        call horout_3d(utilk, artype,yrflag,time3,iexpt,lhycom,
     &              '  i.depth',                ! plot name
     &              'interface_depth',          ! ncdf name
     &              'm',                        ! units
     &              1,kk,ltheta, frmt,ioin, identity)
      endif
c
c --- ---------------------------
c --- interface vertical velocity
c --- ---------------------------
c
c --- 'wviio ' = intf. k vertical velocity I/O unit (0 no I/O)
      ioin=iowviin
      if (ioin.gt.0) then
        do j=1,jj1
          do i=1,ii1
            if (ip(i,j).ne.0 .and. i.lt.ii .and. j.lt.jj ) then
              utilk(i,j,1)=0.0   ! surface vertical velocity is zero
            else
              utilk(i,j,1)=flag
            endif
          enddo
        enddo
        write(98,*) 'ip   ',ip(100,59),ip(100,58),ip(99,59),ip(99,58)
        write(98,*) ' utilk ',  utilk(100,59,1),utilk(100,58,1),      
     *                utilk(99,59,1),utilk(99,58,1)
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              if (pw(i,j,k).ne.flag) then
                utilk(i,j,k+1)=86400.0*pw(i,j,k)
              else
                utilk(i,j,k+1)=flag
              endif
            enddo
          enddo
        enddo
        call infac2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        IF (doing_grib) ioin = iogrbin
        identity = 0 ! 0 -> non-grib variable, currently RG
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,lhycom,
     &              '  i.veloc',                   ! plot name
     &              'interface_vertical_velocity', ! ncdf name
     &              'm/day',                       ! units
     &              kz, frmt,ioin, identity)
      endif
c
c --- -------------------
c --- u-velocity
c --- -------------------
c
c --- 'uvlio ' = u-velocity I/O unit (0 no I/O)
      ioin=iouvlin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0 .and. i.lt.ii) then
                utilk(i,j,k)=50.0*(u(i,j,2*k)+u(i+1,j,2*k))
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        IF (doing_grib) ioin = iogrbin
        identity = parm_u_vel ! 0 -> non-grib variable, currently RG
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,lhycom,
     &              ' u-veloc.',                ! plot name
     &              'u_velocity',               ! ncdf name
     &              'cm/s',                     ! units
     &              kz, frmt,ioin, identity)
      endif
c
c --- -------------------
c --- v-velocity
c --- -------------------
c
c --- 'vvlio ' = v-velocity I/O unit (0 no I/O)
      ioin=iovvlin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0 .and. j.lt.jj) then
                utilk(i,j,k)=50.0*(v(i,j,2*k)+v(i,j+1,2*k))
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        IF (doing_grib) ioin = iogrbin
        identity = parm_v_vel ! 0 -> non-grib variable, currently RG
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,lhycom,
     &              ' v-veloc.',                ! plot name
     &              'v_velocity',               ! ncdf name
     &              'cm/s',                     ! units
     &              kz, frmt,ioin, identity)
      endif
c
c --- -------------------
c --- speed
c --- -------------------
c
c --- 'splio ' = speed I/O unit (0 no I/O)
      ioin=iosplin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0 .and. i.lt.ii .and. j.lt.jj) then
                utilk(i,j,k)=50.0*sqrt( (u(i,j,2*k)+u(i+1,j,2*k))**2 +
     &                                (v(i,j,2*k)+v(i,j+1,2*k))**2  )
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        IF (doing_grib) ioin = iogrbin
        identity = 0 ! 0 -> non-grib variable, currently RG
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,lhycom,
     &              ' speed   ',                ! plot name
     &              'speed',                    ! ncdf name
     &              'cm/s',                     ! units
     &              kz, frmt,ioin, identity)
      endif
c
c --- ----------------
c --- temperature
c --- ----------------
c
c --- 'temio ' = temperature I/O unit (0 no I/O)
      ioin=iotemin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              utilk(i,j,k)=temp(i,j,2*k)
            enddo
          enddo
        enddo
c        do ik=kk,1
c          utilk(i,j,ik)=utilk1(i,j,k)
c        enddo
        call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        IF (doing_grib) ioin = iogrbin
        identity = parm_temp ! 0 -> non-grib variable, currently RG
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,lhycom,
     &              '  temp   ',                ! plot name
     &              'temperature',              ! ncdf name
     &              'degC',                     ! units
     &              kz, frmt,ioin, identity)
      endif
c
c --- -------------
c --- salinity
c --- -------------
c
c --- 'salio ' = salinity I/O unit (0 no I/O)
      ioin=iosalin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              utilk(i,j,k)=saln(i,j,2*k)
            enddo
          enddo
        enddo
        call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        IF (doing_grib) ioin = iogrbin
        identity = parm_salin ! 0 -> non-grib variable, currently RG
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,lhycom,
     &              ' salinity',                ! plot name
     &              'salinity',                 ! ncdf name
     &              'psu',                      ! units
     &              kz, frmt,ioin, identity)
      endif
c
c --- -------------
c --- density
c --- -------------
c
c --- 'tthio ' = density I/O unit (0 no I/O)
      ioin=iotthin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              utilk(i,j,k)=th3d(i,j,2*k)
            enddo
          enddo
        enddo
        call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        IF (doing_grib) ioin = iogrbin
        identity = parm_density ! 0 -> non-grib variable, currently RG
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,lhycom,
     &              ' density ',                ! plot name
     &              'density',                  ! ncdf name
     &              'sigma',                    ! units
     &              kz, frmt,ioin, identity)
      endif
c
c --- --------------------------
c --- kinetic energy
c --- --------------------------
c
      if     (artype.eq.2) then  ! mean archive
c ---   'keio  ' = kinetic energy I/O unit (0 no I/O)
        ioin=iokein
        if (ioin.gt.0) then
          do k= 1,kk
            do j=1,jj1
              do i=1,ii1
                utilk(i,j,k)=ke(i,j,2*k)
              enddo
            enddo
          enddo
          call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
          IF (doing_grib) ioin = iogrbin
          identity = 0 ! 0 -> non-grib variable, currently RG
          call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,lhycom,
     &                ' ke/mass ',                  ! plot name
     &                'kinetic_energy/mass',        ! ncdf name
     &                'm2/s2',                      ! units
     &                kz, frmt,ioin, identity)
        endif
      endif  ! mean or std. archive

C     If we've been doing grib, close the grib output file now:
      IF (doing_grib) THEN
        ierr = bacio(BACLOSE, start, newpos, SIZEOF_CHARACTER, 0, 
     1                nactual, fdes, fname, grib)
        IF (ierr .NE. 0) THEN
          PRINT *,'Failed to close grib io file cleanly, ierr = ',ierr
        ENDIF
      ENDIF
c
      stop '(normal)'
      end
