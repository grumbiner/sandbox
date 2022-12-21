      subroutine horout(array,
     &                  artype,yrflag,time3,iexpt,lhycom,
     &                  name,namel,units, k,ltheta, frmt,io, identity)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      INCLUDE "grib_parms.h"
      implicit none
c
      character*(*)    name,namel,units,frmt
      logical          lhycom,ltheta
      integer          artype,yrflag,iexpt,k,io
      double precision time3(3)
      real             array(ii,jj),thetak
      INTEGER          identity
c
c     write out array to unit io based on frmt.
c
c     array size and frmt        must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the netCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c
c     Supported I/O types are:
c       frmt=='netCDF'        for netCDF I/O,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='BINDEP'        for unformatted sequential wht lat lon depth I/O,
c       frmt=='REVERSE'   for unformatted sequential reverse Z I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.
c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c       frmt=='GRIB'          GRIB added 7/22/05 R. Grumbine
c
c     This version does not support frmt=='netCDF'.
c
      logical          :: lopen
      integer          :: i,j,l,iyear,month,iday,ihour
      real             :: hmin,hmax
      double precision :: dt,dt0,year
c
      character*81,     save :: label  = ' '
      integer,          save :: iotype = -1
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'jan','feb','mar','apr','may','jun',
     &                 'jul','aug','sep','oct','nov','dec'/
C     Dummies for gribbing
      INTEGER kz
      REAL zz(1)
      DATA kz / 1 /
      INTEGER, save :: ibms = 1
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          call flush(lp)
        if     (frmt(1:l).eq.'HYCOM')  then
c
c         HYCOM .[ab] I/O.
c
          call zbiost(ii,jj)
          iotype = 1
          write(lp,'(/a/)') 'horout - HYCOM I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BIN')    then
c
c         unformatted sequential I/O.
c
          iotype = 2
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(/a/)') 'horout - unformatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:8).eq.'(2f10.4,' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O (lon lat value).
c
          iotype = -3
          write(lp,'(/a,a/)') 'horout - formatted sequential I/O',
     &                        ' (longitude latitude value)'
          call flush(lp)
        elseif (frmt(1:1).eq.'(' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O.
c
          iotype = 3
          write(lp,'(/a/)') 'horout - formatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'netCDF') then
c
c         netCDF I/O.
c
          iotype = 4
          write(lp,'(/2a/)') 'error in horout - ',
     &                       'netCDF I/O not supported in this version'
          call flush(lp)
         elseif (frmt(1:l).eq.'REVERSE') then
c
c         Unformatted Sequential ...Binary reverse Z I/O
c
          iotype = 5
          write(lp,'(/a/)') 'horout-unformatted reverse bin seq I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BINDEP') then
c
c         Unformatted Sequential ... lat lon dep I/O
c
          iotype = 6
          write(lp,'(/a/)') 'horout-unformatted seq lat lon dep I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'GRIB') then
C         grib files are opened in main program
          iotype = 7

        else
c

c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       initialize label.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        if     (artype.eq.1) then
          call fordate(time3(3),yrflag, iyear,month,iday,ihour)
          if     (yrflag.lt.3) then
            write (label(51:72),112) time3(3)/year,cmonth(month),iday
          else
            write (label(51:72),113) cmonth(month),iday,iyear
          endif
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            dt0 = 15.0
          elseif (yrflag.eq.1) then
            dt0 = 15.25
          elseif (yrflag.eq.2) then
            dt0 = 0.0
          else
            dt0 = 0.0
          endif
          if     (artype.eq.2) then
            write(label(51:72),114) ' mean: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          else
            write(label(51:72),114) ' sdev: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          endif
        endif
        if (lhycom) then
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 112    format ('  year',f7.2,' (',a3,i3.2,')')
 113    format ('  date: ',a3,i3.2,',',i5,'  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
c     complete the label
c
      if     (k.eq.0) then
        label(33:50)=name
      elseif (ltheta) then
        write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
      else
        write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
      endif
c
      if     (iotype.eq.1) then
c
c       HYCOM .[ab] I/O.
c
        call zbiopi(lopen, io)
        if     (.not.lopen) then
          call zbiopn('new', io)
          call zhopen(io, 'formatted', 'new', 0)
        endif
        call zbiowr(array, ip,.false., hmin,hmax, io, .false.)
        write(io,'(a,a,2g15.6)') label(33:81),':',hmin,hmax
        call flush(io)
        write(lp,'(a,a,2g15.6)') label(33:81),':',hmin,hmax
        call flush(lp)
      elseif (iotype.eq.2) then
c
c       unformatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        write(io) array
        call flush(io)
        write(lp,'(a)') label(33:81)
        call flush(lp)
      elseif (iotype.eq.3) then
c
c       formatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        write(io,frmt) array
        call flush(io)
        write(lp,'(a)') label(33:81)
        call flush(lp)
      elseif (iotype.eq.-3) then
c
c       formatted sequential I/O (lon lat value)
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do j= 1,jj
          do i= 1,ii
            if     (array(i,j).ne.fill_value) then
              write(io,frmt) plon(i,j),plat(i,j),array(i,j)
             else
                write(io,frmt) plon(i,j),plat(i,j),-999.0
            endif
          enddo
        enddo
        call flush(io)
        write(lp,'(a)') label(33:81)
        call flush(lp)
      elseif (iotype.eq.5) then
c
c       unformatted sequential binary reverse Z I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        write(io) array
        call flush(io)
        write(lp,'(a)') label(33:81)
        call flush(lp)
      elseif (iotype.eq.6) then
c
c       unformatted sequential lat lon depth I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        write(io) array
        call flush(io)
        write(lp,'(a)') label(33:81)
        call flush(lp)
      elseif (iotype.eq.7) then   ! GRIB BRANCH
        CALL engrib_hycom(iyear, month, iday, ihour, 24, fill_value,
     1                    ibms, identity, zz, kz, array)

      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop
      endif
      return
      end

      subroutine horout_3d(array,
     &              artype,yrflag,time3,iexpt,lhycom,
     &              name,namel,units, kf,kl,ltheta, frmt,io, identity)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      implicit none
c
      character*(*)    name,namel,units,frmt
      logical          lhycom,ltheta
      integer          artype,yrflag,iexpt,kf,kl,io
      double precision time3(3)
      real             array(ii,jj,*),thetak
      INTEGER identity
c
c     write out a 3-d layer array to unit io based on frmt.
c
c     2-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the netCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c
c     Supported I/O types are:
c       frmt=='netCDF'        for netCDF I/O,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='REVERSE'   for unformatted sequential binary reverse Z I/O,
c       frmt=='BINDEP'   for unformatted sequential lon lat dep I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.
c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c
c     This version does not support frmt=='netCDF'.
c
      logical          :: lopen
      integer          :: i,j,k,l,iyear,month,iday,ihour
      real             :: hmin(999),hmax(999)
      double precision :: dt,dt0,year
c
      character*81,     save :: label  = ' '
      integer,          save :: iotype = -1
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'jan','feb','mar','apr','may','jun',
     &                 'jul','aug','sep','oct','nov','dec'/
C     Variables for gribbing
      INTEGER, save :: ibms = 1
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        if     (frmt(1:l).eq.'HYCOM')  then
c
c         HYCOM .[ab] I/O.
c
          call zbiost(ii,jj)
          iotype = 1
          write(lp,'(/a/)') 'horout - HYCOM I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BIN')    then
c
c         unformatted sequential I/O.
c
          iotype = 2
          write(lp,'(/a/)') 'horout - unformatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:8).eq.'(2f10.4,' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O (lon lat value).
c
          iotype = -3
          write(lp,'(/a,a/)') 'horout - formatted sequential I/O',
     &                        ' (longitude latitude value)'
          call flush(lp)
        elseif (frmt(1:1).eq.'(' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O.
c
          iotype = 3
          write(lp,'(/a/)') 'horout - formatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'netCDF') then
c
c         netCDF I/O.
c
          iotype = 4
          write(lp,'(/2a/)') 'error in horout - ',
     &                       'netCDF I/O not supported in this version'
          call flush(lp)
        elseif (frmt(1:l).eq.'REVERSE')    then
c
c         unformatted sequential reverse binary Z I/O.
c
          iotype = 5
          write(lp,*) 'iotype  ',iotype
          write(lp,'(/a/)') 'horout-unformatted seq bin reverse I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BINDEP') then 
c
c         unformatted sequential lat lon dep I/O.
c
          iotype = 6
          write(lp,*) 'iotype  ',iotype
          write(lp,'(/a/)') 'horout-unformatted seq lat long dep I/O'
          call flush(lp)
        elseif (frmt(1:l) .eq. 'GRIB') then
          iotype = 7
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       initialize label.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        if     (artype.eq.1) then
          call fordate(time3(3),yrflag, iyear,month,iday,ihour)
          if     (yrflag.lt.3) then
            write (label(51:72),112) time3(3)/year,cmonth(month),iday
          else
            write (label(51:72),113) cmonth(month),iday,iyear
          endif
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            dt0 = 15.0
          elseif (yrflag.eq.1) then
            dt0 = 15.25
          elseif (yrflag.eq.2) then
            dt0 = 0.0
          else
            dt0 = 0.0
          endif
          if     (artype.eq.2) then
            write(label(51:72),114) ' mean: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          else
            write(label(51:72),114) ' sdev: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          endif
        endif
        if (lhycom) then
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 112    format ('  year',f7.2,' (',a3,i3.2,')')
 113    format ('  date: ',a3,i3.2,',',i5,'  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      if     (iotype.eq.1) then
c
c       HYCOM .[ab] I/O.
c
        call zbiopi(lopen, io)
        if     (.not.lopen) then
          call zbiopn('new', io)
          call zhopen(io, 'formatted', 'new', 0)
        endif
        call zbiowr3(array(1,1,kf),kl-kf+1,
     +               ip,.false., hmin(kf),hmax(kf), io, .false.)
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          write(io,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(io)
          write(lp,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(lp)
        enddo
      elseif (iotype.eq.2) then
c
c       unformatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          write(io) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.3) then
c
c       formatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          write(io,frmt) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.-3) then
c
c       formatted sequential I/O (lon lat value).
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          do j= 1,jj
            do i= 1,ii
              if     (array(i,j,k).ne.fill_value) then
                write(io,frmt) plon(i,j),plat(i,j),array(i,j,k)
             else
                write(io,frmt) plon(i,j),plat(i,j),-999.0
              endif
            enddo
          enddo
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.5) then
c
c       unformatted sequential binary reverse Z I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          write(io) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.6) then
c
c     unformatted sequential lat lon dep I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          do j= 1,jj
            do i= 1,ii
              if     (array(i,j,k).ne.fill_value) then
                write(io) plon(i,j),plat(i,j),array(i,j,k)
             else
                write(io) plon(i,j),plat(i,j),-999.0
              endif
            enddo
          enddo
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      else if (iotype .eq. 7) then
        PRINT *,'horout_3d gribbing here'
        CALL engrib_hycom(iyear, month, iday, ihour, 24, fill_value,
     1                    ibms, identity, theta, kf, array)
      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout_3d - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop
      endif
      return
      end

      subroutine horout_3z(array,zz,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namel,units, kz, frmt,io, identity)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      INCLUDE "grib_parms.h"
      implicit none
c
      character*(*)    name,namel,units,frmt
      logical          lhycom
      integer          artype,yrflag,iexpt,kz,io
      double precision time3(3)
      real             array(ii,jj,kz),zz(kz)
      INTEGER          identity
c
c     write out a 3-d z-level array to unit io based on frmt.
c
c     3-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the netCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c
c     Supported I/O types are:
c       frmt=='netCDF'        for netCDF I/O,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='REVERSE'   for unformatted sequential binary reverse Z I/O,
c       frmt=='BINDEP'           for unformatted sequential lon lat dep I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.
c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c       frmt=='GRIB'          GRIB added 7/22/05 R. Grumbine
c
c     This version does not support frmt=='netCDF'.
c
      logical          :: lopen
      integer          :: i,j,k,l,iyear,month,iday,ihour
      real             :: hmin(999),hmax(999)
      double precision :: dt,dt0,year
c
      character*81,     save :: label  = ' '
      integer,          save :: iotype = -1
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'jan','feb','mar','apr','may','jun',
     &                 'jul','aug','sep','oct','nov','dec'/
C     Variables for gribbing
      INTEGER, save :: ibms = 1
c
      write(lp,*) 'iotype  ',iotype
      call flush(lp)
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
c          write(lp,'(3a)')   'frmt   = "', frmt(1:l),'"'
c          call flush(lp)
        if     (frmt(1:l).eq.'HYCOM')  then
c
c         HYCOM .[ab] I/O.
c
          call zbiost(ii,jj)
          iotype = 1
          write(lp,'(/a/)') 'horout - HYCOM I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BIN')    then
c
c         unformatted sequential I/O.
c
          iotype = 2
          write(lp,'(/a/)') 'horout - unformatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:8).eq.'(2f10.4,' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O (lon lat value).
c
          iotype = -3
          write(lp,'(/a,a/)') 'horout - formatted sequential I/O',
     &                        ' (longitude latitude value)'
          call flush(lp)
        elseif (frmt(1:1).eq.'(' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O.
c
          iotype = 3
          write(lp,'(/a/)') 'horout - formatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'netCDF') then
c
c         netCDF I/O.
c
          iotype = 4
          write(lp,'(/2a/)') 'error in horout - ',
     &                       'netCDF I/O not supported in this version'
          call flush(lp)
        elseif (frmt(1:l).eq.'REVERSE')    then
c
c         unformatted sequential binary reverse Z I/O.
c
          iotype = 5
          write(lp,'(/a/)') 'horout-reverse unformatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BINDEP') then
c
c         unformatted sequential lon lat dep Z I/O.
c
          iotype = 6
          write(lp,'(/a/)') 'horout-unformatted seq lat lon dep I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'GRIB') then
C         Grib io initialized in main program
          iotype = 7
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       initialize label.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        if     (artype.eq.1) then
          call fordate(time3(3),yrflag, iyear,month,iday,ihour)
          if     (yrflag.lt.3) then
            write (label(51:72),112) time3(3)/year,cmonth(month),iday
          else
            write (label(51:72),113) cmonth(month),iday,iyear
          endif
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            dt0 = 15.0
          elseif (yrflag.eq.1) then
            dt0 = 15.25
          elseif (yrflag.eq.2) then
            dt0 = 0.0
          else
            dt0 = 0.0
          endif
          if     (artype.eq.2) then
            write(label(51:72),114) ' mean: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          else
            write(label(51:72),114) ' sdev: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          endif
        endif
        if (lhycom) then
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 112    format ('  year',f7.2,' (',a3,i3.2,')')
 113    format ('  date: ',a3,i3.2,',',i5,'  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      if     (iotype.eq.1) then
c
c       HYCOM .[ab] I/O.
c
        call zbiopi(lopen, io)
        if     (.not.lopen) then
          call zbiopn('new', io)
          call zhopen(io, 'formatted', 'new', 0)
        endif
        call zbiowr3(array,kz,
     +               ip,.false., hmin,hmax, io, .false.)
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          write(io,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(io)
          write(lp,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(lp)
        enddo
      elseif (iotype.eq.2) then
c
c       unformatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          write(io) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.3) then
c
c       formatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          write(io,frmt) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.-3) then
c
c       formatted sequential I/O (lon lat value).
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          do j= 1,jj
           do i= 1,ii
            if     (array(i,j,k).ne.fill_value) then
             write(io,frmt) plon(i,j),plat(i,j),array(i,j,k),zz(k)
            else
             write(io,frmt) plon(i,j),plat(i,j),-999.0,zz(k)
            endif
           enddo
         enddo
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.5) then
c
c       unformatted sequential binary reverse Z I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          write(io) array(:,:,(kz+1)-k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
       elseif (iotype.eq.6) then
c
c       unformatted sequential lat lon depI/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          do j= 1,jj
           do i= 1,ii
            if     (array(i,j,k).ne.fill_value) then
             write(io) plon(i,j),plat(i,j),array(i,j,k),zz(k)
            else
             write(io) plon(i,j),plat(i,j),-999.0,zz(k)
            endif
           enddo
         enddo
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo

      elseif (iotype.eq.7) then   ! GRIB BRANCH
        CALL engrib_hycom(iyear, month, iday, ihour, 24, fill_value,
     1                    ibms, identity, zz, kz, array)


      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout_3z - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop
      endif

      return
      end
