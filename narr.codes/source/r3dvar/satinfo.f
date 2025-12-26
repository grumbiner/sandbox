 subroutine satinfo(nusat,nuchan,itype,iuse,mchannel, &
                     varch,polar,wavenumber,frequency, &
                     jpch,mype,mype_rad)

!**** *satinfo* - initialise satellite-dependent data for tovs.
!
!   98-05-15  weiyu yang       mpp version
!   99-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!     purpose.
!     --------
!          to initialise constants and parameters
!          for use of satellite data.
!
!   input argument list:
!     wavenumber      - wavenumber for channels                
!     frequency       - wavenumber for channels                
!     jpch            - number of channels*sat            
!     mype            - p.e. number            
!     mype_rad        - p.e. number for print out
!
!   output argument list:
!     nusat           - satellite number
!     nuchan          - channel number
!     itype           - ir/microwave indicator
!     iuse            - use parameter
!     mchannel        - index for surface emissivity calculations
!     varch           - variance for each channel
!     polar           - polarization
!
!     externals.
!     ----------
!          none.
!
!     reference.
!     ----------
!
!     author.
!     -------
!          John Derber                                
!
!     modifications.
!     --------------
!          18/09/91  changed coeff files for noaa-12
!          13/08/92. for version 2. store for required sats only
!                    add ssu and change channel indices to:
!                    1-20 = hirs 1-20, 21-24 = msu 1-4, 25-27 = ssu 1-3
!   language: f90
!   machine:  ibm RS/6000 SP
!
      implicit logical(l),character*8(c)

 data iicoef/49/

!
! declarations for binary satellite coefficient file

 integer,dimension(jpch):: nuchan,nusat,itype,iuse,mchannel
 real,dimension(jpch):: varch,polar,wavenumber,frequency
 real,dimension(19)::wvlen

! wavelengths for mchannel calculations

    wvlen(1)=3.5
    wvlen(2)=3.6
    wvlen(3)=3.7
    wvlen(4)=3.8
    wvlen(5)=3.9
    wvlen(6)=4.0
    wvlen(7)=4.1
    wvlen(8)=8.0
    wvlen(9)=8.4
    wvlen(10)=8.8
    wvlen(11)=9.2
    wvlen(12)=9.6
    wvlen(13)=10.0
    wvlen(14)=10.5
    wvlen(15)=11.0
    wvlen(16)=11.5
    wvlen(17)=12.0
    wvlen(18)=12.5
    wvlen(19)=13.0


!
!     -----------------------------------------------------------------
!*         1.   open and read mixed gas trans coeffs and ref profile.
!               ---- --- ---- ----- --- ----- ------ --- --- -------

    open(iicoef,file='satinfo',form='formatted')
    mchannel=0.
    do j=1,jpch
      read(iicoef,1000,err=2000,end=2001) nusat(j),&
            nuchan(j),itype(j),iuse(j),idum,varch(j),polar(j)
      if(itype(j) == 0)then
        wavelength=10000./wavenumber(j)
        if(wavelength <= wvlen(1))then
           mchannel(j)=1
        else if(wavelength >= wvlen(19)) then
           mchannel(j)=19
        else
           do i=1,18
             if(wavelength > wvlen(i) .and. wavelength <= wvlen(i+1))then
               fact=float(i)+(wavelength-wvlen(i))/(wvlen(i+1)-wvlen(i))
               mchannel(j)=nint(fact)
               go to 100
             end if
           end do
           write(6,*)' warning no fit to wavelength ', j, wavelength
 100       continue
        end if
      end if
    end do
    close(iicoef)

    if (mype==mype_rad) then
       do j=1,jpch
          wavelength=10000./wavenumber(j)
          write(6,1020) j,nusat(j),nuchan(j),&
               mchannel(j),varch(j),polar(j),iuse(j),wavelength,frequency(j)
       end do
    endif

    return
1000 format(1X,5I5,2E18.10)
1020 format(i3,' sat = ',&
          i3,' chan = ',i2,' mch = ',i2,' var = ',f7.3,&
          ' pol = ',f5.2,' use = ',i2,' wlth = ',f9.2, &
          ' freq = ',f9.2)
!
!    Read error (fatal error)

2000 continue
    write(6,*)'READCF:  ***ERROR*** error reading satinfo ',mype
    go to 2002
!
!    Read eof (fatal error)

2001 continue
    write(6,*)'READCF:  ***ERROR*** fewer coefficients than jpch=',jpch,' in satinfo '
2002 continue
    close(iicoef)
    write(6,*)'READCF:  stop program execution'
    call stop2(96)

    return
    end
