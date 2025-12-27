      integer function newchn(isat,ichan,nusat,nuchan,jpch)
!
!    subroutine newchn(isat,ichan)
!
!    version 1: 13 August 97
!
!    purpose:
!    for a given satellite and channel produce a combined channel number 
!    based on input from file in rttvi.  if channel/satellite unavailable
!    print error message and stop
!
!    input:
!    isat             satellite number (11 - Noaa 11)
!                                      (12 - Noaa 12)
!                                      (14 - Noaa 14)
!                                      (15 - Noaa 15 HIRS)
!                                      (58 - Goes 8)
!                                      (59 - Goes 9)
!                                      (115- Noaa 15 AMSU)
!    ichan            channel number
!
!     uses common: infosat,cparam
!
!     include 'cparam.h'
!     include 'infosat.h'
      integer,dimension(jpch):: nusat,nuchan
      newchn=0
      do j=1,jpch
        if(nuchan(j) .eq. ichan .and. nusat(j) .eq. isat)then
          newchn=j
          return
        end if
      end do
      write(6,*) ' channel and satellite not found in coefficient dataset'
      write(6,*) ' channel = ',ichan
      write(6,*) ' satellite = ',isat
      return
      end
