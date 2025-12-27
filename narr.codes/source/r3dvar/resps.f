      subroutine resps(pobs,pges,ptype,npdata,iwrite)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    resps      print sfchgt stats.
!   prgmmr: parrish          org: w/nmc22    date: 97-08-12
!
! abstract: print conventional surface pressure stats
!
! program history log:
!   97-08-12  parrish
!
!   input argument list:
!     ppres    - log(pres) 
!     hobs     - observation elevation
!     tges     - ges elevation at observation pressure
!     ptype    - prepda observation types
!     npdata   - number of observations
!
!   output argument list:
!     none                
!
! attributes:
!   language: cft77
!   machine:  cray ymp
!
!$$$
!--------
      real(4) pobs(npdata),pges(npdata),ptype(npdata)
!--------
         real(4),allocatable::dp(:),rspres(:)
!--------
        if(npdata.gt.0) then
         allocate(dp(npdata))
         dp=1.e20
         do i=1,npdata
          if(pges(i).lt.1.e19) dp(i)=pobs(i)-pges(i)
         end do
         allocate(rspres(npdata))
         rspres=1000.
        end if
      scale=1.
      pbot = 2000.
      ptop = 0.
      call dtast(iwrite,ptype,dp,scale,npdata,rspres,pbot,ptop, &
        'current fit of sfcp data, ranges in mb$',' p')


        if(npdata.gt.0) then
         deallocate(dp)
         deallocate(rspres)
        end if

      return
      end
