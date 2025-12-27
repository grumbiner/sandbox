      subroutine respw(pwobs,pwges,pwtype,npwdata,iwrite,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    respw       print precipitable water stats
!   prgmmr: parrish          org: w/nmc22    date: 96-12-04
!
! abstract: print precipitable water stats
!
! program history log:
!   96-12-04  parrish
!
!   input argument list:
!     pwobs     - precipitable water obs
!     pwges     - precipitable water ges
!     pwtype    - prepda observation types
!     npwdata   - number of observations
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
      real(4) pwobs(npwdata),pwges(npwdata),pwtype(npwdata)
!--------
         real(4),allocatable::dpw(:),rspres(:)
!--------
         if(npwdata.gt.0) then
          allocate(dpw(npwdata))
          dpw=1.e20
          do i=1,npwdata
           if(pwges(i).lt.1.e19) dpw(i)=pwobs(i)-pwges(i)
          end do
          allocate(rspres(npwdata))
          rspres=1.
         end if
      scale=1.
      ptop=0.
      pbot=2000.
      if(mype.eq.0) then
        write(iwrite,*) ' note that 1520=152 (ssmi total pw)'
        write(iwrite,*) '           1561=156 (sigma=1. to .9) land goes layer pw'
        write(iwrite,*) '           1562=156 (sigma=.9 to .7) land goes layer pw'
        write(iwrite,*) '           1563=156 (sigma=.7 to .3) land goes layer pw'
        write(iwrite,*) '           1564=156 (sigma=.3 to 0.) land goes layer pw'
        write(iwrite,*) '           1581=158 (sigma=1. to .9) ocean goes layer pw'
        write(iwrite,*) '           1582=158 (sigma=.9 to .7) ocean goes layer pw'
        write(iwrite,*) '           1583=158 (sigma=.7 to .3) ocean goes layer pw'
        write(iwrite,*) '           1584=158 (sigma=.3 to 0.) ocean goes layer pw'
      end if
      call dtast(iwrite,pwtype,dpw,scale,npwdata,rspres,pbot,ptop, &
        'current fit of precip.water data, ranges in mm$','pw')

         if(npwdata.gt.0) then
          deallocate(dpw)
          deallocate(rspres)
         end if

      return
      end
