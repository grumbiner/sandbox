      subroutine resq(qpres,qobs,qges,qsatges,qtype,nqdata,iwrite)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    resq        print temperature stats.
!   prgmmr: parrish          org: w/nmc22    date: 90-10-11
!
! abstract: print conventional temperature stats.
!
! program history log:
!   90-10-11  parrish
!
!   input argument list:
!     qpres    - log(pres) 
!     qobs     - moisture obs
!     qges     - moisture ges
!     qsatges  - saturation moisture for ges
!     qtype    - prepda observation types
!     nqdata   - number of observations
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
      real(4) qpres(nqdata),qobs(nqdata),qges(nqdata),qtype(nqdata)
         real(4) qsatges(nqdata)
!--------
         real(4),allocatable::dq(:),rspres(:)
!--------
         if(nqdata.gt.0) then
          allocate(dq(nqdata))
          allocate(rspres(nqdata))
          dq=1.e20
          do i=1,nqdata
           if(qges(i).lt.1.e19) dq(i)=(qobs(i)-qges(i))*100./qsatges(i)
          end do
          rspres=exp(qpres)
         end if
      scale=1.
      pbot = 2000.
      ptop = 900.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of 950mb moisture data, ranges in percent$',' q')
      pbot = 900.
      ptop = 800.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of 850mb moisture data, ranges in percent$',' q')
      pbot = 800.
      ptop = 700.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of 750mb moisture data, ranges in percent$',' q')
      pbot = 700.
      ptop = 600.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of 650mb moisture data, ranges in percent$',' q')
      pbot = 600.
      ptop = 500.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of 550mb moisture data, ranges in percent$',' q')
      pbot = 500.
      ptop = 400.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of 450mb moisture data, ranges in percent$',' q')
      pbot = 400.
      ptop = 300.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of 350mb moisture data, ranges in percent$',' q')
      pbot = 300.
      ptop = 200.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of 250mb moisture data, ranges in percent$',' q')
      pbot = 200.
      ptop = 100.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of 150mb moisture data, ranges in percent$',' q')
      pbot = 100.
      ptop = 0.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of 50mb moisture data, ranges in percent$',' q')
      pbot = 2000.
      ptop = 500.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of sfc-500mb moisture data, ranges in percent$',' q')
      pbot = 500.
      ptop = 0.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of 500-0mb moisture data, ranges in percent$',' q')
      pbot = 2000.
      ptop = 0.
      call dtast(iwrite,qtype,dq,scale,nqdata,rspres,pbot,ptop, &
        'current fit of all moisture data, ranges in percent$',' q')


        if(nqdata.gt.0) then
         deallocate(dq)
         deallocate(rspres)
        end if

      return
      end
