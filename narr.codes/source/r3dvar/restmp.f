      subroutine restmp(tpres,tobs,tges,ttype,ntdata,iwrite)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    restmp      print temperature stats.
!   prgmmr: parrish          org: w/nmc22    date: 90-10-11
!
! abstract: print conventional temperature stats.
!
! program history log:
!   90-10-11  parrish
!
!   input argument list:
!     tpres    - log(pres) 
!     tobs     - temp obs
!     tges     - temp ges
!     ttype    - prepda observation types
!     ntdata   - number of observations
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
      real(4) tpres(ntdata),tobs(ntdata),tges(ntdata),ttype(ntdata)
!--------
         real(4),allocatable::dt(:),rspres(:)
!--------
        if(ntdata.gt.0) then
         allocate(dt(ntdata))
         allocate(rspres(ntdata))
         dt=1.e20
         do i=1,ntdata
          if(tges(i).lt.1.e19) dt(i)=tobs(i)-tges(i)
         end do
         rspres=exp(tpres)
        end if
      scale=1.
      pbot=2000.
      ptop=900.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of 950mb temperature data, ranges in k$',' T')
      pbot=900.
      ptop=800.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of 850mb temperature data, ranges in k$',' T')
      pbot=800.
      ptop=700.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of 750mb temperature data, ranges in k$',' T')
      pbot=700.
      ptop=600.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of 650mb temperature data, ranges in k$',' T')
      pbot=600.
      ptop=500.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of 550mb temperature data, ranges in k$',' T')
      pbot=500.
      ptop=400.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of 450mb temperature data, ranges in k$',' T')
      pbot=400.
      ptop=300.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of 350mb temperature data, ranges in k$',' T')
      pbot=300.
      ptop=200.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of 250mb temperature data, ranges in k$',' T')
      pbot=200.
      ptop=100.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of 150mb temperature data, ranges in k$',' T')
      pbot=100.
      ptop=0.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of 50mb temperature data, ranges in k$',' T')
      pbot=2000.
      ptop=500.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of sfc-500mb temperature data, ranges in k$',' T')
      pbot=500.
      ptop=0.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of 500-0mb temperature data, ranges in k$',' T')
      pbot=2000.
      ptop=0.
      call dtast(iwrite,ttype,dt,scale,ntdata,rspres,pbot,ptop, &
        'current fit of all temperature data, ranges in k$',' T')

        if(ntdata.gt.0) then
         deallocate(dt)
         deallocate(rspres)
        end if

      return
      end
