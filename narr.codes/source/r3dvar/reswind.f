      subroutine reswind(wpres,etheta,wobs,wges,wtype,nwdata,iwrite)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    reswind     print wind stats.
!   prgmmr: parrish          org: w/nmc22    date: 90-10-11
!
! abstract: print wind stats.
!
! program history log:
!   90-10-11  parrish
!
!   input argument list:
!     wpres    - log(pres) 
!     etheta   - azimuth angle of wind obs
!     wobs     - wind obs
!     wges     - wind ges
!     wtype    - prepda observation types
!     nwdata   - number of observations
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
      real(4) wpres(nwdata),wobs(nwdata),wges(nwdata),wtype(nwdata)
         real(4) etheta(nwdata)
!--------
         real(4),allocatable::dw(:),rspres(:),ethetah(:)
!--------
        if(nwdata.gt.0) then
         allocate(dw(nwdata))
         dw=1.e20
         allocate(ethetah(nwdata))
         ethetah=etheta
         do i=1,nwdata
          if(wges(i).lt.1.e19) then
           dw(i)=wobs(i)-wges(i)
           if(ethetah(i).gt.180.) ethetah(i)=ethetah(i)-360.
           if(ethetah(i).gt.180.) ethetah(i)=ethetah(i)-360.
           if(ethetah(i).lt.-180.) ethetah(i)=ethetah(i)+360.
           if(ethetah(i).lt.-180.) ethetah(i)=ethetah(i)+360.
           if(ethetah(i).lt.-67.5) then
            ethetah(i)=ethetah(i)+180.
            dw(i)=-dw(i)
           end if
           if(ethetah(i).ge.112.5) then
            ethetah(i)=ethetah(i)-180.
            dw(i)=-dw(i)
           end if
          end if
         end do
         allocate(rspres(nwdata))
         rspres=exp(wpres)
        end if
      scale=1.
      azmin=67.5
      azmax=112.5
      pbot=1100.
      ptop=900.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 950mb southerly wind data, ranges in k$',' v')
      pbot=900.
      ptop=800.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 850mb southerly wind data, ranges in k$',' v')
      pbot=800.
      ptop=700.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 750mb southerly wind data, ranges in k$',' v')
      pbot=700.
      ptop=600.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 650mb southerly wind data, ranges in k$',' v')
      pbot=600.
      ptop=500.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 550mb southerly wind data, ranges in k$',' v')
      pbot=500.
      ptop=400.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 450mb southerly wind data, ranges in k$',' v')
      pbot=400.
      ptop=300.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 350mb southerly wind data, ranges in k$',' v')
      pbot=300.
      ptop=200.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 250mb southerly wind data, ranges in k$',' v')
      pbot=200.
      ptop=100.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 150mb southerly wind data, ranges in k$',' v')
      pbot=100.
      ptop=0.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 50mb southerly wind data, ranges in k$',' v')

      azmin=22.5
      azmax=67.5
      pbot=1100.
      ptop=900.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 950mb southwesterly wind data, ranges in k$','sw')
      pbot=900.
      ptop=800.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 850mb southwesterly wind data, ranges in k$','sw')
      pbot=800.
      ptop=700.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 750mb southwesterly wind data, ranges in k$','sw')
      pbot=700.
      ptop=600.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 650mb southwesterly wind data, ranges in k$','sw')
      pbot=600.
      ptop=500.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 550mb southwesterly wind data, ranges in k$','sw')
      pbot=500.
      ptop=400.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 450mb southwesterly wind data, ranges in k$','sw')
      pbot=400.
      ptop=300.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 350mb southwesterly wind data, ranges in k$','sw')
      pbot=300.
      ptop=200.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 250mb southwesterly wind data, ranges in k$','sw')
      pbot=200.
      ptop=100.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 150mb southwesterly wind data, ranges in k$','sw')
      pbot=100.
      ptop=0.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 50mb southwesterly wind data, ranges in k$','sw')

      azmin=-22.5
      azmax=22.5
      pbot=1100.
      ptop=900.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 950mb westerly wind data, ranges in k$',' u')
      pbot=900.
      ptop=800.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 850mb westerly wind data, ranges in k$',' u')
      pbot=800.
      ptop=700.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 750mb westerly wind data, ranges in k$',' u')
      pbot=700.
      ptop=600.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 650mb westerly wind data, ranges in k$',' u')
      pbot=600.
      ptop=500.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 550mb westerly wind data, ranges in k$',' u')
      pbot=500.
      ptop=400.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 450mb westerly wind data, ranges in k$',' u')
      pbot=400.
      ptop=300.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 350mb westerly wind data, ranges in k$',' u')
      pbot=300.
      ptop=200.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 250mb westerly wind data, ranges in k$',' u')
      pbot=200.
      ptop=100.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 150mb westerly wind data, ranges in k$',' u')
      pbot=100.
      ptop=0.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 50mb westerly wind data, ranges in k$',' u')

      azmin=-67.5
      azmax=-22.5
      pbot=1100.
      ptop=900.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 950mb northwesterly wind data, ranges in k$','nw')
      pbot=900.
      ptop=800.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 850mb northwesterly wind data, ranges in k$','nw')
      pbot=800.
      ptop=700.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 750mb northwesterly wind data, ranges in k$','nw')
      pbot=700.
      ptop=600.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 650mb northwesterly wind data, ranges in k$','nw')
      pbot=600.
      ptop=500.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 550mb northwesterly wind data, ranges in k$','nw')
      pbot=500.
      ptop=400.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 450mb northwesterly wind data, ranges in k$','nw')
      pbot=400.
      ptop=300.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 350mb northwesterly wind data, ranges in k$','nw')
      pbot=300.
      ptop=200.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 250mb northwesterly wind data, ranges in k$','nw')
      pbot=200.
      ptop=100.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 150mb northwesterly wind data, ranges in k$','nw')
      pbot=100.
      ptop=0.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 50mb northwesterly wind data, ranges in k$','nw')

      azmin=-70.
      azmax=120.
      pbot=1100.
      ptop=900.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 950mb wind data, ranges in k$',' w')
      pbot=900.
      ptop=800.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 850mb wind data, ranges in k$',' w')
      pbot=800.
      ptop=700.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 750mb wind data, ranges in k$',' w')
      pbot=700.
      ptop=600.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 650mb wind data, ranges in k$',' w')
      pbot=600.
      ptop=500.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 550mb wind data, ranges in k$',' w')
      pbot=500.
      ptop=400.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 450mb wind data, ranges in k$',' w')
      pbot=400.
      ptop=300.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 350mb wind data, ranges in k$',' w')
      pbot=300.
      ptop=200.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 250mb wind data, ranges in k$',' w')
      pbot=200.
      ptop=100.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 150mb wind data, ranges in k$',' w')
      pbot=100.
      ptop=0.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 50mb wind data, ranges in k$',' w')

      pbot=2000.
      ptop=500.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of sfc-500mb wind data, ranges in k$',' w')
      pbot=500.
      ptop=0.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of 500-0mb wind data, ranges in k$',' w')
      pbot=2000.
      ptop=0.
      call dvast(iwrite,wtype,dw,scale,nwdata,rspres, &
               pbot,ptop,ethetah,azmin,azmax, &
        'current fit of all wind data, ranges in k$',' w')

        if(nwdata.gt.0) then
         deallocate(dw)
         deallocate(ethetah)
         deallocate(rspres)
        end if

      call resradarwind(wpres,etheta,wobs,wges,wtype,nwdata,iwrite)

      return
      end
