      subroutine dvastradar(iwrite,type,res,scale,n,rspres,pbot,ptop,etheta,azmin,azmax,mesage,var)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dvast       print table of residuals by data type.
!   prgmmr: parrish          org: w/nmc22    date: 90-10-11
!
! abstract: print table of residuals by data type.
!
! program history log:
!   90-10-11  parrish
!
! usage call dvast(type,res,scale,n,rspres,pbot,ptop,
!    *        etheta,azmin,azmax,mesage)
!   input argument list:
!     type     - obs types (prepda numbers)
!     res      - obs residuals
!     scale    - rescaling unit
!     n        - number of residuals
!     rspres   - observation pressure
!     pbot     - pressure at bottom of layer
!     ptop     - pressure at top of layer
!     etheta   - azimuth of wind observation
!     azmin,azmax - range of azimuths to look at
!     mesage   - message to appear at top of table ($ signals end)
!     var      - variable: u,v,sw,nw or w (w is for combined)
!
!   output argument list:
!     none
!
! attributes:
!   language: cft77
!   machine:  cray ymp
!
!$$$

      include 'mpif.h'  
      include "my_comm.h"

      real(4) type(n),res(n),rspres(n),etheta(n)

      real(8),allocatable::count(:,:),rms(:,:),bias(:,:)
      real(8),allocatable::countall(:,:),rmsall(:,:),biasall(:,:)
      character*1 mesage(100),dollar
      character*2 var
      data dollar/'$'/
!--------

      allocate(count(7,101))
      allocate(rms(7,101))
      allocate(bias(7,101))
      allocate(countall(7,101))
      allocate(rmsall(7,101))
      allocate(biasall(7,101))
      call mpi_comm_rank(my_comm,mype,ierr)

!--    get ioff and make sure it is broadcast to all pe's

      ioff=2200

      count=0._8
      rms=0._8
      bias=0._8
      if(n.gt.0) then
!--------
       do 100 i=1,n
        if(nint(type(i)).lt.2200) go to 99
        if(res(i).gt.1.e19.or.type(i).lt.0. &
              .or.azmin.gt.etheta(i).or.azmax.le.etheta(i) &
              .or.rspres(i).lt.ptop.or.rspres(i).gt.pbot) go to 99
        ress=res(i)*scale
        absr=abs(ress)
        k=1
        if(absr.gt.1.) k=2
        if(absr.gt.2.) k=3
        if(absr.gt.4.) k=4
        if(absr.gt.7.) k=5
        if(absr.gt.10.) k=6
        if(absr.gt.50.) k=7
        itype=nint(type(i))-ioff
        itype=max(1,min(itype,100))
        do 90 l=k,7
          count(l,itype)=count(l,itype)+1._8
          bias(l,itype)=bias(l,itype)+ress*1._8
          rms(l,itype)=rms(l,itype)+ress*ress*1._8
          count(l,101)=count(l,101)+1._8
          bias(l,101)=bias(l,101)+ress*1._8
          rms(l,101)=rms(l,101)+ress*ress*1._8
90      continue
99      continue
100    continue
      end if
      call mpi_allreduce(count,countall,7*101,mpi_real8,mpi_sum,my_comm,ierror)
      call mpi_reduce(rms,rmsall,7*101,mpi_real8,mpi_sum,0,my_comm,ierror)
      call mpi_reduce(bias,biasall,7*101,mpi_real8,mpi_sum,0,my_comm,ierror)

      if(countall(7,101) .le. 0._8)return
      if(mype.eq.0) then
       imsg=1
       do 400 k=1,100
        if(mesage(k).eq.dollar) go to 410
        imsg=imsg+1
400    continue
410    continue
       imsg=max(1,imsg-1)
       write(iwrite,500)(mesage(i),i=1,imsg)
500    format(//,1h ,100a1,//,'  count/bias/rms:',/)
       write(iwrite,600)
600    format(t2,'type',t12,'(e<1)',t22,'(e<2)',t32,'(e<4)', &
               t42,'(e<7)',t52,'(e<10)',t62,'(e<50)',t72,'(50<e)',/)
       do 110 i=1,101
        if(countall(7,i).gt.0._8) then
         do 200 k=1,7
          if(countall(k,i).gt.0._8) then
           biasall(k,i)=biasall(k,i)/countall(k,i)
           rmsall(k,i)=sqrt(rmsall(k,i)/countall(k,i))
          end if
200      continue
         i2=i+ioff
         if(i .le. 100)then
          write(iwrite,700)i2,(countall(k,i),k=1,7)
         else
          write(iwrite,950)(countall(k,i),k=1,7)
         end if
         write(iwrite,800)(biasall(k,i),k=1,7)
         write(iwrite,810)(rmsall(k,i),k=1,7)
         write(60,710) i2,pbot,ptop,var,countall(7,i),biasall(7,i),rmsall(7,i)
        end if
110    continue
700 format(t2,i4,t8,f9.0,t18,f9.0,t28,f9.0,t38,f9.0,t48,f9.0,t58,f9.0,t68,f9.0)
710 format(1x,i4,1x,f6.0,1x,f6.0,1x,a2,1x,f9.0,1x,f9.2,1x,f9.2)
800 format(t4,'bias',t8,f9.2,t18,f9.2,t28,f9.2,t38,f9.2,t48,f9.2,t58,f9.2,t68,f9.2)
810 format(t4,'rms',t8,f9.2,t18,f9.2,t28,f9.2,t38,f9.2,t48,f9.2,t58,f9.2,t68,f9.2)
950 format(t2,'all',t8,f9.0,t18,f9.0,t28,f9.0,t38,f9.0,t48,f9.0,t58,f9.0,t68,f9.0)

      end if
      deallocate(count)
      deallocate(rms)
      deallocate(bias)
      deallocate(countall)
      deallocate(rmsall)
      deallocate(biasall)

      return
      end
