      program readstandard     
!------------------------------------------------------------
! This fortran 90 program reads a data file for the 
! variables SALTY, WTMP, UOGRD, VOGRD, DSLM for one level produced 
! by degribbing any one of the files for the COFS standard grid 
!  cofs.t00z.n00.igb.dtd
!  cofs.t00z.f24.igb.dtd
!  cofs.t00z.f48.igb.dtd
! This program assumes that the input ascii data files
! u.txt, v.txt, t.txt, s.txt wl.txt 
! Renamed 23 January 2003 to:
!   UOGRD.txt, VOGRD.txt, WTMP.txt, SALTY.txt, wl.txt 
! have been produced by degribbing, with the first line
! having the data "332 210" and the next 69720 lines 
! having the elements of the data array one per line.  
!
!  f90 -o read_standard.exe read_standard.f90 
!  ./read_standard.exe >& read_standard.out 
!-----------------------------------------------------------
      implicit none 
      integer im,jm,imjm  
      parameter(im=332,jm=210,imjm=im*jm)   
      real u(im,jm),v(im,jm),t(im,jm),s(im,jm),wl(im,jm) 
      real u1d(imjm)
      integer i,j,k,imax,jmax,ns   
!-----------------------------------------------------------
! initialize to zero 
      do j=1,jm
      do i=1,im
      u(i,j)=0.0
      v(i,j)=0.0
      s(i,j)=0.0
      t(i,j)=0.0 
      wl(i,j)=0.0
      enddo
      enddo
!-----------------------------------------------------------
! read and process U velocity data 
! initializd 1 D array 
      do k=1,imjm
      u1d(k)=0. 
      enddo
!
      open(unit=20,form='formatted',status='old',    &
           file='UOGRD.txt') 
      read(20,90) imax,jmax
   90 format(i3,1x,i3) 
      write(6,91) imax,jmax 
   91 format(5x,' imax = ',i3,' jmax = ',i3)  
! read the U velocity data as a 1-D array 
      do k=1,imjm
      read(20,*) u1d(k)  
      if(u1d(k).gt.100.) u1d(k)=-100.0  
      enddo 
      close(20) 
!      
! create a 2-D array from 1-D array for U vel 
      do 950 j=1,jm 
      ns=im*(j-1) 
      do 925 i=1,im
      u(i,j)=u1d(ns+i) 
  925 continue 
  950 continue 
!
!------------------------------------------------------------
! read and process the V velocity data 
! initialize to zero 
      do k=1,imjm
      u1d(k)=0. 
      enddo
! read and process V velocity data 
      open(unit=20,form='formatted',status='old',    &
           file='VOGRD.txt') 
! read header for each hour of V velocity data 
      read(20,90) imax,jmax
      write(6,91)  imax,jmax 
! read the V velocity data as a 1-D array 
      do k=1,imjm
      read(20,*) u1d(k)  
      if(u1d(k).gt.100.) u1d(k)=-100.0  
      enddo 
      close(20) 
!      
! create a 2-D array from 1-D array for V vel 
      do 850 j=1,jm 
      ns=im*(j-1) 
      do 825 i=1,im
      v(i,j)=u1d(ns+i) 
  825 continue 
  850 continue 
!
!------------------------------------------------------------
! read and process the Salinity data 
! initialize to zero 
      do k=1,imjm
      u1d(k)=0. 
      enddo
!
      open(unit=20,form='formatted',status='old',    &
           file='SALTY.txt') 
! read header for S data 
      read(20,90) imax,jmax
      write(6,91)  imax,jmax 
! read the V velocity data as a 1-D array 
      do  k=1,imjm
      read(20,*) u1d(k)  
      if(u1d(k).gt.100.) u1d(k)=-100.0  
      enddo 
      close(20) 
!      
! create a 2-D array from 1-D array for S  
      do 750 j=1,jm 
      ns=im*(j-1) 
      do 725 i=1,im
      s(i,j)=u1d(ns+i) 
  725 continue 
  750 continue 
!
!------------------------------------------------------------
! read and process the Temperature  data 
! initialize to zero 
      do k=1,imjm
      u1d(k)=0. 
      enddo
!
      open(unit=20,form='formatted',status='old',    &
           file='WTMP.txt') 
! read header for T data file  
      read(20,90) imax,jmax
      write(6,91)  imax,jmax 
! read the V velocity data as a 1-D array in deg Kelvin 
      do  k=1,imjm
      read(20,*) u1d(k)  
      if(u1d(k).gt.350.) u1d(k)=-100.0  
      enddo 
      close(20) 
!      
! create a 2-D array from 1-D array for T  
      do 650 j=1,jm 
      ns=im*(j-1) 
      do 625 i=1,im
      t(i,j)=u1d(ns+i) 
  625 continue 
  650 continue 
!
! convert Kelvin temp to Celsius temp
      do j=1,jm
      do i=1,im
      if(t(i,j).gt.0.) t(i,j)=t(i,j)-273. 
      enddo
      enddo 
!----
!------------------------------------------------------------
! read and process the Water Level data 
! initialize to zero 
      do k=1,imjm
      u1d(k)=0. 
      enddo
!
      open(unit=20,form='formatted',status='old',    &
           file='wl.txt') 
! read header for water level data 
      read(20,90) imax,jmax
      write(6,91)  imax,jmax 
! read the water level data as a 1-D array 
      do  k=1,imjm
      read(20,*) u1d(k)  
      if(u1d(k).gt.100.) u1d(k)=-100.0  
      enddo 
      close(20) 
!      
! create a 2-D array from 1-D array for water level  
      do 450 j=1,jm 
      ns=im*(j-1) 
      do 425 i=1,im
      wl(i,j)=u1d(ns+i) 
  425 continue 
  450 continue 
!
!------------------------------------------------------------
!----
!--------------------------------------------------------------
! write the U, V, S, T, wl fields to a file for input to a plotting program  
      open(unit=30,form='unformatted',status='unknown',    &
           file='uvstw.txt')  
      write(30) u,v,s,t,wl 
      close(30) 
!
      stop
      end 
!-----------------------------------------------------------------------
