      program readsection
!------------------------------------------------------------
! This program reads the ETOPO2 bathymetry file 'etopo2.raw'
! that was taken from a CDROM from NGDC.   
!-----------------------------------------------------------
      implicit none 
      integer im,jm,i,j,iskip,jskip
      real wlon,elon,slat,nlat 
      character*30 cregion
      character*10 clabel   
      integer*2 idepth(10800,5400) 
      real, allocatable::depth(:,:),alon(:,:),alat(:,:)    
!------------------------------------------------------------
! read the etopo2 bathymetry file 
      open(unit=10,form='unformatted',status='old',access='direct', &
          recl=21600,file='etopo2.raw')  
      do j=1,5400 
      read(10,rec=j) (idepth(i,j),i=1,10800)  
      enddo
      close(10)  
!-----------------------------------------------------------
! read the input file with desired section 
      open(unit=15,form='formatted',status='old', &
           file='read_section.in')   
      read(15,'(a30)') cregion
      read(15,'(a10)') clabel 
      read(15,*) wlon
      read(15,*) elon
      read(15,*) slat
      read(15,*) nlat  
      close(15)   
!
      write(6,'(a30)') cregion
      write(6,'(a10)') clabel
   97 format(a30)       
      write(6,95) wlon,elon,slat,nlat
   95 format(2x,'wlon = ',f10.4,'elon = ',f10.4,' slat = ',f10.4, &
             ' nlat = ',f10.4)    
!---------------------------------------------------------------
! cut out the section specified by file 'read_section.in'
! The etopo2 data is listed from -180 to +180
! Check to see if domain crosses International Date line
      if(elon.gt.wlon) then 
! proceed normally, domain does not cross International Date Line 
      iskip=30*(180+wlon) 
      jskip=30*( 90-nlat) 
!
      im=(elon-wlon)*30.0+1
      jm=(nlat-slat)*30.0+1
      write(6,96) im,jm    
!
      allocate (depth(im,jm),alon(im,jm),alat(im,jm)) 
!
   96 format(5x,'im = ',i5,' jm = ',i5) 
      do j=1,jm
      do i=1,im
      depth(i,j)=idepth(iskip+i,jskip+jm+1-j) 
      enddo
      enddo 
!-----------------------------------------------------------------
      else
! Domain crosses International Date Line - special processing 
      stop
      endif 
!--------------------------------------------------------------
! define the longitude and latitude arrays  
      do j=1,jm
      do i=1,im
      alon(i,j)=wlon+(2./60.)*(i-1) 
      alat(i,j)=slat+(2./60.)*(j-1)  
      enddo
      enddo 
!----------------------------------------------------------------
! make ocean depths postive, land negative
      do j=1,jm
      do i=1,im
      depth(i,j)=-depth(i,j)   
      enddo
      enddo 
!-------------------------------------------------------------------
! write out values as a check
      i=1
      j=1
      write(6,90) i,j,alon(i,j),alat(i,j),depth(i,j)  
      i=im
      j=1
      write(6,90) i,j,alon(i,j),alat(i,j),depth(i,j)  
      i=1
      j=jm
      write(6,90) i,j,alon(i,j),alat(i,j),depth(i,j)  
      i=im
      j=jm  
      write(6,90) i,j,alon(i,j),alat(i,j),depth(i,j)  
   90 format(5x,'i = ',i10,' j = ',i10,' alon = ',f7.2, & 
             ' alat = ',f7.2,' depth = ',f10.0) 
!---------------------------------------------------------------
! write header data to a separate file 
      open(unit=19,form='formatted',status='unknown',  & 
           file=clabel//'.txt')  
      write(19,'(a30)') cregion
      write(19,'(a10)') clabel 
      write(19,98) im,jm,wlon,elon,slat,nlat
   98 format(2i5,4f10.4) 
      close(19) 
!-------------------------------------------------------------
! write lon,lat, depth to file 
      open(unit=20,form='unformatted',status='unknown', &
           file=clabel//'.dat')
      write(20) alon,alat,depth 
      close(20) 
!-----------------------------------------------------------------
      stop
      end 
