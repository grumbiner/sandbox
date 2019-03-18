      program etopo5
c  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

C     Program to read in etopo5 data set, from HS Chen.

      parameter (latmx=59,lonmx=144,mxmx=latmx*lonmx) 
      parameter (rnorth=77.5,rsouth=-67.5,rwest=0.0,reast=357.5,
     1           dlat=2.5,dlon=2.5) 
      dimension xydepth(lonmx,latmx),depth(lonmx),idepth(lonmx),
     1          xlon(lonmx),ylat(latmx),adepth(lonmx)
      character filenamei*80,adepth*1
c
      filenamei='/cray_tmp/wd21hc/etopo/etopo5'
      filenamei='/tmp/wd21hc/etopo/etopo5'
      iin=11
      open(unit=iin,file=filenamei,form='unformatted',recl=8640, 
     &     access='direct',status='old')
c
c
      write(6,150)rnorth,rsouth,rwest,reast
  150 format(' Region of interest selected:',/,
     &        ' latitude:  ',f8.2,' to ',f8.2,/,
     &        ' longitude: ',f8.2,' to ',f8.2)
c
      nlon = nint((reast-rwest)/dlon)+1
      nlat = nint((rnorth-rsouth)/dlat)+1
      print *,'Number of Longitude points: ',nlon
      print *,'Number of Latitude points: ',nlat
c
      if(nlat.gt.latmx .or. nlon.gt.lonmx) then
        print *, 'Try a smaller domain for lon, lat max' 
c       go to 1
	stop	 
      end if
c
      do i=1,nlon
        xlon(i)=rwest+(i-1)*dlon
      enddo
      write(6,444) (xlon(i),i=1,nlon)
  444 format(10f8.2)
      do j=1,nlat
        ylat(j)=rsouth+(j-1)*dlat
      enddo
      write(6,444) (ylat(i),i=1,nlat)
c
c
      do 190 j=1,nlat
      do 170 i=1,nlon
        call readet5(ylat(j),xlon(i),iin,jdep,ierr)
        write(6,445)j,i,ylat(j),xlon(i),iin,jdep,ierr
  445 format(2i5,2f10.2,3i10)
c	print *, rlat,rlon,jdep
        if(ierr.ne.0)then
          print *,' plot-topo: error in readet5 number ',ierr, i, j
          call exit(1)
        endif
        depth(i)=-float(jdep)
  170   continue
	do 180 k=1,nlon
        xydepth(k,j)=depth(k)
  180   continue
  190 continue
c
c     do 30 i=1,nlat 
c     do 30 j=1,nlon
c     xydepth(j,i)=xydepth(j,i)/1000.
c	values now in km.
c     if(dmin.gt.xydepth(j,i))dmin=xydepth(j,i)
c     if(dmax.lt.xydepth(j,i))dmax=xydepth(j,i)
c  30 continue
c
c  Set up topoglo.frm for a PREPROC run. 
c
      write(2,200) dlat,dlon,rsouth,rnorth,rwest,reast
  200 format(8f10.5)
  205 format(12(i5,a1))
      do 240 j=1,nlat
        do 210 i=1,nlon
          if(xydepth(i,j).lt.0.0) then
            idepth(i)=-xydepth(i,j)
            adepth(i)='D'
          else
            idepth(i)=xydepth(i,j)
            adepth(i)='E'
          endif
  210   continue
      write(2,205) (idepth(i),adepth(i),i=1,nlon)
  240 continue
      stop
      end
