       program pcpmon
       parameter (im=237,jm=387)
       real pcpob(im,jm),pcpob3hr(im,jm)
       real pcprr(im,jm)
       real land(im,jm)
       real pcpdiff(im,jm)
       real pcpstd(im,jm)
       real stddifftot(im,jm),stddiffland(im,jm),stddiffwater(im,jm)
       real*8 biasland, biaswater, biastot
       real*8 stdland, stdwater, stdtot
       real*8 biaslandsum, biaswatersum, biastotsum
       real*8 stdlandsum, stdwatersum, stdtotsum
       real*8 pcprrtotsum, pcprrlandsum, pcprrwatersum
       real*8 pcpobtotsum, pcpoblandsum, pcpobwatersum
       real*8 pcprrmean, pcprrlandmean, pcprrwatermean
       real*8 pcpobmean, pcpoblandmean, pcpobwatermean
       integer kpds(25),kgds(22),jpds(25),jgds(22)
       logical*1 bit(im,jm)
c
c Initialize the summing variables
c
       biaslandsum=0.0
       biaswatersum=0.0
       biastotsum=0.0
       stdlandsum=0.0
       stdwatersum=0.0
       stdtotsum=0.0
       pcpob3hr=0.0
       pcprrtotsum=0.0
       pcprrlandsum=0.0
       pcprrwatersum=0.0
       pcpobtotsum=0.0
       pcpoblandsum=0.0
       pcpwatersum=0.0
c 
c Initialize counters
c
       itot=0
       iland=0
       iwater=0
c
c Baopen the files
c
       call baopen(10,'fort.10',ireto)
       call baopen(21,'fort.21',ireto)
       call baopen(22,'fort.22',ireto)
       call baopen(23,'fort.23',ireto)
       call baopen(30,'fort.30',ireto)
c
c Read in the precip obs (analyses)
c
       do ihr=1,3
        iunit=20+ihr
        jpds=-1
        jgds=-1
        jpds(5)=61
        call getgb(iunit,0,im*jm,0,jpds,jgds,kf,k,kpds,kgds,bit,
     *    pcpob,iret) 
        print*,'kpds=',kpds
        print*,'kgds=',kgds
        if(iret.ne.0) then
         print*,'Problem in reading in precip ob at ihr=',ihr
         print*,'iret=',iret
         stop 1
        endif
        do j=1,jm
        do i=1,im
          if(pcpob(i,j).lt.0.0) pcpob(i,j)=0.0
          pcpob3hr(i,j)=pcpob3hr(i,j)+pcpob(i,j)
c         print*,'i,j,pcpob3hr,pcpob=',i,j,pcpob3hr(i,j),pcpob(i,j)
        enddo
        enddo
       enddo
c
c Read in the RR precip (from the corresponding RR first guess)
c
       iunit=10
       jpds=-1
       jgds=-1
       jpds(5)=61
       call getgb(iunit,0,im*jm,0,jpds,jgds,kf,k,kpds,kgds,bit,
     *     pcprr,iret)
        print*,'kpds=',kpds
        print*,'kgds=',kgds
       if(iret.ne.0) then
         print*,'Problem reading in the RR precip'
         print*,'iret=',iret
         stop2
       endif
       do j=1,jm
       do i=1,im
        if(pcprr(i,j).lt.0.0) pcprr(i,j)=0.0
       enddo
       enddo
c
c Read in the land/sea mask from the RR fixed field file
c
      iunit=30
      jpds=-1
      jgds=-1
      jpds(5)=81
      call getgb(iunit,0,im*jm,0,jpds,jgds,kf,k,kpds,kgds,bit,
     *   land,iret)
      if(iret.ne.0) then
        print*,'Problem reading in the land/sea mask'
        print*,'iret=',iret
        stop 3
      endif 
c
c Calculate precip differences; calculate the sums
c Also calculate the sums for the mean ob and mean RR precip
c
      do j=1,jm
      do i=1,im
       pcpdiff(i,j)=pcpob3hr(i,j)-pcprr(i,j)
       biastotsum=biastotsum+pcpdiff(i,j)
       pcprrtotsum=pcprrtotsum+pcprr(i,j)
       pcpobtotsum=pcpobtotsum+pcpob3hr(i,j) 
       itot=itot+1
       if(land(i,j).gt.0.5) then ! land
        biaslandsum=biaslandsum+pcpdiff(i,j)
        pcprrlandsum=pcprrlandsum+pcprr(i,j)
        pcpoblandsum=pcpoblandsum+pcpob3hr(i,j)
        iland=iland+1
       else                 ! water
        biaswatersum=biaswatersum+pcpdiff(i,j)
        pcprrwatersum=pcprrwatersum+pcprr(i,j)
        pcpobwatersum=pcpobwatersum+pcpob3hr(i,j)
        iwater=iwater+1
       endif
      enddo
      enddo
c
c Calculate the biases
c
      biastot=biastotsum/itot
      biasland=biaslandsum/iland
      biaswater=biaswatersum/iwater
c
c Calculate the mean RR and ob precipitation values
c
      pcprrmean=pcprrtotsum/itot
      pcprrlandmean=pcprrlandsum/iland
      pcprrwatermean=pcprrwatersum/iwater
      pcpobmean=pcpobtotsum/itot
      pcpoblandmean=pcpoblandsum/iland
      pcpobwatermean=pcpobwatersum/iwater 
c
c Calculate the differences for the standard deviations; square the diffs
c Calculate the sums
c
      do j=1,jm
      do i=1,im
       stddifftot(i,j)=(pcpdiff(i,j)-biastot)*
     *     (pcpdiff(i,j)-biastot)
       stdtotsum=stdtotsum+stddifftot(i,j)
       if(land(i,j).gt.0.5) then  ! land
        stddiffland(i,j)=(pcpdiff(i,j)-biasland)*
     *     (pcpdiff(i,j)-biasland)
        stdlandsum=stdlandsum+stddiffland(i,j)
       else
        stddiffwater(i,j)=(pcpdiff(i,j)-biaswater)*
     *     (pcpdiff(i,j)-biaswater)
        stdwatersum=stdwatersum+stddiffwater(i,j)
       endif
      enddo
      enddo
c
c Calculate the standard deviations
c
      stdtot=stdtotsum/itot
      stdland=stdlandsum/iland
      stdwater=stdwatersum/iwater

c
c Now write out the biases and standard deviations
c
      write(50,100) pcpobmean, pcpoblandmean, pcpobwatermean
      write(50,100) pcprrmean, pcprrlandmean, pcprrwatermean
      write(50,100) biastot,biasland,biaswater
      write(50,100) stdtot,stdland,stdwater
100   format(1x,3f8.5)

      stop
      end
