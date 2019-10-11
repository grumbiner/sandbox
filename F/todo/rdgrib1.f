      program rdgrib1
c  example of using rdgi (read index file) and rdgb (read grib file)
c  both the index file (fort.31) and the grib file (fort.11)
c  should be assigned with "-s unblocked" attribute.
      data lupgi/31/
      data lupgb/11/
      parameter(mgi=100)
      integer lgrib(mgi),lskip(mgi),kpds(22,mgi),kgds(20,mgi)
      logical lbms(100000)
      real data(100000)
      ixgi=0
      ngi=mgi
      dowhile(ngi.eq.mgi)
        call rdgi(lupgi,ixgi,mgi,ngi,lgrib,lskip,kpds,kgds)
         print *,' after rdgi. ngi=',ngi
        do n=1,ngi
          ipu=kpds(5,n)
          im=kgds(2,n)
          jm=kgds(3,n)
          print *,'n,ipu=',n,ipu
          if(ipu.eq.1) then
            call rdgb(lupgb,lgrib(n),lskip(n),kpds(1,n),kgds(1,n),
     &                ndata,lbms,data,iret)
            if(iret.ne.0) call exit(iret)
            print *,data(1)
          endif
        enddo
      enddo
      stop
      end

      subroutine rdgi(lugi,ixgi,mgi,ngi,lgrib,lskip,kpds,kgds)
c
c  read grib index file
c  input
c    lugi - logical unit to read
c    ixgi - number of bytes to read (set to zero to read from the start)
c    mgi  - maximum number of index records to read
c  output
c    ixgi - number of bytes read
c    ngi  - number of records read
c    lgrib(ngi) - length of ngi grib records
c    lskip(ngi) - bytes to skip for ngi grib records
c    kpds(22,ngi) - ngi unpacked product definition sections
c    kgds(20,ngi) - ngi unpacked grid definition sections
c
      integer lgrib(mgi),lskip(mgi),kpds(22,mgi),kgds(20,mgi),kptr(16)
      character chead*80,cindex*26,cpdsgds*120
      ngi=0
      if(ixgi.eq.0) then
        rewind lugi
        call rdbyte(lugi,80,chead,iret)
        if(iret.ne.0) return
        ixgi=ixgi+80
        read(chead(16:20),'(i5)') ihgi
        dowhile(ixgi.lt.ihgi)
          irgi=min(ihgi-ixgi,80)
          call rdbyte(lugi,irgi,chead,iret)
          if(iret.ne.0) return
          ixgi=ixgi+irgi
        enddo
      endif
      dowhile(ngi.lt.mgi)
        ngi1=ngi+1
        call rdbyte(lugi,26,cindex,iret)
        if(iret.ne.0) return
        ixgi=ixgi+26
        read(cindex,'(2i10,2i3)') lgrib(ngi1),lskip(ngi1),lpds,lgds
        call rdbyte(lugi,lpds+lgds,cpdsgds,iret)
        if(iret.ne.0) return
        ixgi=ixgi+lpds+lgds
        kptr=0
        kptr(3)=lpds
        call fi632(cpdsgds,kptr,kpds(1,ngi1),iret)
        if(iret.ne.0) return
        call fi633(cpdsgds,kptr,kgds(1,ngi1),iret)
        if(iret.ne.0) return
        ngi=ngi1
      enddo
      return
      end

      subroutine rdgb(lugb,lgrib,lskip,kpds,kgds,ndata,lbms,data,iret)
c
c  read grib file
c  input
c    lugb - logical unit to read
c    lgrib - length of grib record
c    lskip - bytes to skip for grib record
c  output
c    kpds(22) - unpacked product definition section
c    kgds(20) - unpacked grid definition section
c    ndata    - number of data points
c    lbms(ndata) - logical bit map
c    data(ndata) - data unpacked
c    iret - return code
c
      character grib(lgrib)*1
      integer kpds(22),kgds(20),kptr(16)
      logical lbms(*)
      real data(*)
      ndata=0
      call baread(lugb,lskip,lgrib,grib,iret)
      if(iret.ne.0) return
      call w3fi63(grib,kpds,kgds,lbms,data,kptr,iret)
      if(iret.ne.0) return
      ndata=kptr(10)
      return
      end
      subroutine baread(lu,ib,nb,a,iret)
      character a(nb)
      parameter(nwk=512,nbw=8,nbk=nwk*nbw)
      dimension b(nwk)
      iw=ib/nbw
      nw=(ib+nb-1)/nbw+1-ib/nbw
      call setpos(lu,3,iw)
      nwl=nw
      ka=0
      kb=ib-iw*nbw
      dowhile(nwl.gt.0)
        kw=min(nwl,nwk)
        bufferin(lu,0) (b(1),b(kw))
        kn=min(nb-ka,kw*nbw-kb)
        l=length(lu)
        if(l.lt.kw) then
          iret=1
          return
        endif
        call strmov(b,kb+1,kn,a,ka+1)
        nwl=nwl-kw
        ka=ka+kn
        kb=0
      enddo
      iret=0
      return
      end
      subroutine rdbyte(lu,lc,c,iret)
      character c(lc)
      read(lu,end=1,err=2) c
      iret=0
      return
1     iret=1
      return
2     iret=2
      return
      end
