From wd23ss Wed Dec  1 16:13 EST 1993
Return-Path: <wd23ss>
Received:  by sn1029.domain.domain
	id AA88221; 5.61/CRI-%I%; Wed, 1 Dec 93 16:13:42 -0500
Date: Wed, 1 Dec 93 16:13:42 -0500
From: Suranjana Saha <wd23ss>
Full-Name: Suranjana Saha
Message-Id: <9312012113.AA88221@sn1029.domain.domain>
Apparently-To: wd21rg

	program wksnice
        parameter(itsday=1,itsmth=1,itsyr=73)
        parameter(isday=31,ismth=12,isyr=84)
        parameter(ieday=31,iemth=12,ieyr=92)
        parameter(idim=181,jdim=91)
        parameter(idimo=180)
c       parameter(kys=1985,kye=1992)
        parameter(kys=1985,kye=1985)
c
        character*28 slmask
        character*33 ice
        character*34 snow
        character*32 snocl
        character*35 snowice
c
        character*1 sealnd(idim,jdim)
        character*1 snoc(idim,jdim)
        character*1 snice(idim,jdim)
        character*1 iland,isea,seaice,isno,igreen
        character*2 jnum(31),jflagn,jflago,jyr,jmth,jday,jhr
c
        dimension sice(idimo,jdim)
        dimension snonh(idim,46)
        dimension out(idim,jdim)
c
        data iland/'X'/,isea/' '/,igreen/'G'/,seaice/'I'/,isno/'S'/
        data jflagn/'01'/,jflago/'00'/,jhr/'00'/
        data jnum/'01','02','03','04','05','06','07','08','09','10',
     1            '11','12','13','14','15','16','17','18','19','20',
     2            '21','22','23','24','25','26','27','28','29','30',
     3            '31'/
c
        slmask='/reanl1/monitor/fixed/slmask'
        snow='/reanl1/monitor/fixed/snow.timeser'
        snocl='/reanl1/monitor/fixed/snow.climo'
        ice='/reanl1/monitor/fixed/ice.timeser'
        snowice='/reanl1/monitor/fixed/snice.timeser'
c
        call assign('assign -Fcos -Nibm -Cebcdic u:50')
        open(50,file=slmask,form='unformatted')
        call assign('assign -Ff77 -Nieee -Cascii u:51')
        open(51,file=snow,form='unformatted')
        call assign('assign -Fcos -Nibm -Cascii u:52')
        open(52,file=snocl,form='unformatted')
        call assign('assign -Ff77 -Nieee -Cascii u:53')
        open(53,file=ice,form='unformatted')
c
        call assign('assign -Fcos -Nibm -Cascii u:54')
        open(54,file=snowice,form='unformatted')
        call assign('assign -Ff77 -Nieee -Cascii u:55')
        open(55,file='/tmp/wd23ss/sintest',form='unformatted')
c
c... get landsea mask..
c
        read(50) sealnd
c
        call w3fs17(itsyr,itsmth,itsday,ncnts)
        print *,' tape start date ',itsyr,itsmth,itsday,ncnts
        call w3fs17(isyr,ismth,isday,ncns)
        print *,' actual start date ',isyr,ismth,isday,ncns
        call w3fs17(ieyr,iemth,ieday,ncne)
        print *,' end date ',ieyr,iemth,ieday,ncne
c
c...  spool snow data to correct start date...
c
        do ncn=ncnts,ncns-1,7
        read(51) snonh
        enddo
c
        nweek=0
        do ncn=ncns,ncne,7
        nweek=nweek+1
c
c... fill in land sea contrast...
c
        do j=1,jdim
        do i=1,idim
        snice(i,j)=sealnd(i,j)
        enddo
        enddo
c
c... fill in northern hemisphere snow...
c
        read(51) snonh
        do j=1,46
        do i=1,idim
        if((sealnd(i,j).eq.iland).and.(snonh(i,j).gt.0.))
     *  snice(i,j)=isno
        enddo
        enddo
c
c... read in southern hemisphere snow climatology for the month.....
c
        rewind 52
        do k=1,imth
        read(52) snoc
        enddo
c
c... fill in southern hemisphere snow from climatology.....
        do j=47,jdim
        do i=1,idim
        if((sealnd(i,j).eq.iland).and.(snoc(i,j).eq.isno))
     *  snice(i,j)=isno
        enddo
        enddo
c
c... fill in global seaice....
c
        read(53) sice
        do j=1,jdim
        do i=1,idim
        if((sealnd(i,j).eq.isea).and.(sice(i,j).gt.0.))
     *  snice(i,j)=seaice
        enddo
        enddo
c
c... fill in greenland glaciers...
c
        do j=5,15
        do i=150,170
        if(sealnd(i,j).eq.iland) snice(i,j)=igreen
        enddo
        enddo
        snice(171,6)=igreen
        snice(149,6)=igreen
        snice(146,7)=igreen
        snice(147,7)=igreen
        snice(148,7)=igreen
        snice(149,7)=igreen
c
c... fill in antarctica glaciers...
c
        do j=79,jdim
        do i=1,idim
        if(sealnd(i,j).eq.iland) snice(i,j)=igreen
        enddo
        enddo
c
        call datex(ncn,jyr,jmth,jday,iyr,imth,iday,jnum)
        write(54) jflagn,jyr,jmth,jday,jhr,snice,jyr,jmth,jday,jhr
        print *,'week ',nweek,' date ',jyr,jmth,jday,jhr
c
        do ncnx=ncn+1,ncn+6
        call datex(ncnx,jyr,jmth,jday,iyr,imth,iday,jnum)
        write(54) jflago,jyr,jmth,jday,jhr,snice,jyr,jmth,jday,jhr
        enddo
c
        do j=1,jdim
        do i=1,idim
        if(snice(i,j).eq.iland) out(i,j)=1.
        if(snice(i,j).eq.isea) out(i,j)=2.
        if(snice(i,j).eq.isno) out(i,j)=3.
        if(snice(i,j).eq.seaice) out(i,j)=4.
        if(snice(i,j).eq.igreen) out(i,j)=5.
        enddo
        enddo
        write(55) out
c
        enddo
c
        stop
        end
        subroutine datex(ncn,jyr,jmth,jday,iyr,imth,iday,jnum)
c
        character*2 jnum(31),jyr,jmth,jday
c
        call w3fs19(ncn,iyr,imth,iday)
        write(jyr,'(i2)') iyr
        jmth=jnum(imth)
        jday=jnum(iday)
c
        return
        end


