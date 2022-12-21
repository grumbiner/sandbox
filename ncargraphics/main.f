      PROGRAM allmain
      IMPLICIT none
!Generalized samples of main programs from Gemmill ncargraphics library


      ! Get data

!Make calls (each call block is independent)
        slat = 40.0
        delat = 1.0
        call geos(p,ug,vg,idim,jdim,slat,dlat)

!
       call radius(p,r,idim,jdim)

!
      call OPNGKS
c-----------------------------------------------------------
      call setusv ('IM',5)
c red
      call stclrs (10000,   00, 1500,10000,ic1)
c green
      call stclrs (    0,10000,    0,10000,ic2)
c purple
      call stclrs ( 4000,    0,10000,10000,ic3)
c light blue
      call stclrs ( 1500,10000,10000,10000,ic4)
c black
      call stclrs (    0,    0,    0,10000,ic5)
      call setusv ('II',ic1)
c
      call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
c
      call setusv ('II',ic1)
      call wtstr (0.40,0.99,nsat,2,0,0)
c
      call wtstr (0.35,0.90,'PLOT VARIABLE',2,0,0)
      call wtstr (0.50,0.90,ivar(iv),2,0,0)
c
      call wtstr (0.35,0.96,'SAT #',2,0,0)
      call wtstr (0.50,0.96,nf(inf),2,0,0)
c
      call wtstr (0.35,0.93,'ALG',2,0,0)
      call wtstr (0.50,0.93,ialg(ia),2,0,0)
c
c
      call wtstr (0.35,0.87,'FOR STAT',2,0,0)
      call wtstr (0.50,0.87,istat(is),2,0,0)
      call wtstr (0.60,0.87,istat(is+1),2,0,0)
c
      call wtstr(0.35,0.84,'AGAINST ',2,0,0)
      call wtstr(0.50,0.84,iby,2,0,0)
c
      call wtstr(0.35,0.02,iby,1,0,0)
c
c
      call setusv ('II',ic5)
c
       call pwrit (0.03,0.35,ivar(iv),6,13,90,0)
      y1 = 0.10
      y2 = 0.60
      call setusv ('II',ic5)
      call set (0.1,0.90,y1,y2,xmin,xmax,ymin,ymax,1)
      call labmod('(f4.0)','(f4.0)',4,4,0,0,0,0,0)
      call periml(13,5,4,5)
      ic = 5
      nn= 2
      call plotdt (x1,x2,nn,ic)
      ic = 5 
      nn= 2
      call plotdt (x1,x2,nn,ic)
       ntd = 64
      if(i.eq.1) ic=1
      if(i.eq.2) ic=3
      if(i.eq.3) ic=2
       ic = 5
      call plotdt (x1,x2,ntd,ic)
      call CLSGKS
!---------------------------------------
      call OPNGKS
c
c--------- THE COLOR TABLE -----------------------
c
      call setusv ('IM',5)
c red
      CALL STClrs (10000,   00, 1500,10000,ic1)
cb green
      call stclrs (    0,10000,    0,10000,ic2)
c purple
      call stclrs ( 4000,    0,10000,10000,ic3)
C LIGht blue
      call stclrs ( 1500,10000,10000,10000,ic4)
c black
      call stclrs (    0,    0,    0,10000,ic5)
      call setusv ('II',ic2)
c
 1     call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)

c 
      call wtstr (0.40,0.99,'VECTOR CORRELATION  -',1,0,0)
      call setusv ('II',ic1)
      call wtstr (0.40,0.96,'TIME SERIES PLOT',1,0,0)
      call setusv ('II',ic4)
c
      write(head,888) intval
 888  format(1x,'TIME INTERVAL',I5) 
      call wtstr (0.35,0.93,head,1,0,0)
c
      write(head,889) ihr 
 889  format(1x,'HRS  every ' I2,' hrs')
      call wtstr (0.58,0.93,head,1,0,0)
c
      if(dd1.eq.25) write(head,886) dd1
      if(dd1.eq.10) write(head,879) dd1
 886  format('CURRENT',f5.0,'m   with')  
 879  format(' WIND  ',f5.0,'m   with')   
      call wtstr (0.40,0.90,head,1,0,0)
c
      call setusv ('II',ic1) 
      write(head,884) dd2
 884  format('CURRENTS',f5.0)
      call wtstr (0.55,0.88,head,1,0,0)
c
      call setusv ('II',ic4)
      write(head,883) dd3
 883  format(13x,f5.0,'m')
      call wtstr (0.55,0.88,head,1,0,0)
c
      call setusv ('II',ic5)
      call setusv ('II',ic2)
      call set (0.1,0.80,y1,y2,0.0,400.0,0.0,1.5,1)
c
      call labmod('(i3','(f3.1)',3,3,0,0,0,0,0)
      call periml(20,2,3,5)
      x1(1) = 0.0
      x2(1) = 1.414
      x1(2) = 420.0
      x2(2) = 1.414
      call setusv ('II',ic1)
      call plotdt(x1,x2,2,1,icorp)
      call setusv ('II',ic1)
      call plotdt(xnt,vc12,jp,1,icorp)
      call setusv ('II',ic4)
      call plotdt(xnt,vc13,jp,4,icorp)
      call setusv ('II',ic5)
      call plotdt(xnt,vc14,jp,5,icorp)
      call setusv ('II',ic2)
      call CLSGKS

!---------------------------------------------------------
        call ncarxyt(corr,corre,inc(1),nd,labl,nll,
     *  loc,nlc,xyt,1,pos(i))
        call frame
        CALL CLSGKS
!---------------------
      call OPNGKS
c
c--------- THE COLOR TABLE -----------------------
c
      call setusv ('IM',5)
c red
      CALL STClrs (10000,   00, 1500,10000,ic1)
cb green
      call stclrs (    0,10000,    0,10000,ic2)
c purple
      call stclrs ( 4000,    0,10000,10000,ic3)
C LIGht blue
      call stclrs ( 1500,10000,10000,10000,ic4)
c black
      call stclrs (    0,    0,    0,10000,ic5)
      call setusv ('II',ic5)
c
 1     call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
 
c
      call wtstr (0.40,0.99,'VECTOR CORRELATION  -',1,0,0)
      call setusv ('II',ic5)
      call wtstr (0.40,0.96,'TIME SERIES PLOT',1,0,0)
      call setusv ('II',ic5) 
c
      write(head,888) intval
 888  format(1x,'TIME INTERVAL',I5) 
      call wtstr (0.35,0.93,head,1,0,0)
c 
      write(head,889) ihr
 889  format(1x,'HRS  every ' I2,' hrs')
      call wtstr (0.58,0.93,head,1,0,0)
c
      if(dd1.eq.25) write(head,886) dd1
      if(dd1.eq.10) write(head,879) dd1
 886  format('CURRENT',f5.0,'m   with')  
 879  format(' WIND  ',f5.0,'m   with')
      call wtstr (0.40,0.90,head,1,0,0)
c
      call setusv ('II',ic5)
      write(head,884) dd2
 884  format('CURRENTS',f5.0)
      call wtstr (0.55,0.88,head,1,0,0)
c
      call setusv ('II',ic5)
      write(head,883) dd3
 883  format(13x,f5.0,'m')
      call wtstr (0.55,0.88,head,1,0,0)
      call setusv ('II',ic5)
      call setusv ('II',ic2)
      call set (0.1,0.80,y1,y2,0.0,400.0,0.0,1.5,1)
c
      call labmod('(i3','(f3.1)',3,3,0,0,0,0,0)
      call periml(20,2,3,5)
c
      x1(1) = 0.0
      x2(1) = 1.0
      x1(2) = 420.0
      x2(2) = 1.0
      call setusv ('II',ic1)
      call plotdt(x1,x2,2,1,icorp)
      call setusv ('II',ic1)
      call plotdt(xnt,vc12,jp,1,icorp)
      icorp=1
      call setusv ('II',ic4)
      call plotdt(xnt,vc13,jp,4,icorp)
      call setusv ('II',ic5)
      call frame
 101  continue
cc
 600  continue
      call CLSGKS
!------------------------------------------------------------
        CALL OPNGKS
        call ncarxyt(corr,corre,inc(1),nd,labl,nll,
     *  loc,nlc,xyt,1,pos(i))
        CALL CLSGKS
!--------------------------------------------------------------
       CALL OPNGKS
       call setclr(ibckb)
       CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
        call setusv ('II',i12)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
C
       iymd = iy*10000 + im*100 + id
       ihm = ih*100 + imn
       write(6,625) iymd,ihm
  625  format(1h ,' YMD',i9,'  HM',i5)
c
       write(head0,876) head8,iymd,ihm
 876  format (a13,'  YMD',i9,'   HM',i5)
      print *,'head=',head0
      call wtstr (.50,.99,head0,2,0,0)
       call wtstr (.40,.96,head0,1,0,0)
  59  continue
c
        no = 9
      if (no.eq.0) then                                                 


          call setusv ('II',i3)                               
          call wtstr (.25,.82,'*** NO DATA AVAILABLE ***',2,0,0)
          call wtstr (.25,.80,'*************************',2,0,0)
          call frame
          go to 444                                           
      endif                                                   
      call setusv ('II',i12)                                  
      if (ibckb.eq.1) call setusv ('II',i2)                   
      call cpsetr ('VPB - VIEWPORT BOTTOM',.20)               
      call supmap (jproj,klat,rlon,rot,                       
     1        ymin,-xmax,ymax,-xmin,-2,10,1,idot,ier)
      if (ier.ne.0) print *,'supmap error=',ier
      CALL GETSET(fl,fr,fb,ft,ul,ur,ub,ut,1)
      CALL SET(fl,fr,fb,ft,-xmax,-xmin,ymin,ymax,1)
c
      CALL LABMOD ('(f5.0)','(f5.0)',5,5,1,1,0,0,0)
      CALL PERIML (1,1,1,1)
 55   continue
      if(mod(nobs,1).eq.0)
     * call draw(ivar,isid,norb,nobs,xlat,xlon,swnn,nrfnn,wv,ql,sst)
c
       go to 222
 444  continue
      if(nobs.le.1) write(6,652) np,idsat(isat),head3(isat)
 652  format(1h ,' NO DATA for SSMI sat',i4,a4,'  NP',i3)
      if(nobs.gt.1) write(6,654) np,idsat(isat),head3(isat),nobs,
     *in
 654  format(1h ,' COMPLETE PLOT #',i3,' for SSMI sat',i4,a4,
     *'  NOBS',i4,'  rewind on in',i3,' & call frame')
       rewind in
       call frame
c
 9999 CALL CLSGKS
!-------------------------------------------------------
      call OPNGKS
c
c--------- THE COLOR TABLE -----------------------
c
      call setusv ('IM',5)
c red
      CALL STClrs (10000,   00, 1500,10000,ic1)
cb green
      call stclrs (    0,10000,    0,10000,ic2)
c purple
      call stclrs ( 4000,    0,10000,10000,ic3)
C LIGht blue
      call stclrs ( 1500,10000,10000,10000,ic4)
c black
      call stclrs (    0,    0,    0,10000,ic5)
      call setusv ('II',ic1)
c
      call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
c
      call wtstr (0.40,0.99,'QSCAT vs MODEL ANL -- by CELL NUMBER -',
     * 2,0,0) 
      call wtstr (0.28,0.96,'SPEED DIFFERENCES (m/s)',2,0,0)
      call setusv ('II',ic5)
      write(head,888) nrf
 888  format(a28) 
      call wtstr (0.10,0.94,head,1,0,0)
      write(head,888) nw1
      call wtstr (0.25,0.94,head,1,0,0)
      write(head,888) nw2
      call wtstr (0.40,0.94,head,1,0,0)
      write(head,888) jswv
      call wtstr (0.30,0.88,head,1,0,0)
      call wtstr (0.40,0.92,head,1,0,0)
c
      write(head2,887) n2  
c 887  format(1x,'Number of data points',i10)
      call wtstr (0.42,0.90,head2,1,0,0)
      call setusv ('II',ic2)
      call wtstr (0.30,0.83,'Swath BIAS =',1,0,0)
      write(head,881) bbb
 881  format(1x,f6.2)
      call wtstr (0.50,0.83,head,1,0,0)

      write(head,881) rrr
      call wtstr (0.50,0.86,head,1,0,0)
      call wtstr (0.30,0.86,'Swath RMS = ',1,0,0)
      call setusv ('II',ic5)
      call wtstr (0.10,0.03,' CELL #',1,0,0)

  48  format(1x,'Figure #',i2)
      nfig = nfig + 1

      call setusv ('II',ic5)

      call set (0.1,0.80,y1,y2,0.0,72.,-1.0,6.0,1)
c
      call labmod('(f4.0)','(f5.1)',4,5,0,0,0,0,0)

      call periml(9,8,14,1)
      call plotdt(x1,x2,2,iflg2,icorp)
      x1(1) = 0.0
      x2(1) = 0.94 + xinc*xl
      x1(2) = 72
      x2(2) = 0.94 + xinc*xl
      call plotdt(x1,x2,2,iflg2,icorp)
      x1(1) = 0.0
      x2(1) = 1.94 + xinc*xl
      x1(2) = 72
      x2(2) = 1.94 + xinc*xl
      call plotdt(x1,x2,2,iflg2,icorp)
      x1(1) = 0.0
      x2(1) = 3.0
      x1(2) = 72
      x2(2) = 3.0

      np = 0
      do 25 i=3,74

 630  format(1h ,'nc',i3,'  xss',f6.0)
      if(xss(i).eq.0.0) go to 25
      np = np + 1 
      x1(np) = xc(i)
      x2(np) = -bs(i) - 0.06 + xinc*xl
      write(6,631) np,x1(np),x2(np)
 631  format(1h ,'np',i3,'  x1,x2',2f6.1)
  25  continue
      call plotdt(x1,x2,np,2,icorp)
      call plotdt(x1,x2,np,1,icorp)
 110   continue
      call frame
c
      call setusv ('II',ic1)
c
      call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
c
      call wtstr (0.40,0.99,'QSCAT vs MODEL ANL -- by CELL NUMBER -',
     * 2,0,0)
      call wtstr (0.34,0.96,'DIRECTION DIFFERENCES (Degrees)',2,0,0)
      call setusv ('II',ic1)
      call setusv ('II',ic5)
      write(head,888) nrf
c 888  format(1x,a20)
      call wtstr (0.10,0.94,head,1,0,0)
      write(head,888) nw1
      call wtstr (0.25,0.94,head,1,0,0)
      write(head,888) nw2
      call wtstr (0.40,0.94,head,1,0,0)
      n3 = sn3
      bbb = sdd/sn3
      rrr = sqrt(sdd2/sn3)

c 611  format(1h ,i8,2f6.1)
      write(head,887) mndte,mxdte
  887  format(1x,i10,3x,i10)
      call wtstr (0.40,0.92,head,1,0,0)
      call wtstr (0.42,0.90,head2,1,0,0)
      write(head,888) jswv
      call wtstr (0.30,0.88,head,1,0,0)
      call setusv ('II',ic2)
      call wtstr (0.30,0.83,'Swath BIAS = ',1,0,0)
      write(head,881) bbb
      call wtstr (0.50,0.83,head,1,0,0)
      call setusv ('II',ic1)
      call wtstr (0.30,0.86,'Swath RMS = ',1,0,0)
      write(head,881) rrr
      call wtstr (0.50,0.86,head,1,0,0)
      call setusv ('II',ic5)
      call wtstr (0.10,0.03,' CELL #',1,0,0)

c  48  format(1x,'Figure #',i2)

c............................
           y1 = 0.08
           y2 = 0.80
      iflg2 = 2
      iflg1 = 1
      icorp = 0
c ...............................................................
       call setusv ('II',ic5)
c23456
       call set (0.1,0.80,y1,y2,0.0,72.0,-10.0,120.0,1)
c
      call labmod('(f4.0)','(f5.0)',4,5,0,0,0,0,0)
      call periml(9,8,13,1)
      call plotdt(x1,x2,2,iflg2,icorp)
c
      x1(1) = 0.0
      x2(1) = 0.0 - 0.4 + xinc*xl
      x1(2) = 72
      x2(2) = 0.0 - 0.4 + xinc*xl
      call plotdt(x1,x2,2,iflg2,icorp)
c
      np = 0
      do 125 i=3,74
      if(xdd(i).eq.0.0) go to 125
      np = np + 1
      x1(np) = xc(i)
      x2(np) = bd(i) - 0.4+ xinc*xl
      write(6,631) np,x1(np),x2(np)
 125  continue
      call plotdt(x1,x2,np,2,icorp)
      np = 0
      do 126 i=3,74
      if(xdd(i).eq.0.0) go to 126
      np = np + 1
      x1(np) = xc(i)
      x2(np) = rd(i) - 0.4 + xinc*xl
      write(6,631) np,x1(np),x2(np)
 126  continue
      call plotdt(x1,x2,np,1,icorp)
 120   continue
      call frame 
c
        end file in
 1000  continue

       write(6,1601)
 1601 format(1h ,'END PLOT')
 600  continue
c 1002  continue 
      call CLSGKS
!--------------------------------------------------------------
      call setusv ('II',i5)
c
      call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)

c
      call wtstr (0.42,0.99,'QuikSCAT VIEWING GEOMETERY ',2,0,0)
      call wtstr (0.40,0.96,'TWO ANTENNAE at 54 & 46 Degree ',1,0,0)
      call wtstr (0.65,0.96,'    INCIDENT ANGLES ',1,0,0)
      call wtstr (0.30,0.93,'SWATH 1800 km',1,0,0)
      call wtstr (0.35,0.90,'LIMITATIONS:',1,0,0)
      call wtstr (0.30,0.87,'  OUTER EDGES -',1,0,0)
      call wtstr (0.55,0.87,'ONLY ONE SCANNING ANTENNA',1,0,0)
      call wtstr (0.37,0.84,'  NADIR - SMALL LOOK ANGLES',
     *1,0,0)
      call setusv ('II',i1)
      call wtstr (0.44,0.76,'   1800 KM',2,0,0)
      call wtstr (0.44,0.55,'NADIR',2,0,0)
      call wtstr (0.14,0.55,'EDGE',2,0,0)
      call wtstr (0.14,0.46,'200KM',2,0,0)
      call wtstr (0.76,0.46,'200KM',2,0,0)
      call wtstr (0.76,0.55,'EDGE',2,0,0)
      call wtstr (0.45,0.46,'1400 KM',2,0,0)

      call setusv ('II',ic5)
      call setusv ('II',ic5)
      call set (0.1,0.80,y1,y2,0.0,1800.0,0.0,1800.0,1)
c
      call labmod('(i4)','(i4)',4,4,0,0,0,0,0)
      call periml(18,1,18,1)
c
      x1(1) = 900.0
      y1(1) = 0.0
      x1(2) = 900.0
      y1(2) = 1800.0
      n=2
          call curve (x1,y1,n)
c
      x1(1) = 900.0
      y1(1) = 0.0
      x1(2) = 900.0
      y1(2) = 1800.0
      n=2
          call curve (x1,y1,n)
      x1(1) = 900.0
      y1(1) = 0.0
      x1(2) = 900.0
      y1(2) = 1800.0
      n=2
          call curve (x1,y1,n)
      x1(1) = 880.0
      y1(1) = 1710.0
      x1(2) = 900.0
      y1(2) = 1800.0
      n=2
          call curve (x1,y1,n)
          call curve (x1,y1,n)
c
      x1(1) = 200.0  
      y1(1) = 900.0
      x1(2) = 1600.0
      y1(2) = 900.0
      n=2 
          call curve (x1,y1,n)
c
      x1(1) = 0.0  
      y1(1) = 1650.0
      x1(2) = 1800.0
      y1(2) = 1650.0
      n=2 
          call curve (x1,y1,n)
c
      do 51 i=1,21
      x1(1) = 700.0
      y1(1) = 0.0  + (i-1)*200.
      x1(2) = 700.0
      y1(2) = 100.0 + (i-1)*200.
      n=2
          call curve (x1,y1,n)
  51    continue
c
      do 52 i=1,21
      x1(1) = 1100.0
      y1(1) = 0.0  + (i-1)*200.
      x1(2) = 1100.0 
      y1(2) = 100.0 + (i-1)*200.
      n=2 
          call curve (x1,y1,n)
          call curve (x1,y1,n)
  52    continue
c
      do 53 i=1,21
      x1(1) = 200.0
      y1(1) = 0.0  + (i-1)*200.
      x1(2) = 200.0
      y1(2) = 100.0 + (i-1)*200.
      n=2
          call curve (x1,y1,n)
  53    continue
c
      do 54 i=1,21
      x1(1) = 1600.0
      y1(1) = 0.0  + (i-1)*200.
      x1(2) = 1600.0
      y1(2) = 100.0 + (i-1)*200.
      n=2
          call curve (x1,y1,n)
  54    continue
c
c
      n=37
          call curve (cx11,cy11,n)
          call curve (cx21,cy21,n)
          call curve (cx12,cy12,n)
          call curve (cx22,cy22,n)
          call curve (cx13,cy13,n)
          call curve (cx23,cy23,n)
          call points (xx1,xx2,1,ich,if)
      call CLSGKS
!------------------------------------------------------------------
      CALL OPNGKS
      call setusv ('IM',13)
      read (5,501) ibckb
 501   format (i2)
      if (ibckb.eq.0) then
c       light yellow
        call stclrs (10000,10000, 5000,10000,i1)
        call stclrs (10000, 5500, 5500,10000,i2)
        call stclrs ( 5000, 0000,10000,10000,i3)
        call stclrs ( 0000, 0000,10000,10000,i4)
        call stclrs ( 0000, 5000,10000,10000,i5)
        call stclrs ( 0000,10000,10000,10000,i6)
        call stclrs ( 0000,10000, 6000,10000,i7)
        call stclrs ( 4000, 4000, 1000,10000,i8)
        call stclrs (10000, 7000, 0000,10000,i9)
        call stclrs ( 9000, 4000, 1000,10000,i10)
        call stclrs (10000, 0000, 0000,10000,i11)
c       black
        call stclrs ( 0000, 0000, 0000,10000,i12)
      else
        call stclrs ( 0000, 7000, 7000,10000,i1)
        call stclrs (10000, 6000, 6000,10000,i2)
        call stclrs ( 5000, 0000,10000,10000,i3)
        call stclrs ( 0000, 0000,10000,10000,i4)
        call stclrs ( 0000, 5000,10000,10000,i5)
        call stclrs ( 0000,10000,10000,10000,i6)
        call stclrs ( 0000,10000, 4000,10000,i7)
        call stclrs ( 7000,10000, 0000,10000,i8)
        call stclrs (10000, 7000, 0000,10000,i9)
        call stclrs ( 9000, 4000, 1000,10000,i10)
        call stclrs (10000, 0000, 0000,10000,i11)
c       black
        call stclrs ( 0000, 0000, 0000,10000,i12)
        call gscr (1,13,.70,.70,.70)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      if (ibckb.eq.1) then
c         use next extra 3 lines if you want a black background
c         =====================================================
         call gsfaci (i13)
         call gsfais (1)
         call gfa (4,x,y)
      endif
         call setusv ('II',i2)
c     endif
      call wtstr (.55,.99,head1,1,0,0)
c
      write(head1,877) hhhead
 877  format (a37)
      call wtstr (.55,.97,head1,1,0,0)
      write(head1,877) hhead
      call wtstr (.55,.95,head1,1,0,0)
      if (np.eq.0) then
          xmin = -180.0
          xmax = 180.0
          ymin = -90.0
          ymax = 90.0 
          call setusv ('II',i3)
          call wtstr (.25,.82,'*** NO DATA AVAILABLE ***',2,0,0)
          call wtstr (.25,.80,'*************************',2,0,0)
      call setusv ('II',i12)
      if (ibckb.eq.1) call setusv ('II',i2)
      call cpsetr ('VPB - VIEWPORT BOTTOM',.20)
      call supmap (jproj,klat,rlon,rot,
     1        ymin,-xmax,ymax,-xmin,-2,10,1,idot,ier)
      if (ier.ne.0) print *,'supmap error=',ier
      CALL GETSET(fl,fr,fb,ft,ul,ur,ub,ut,1)
      CALL SET(fl,fr,fb,ft,-xmax,-xmin,ymin,ymax,1)
c
      CALL LABMOD ('(f5.0)','(f5.0)',5,5,0,0,0,0,0)
      CALL PERIML (1,1,1,1)
      if (np.gt.0)
     1   call winds(np,xlat,xlon,iwspd,iwdir,i444,ibckb,
     1   isv)
      call frame
 9999 CALL CLSGKS
!------------------------------------------------------------------------
      call setusv ('II',ic5)
c
      call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
c
      call wtstr(0.25,0.09,'QuikSCAT vs MODEL ANALYSES ',
     * 2,0,0) 
c      call wtstr(0.25,0.96,'by CELL NUMBER',2,0,0)
c
      call wtstr (0.25,0.61,'SPEED DIFFERENCES (m/s)',1,0,0)
      call setusv ('II',ic5) 
      write(head,888) nrf
 888  format(a28)
      call wtstr (0.05,0.14,head,1,0,0)
c 
      write(head,888) nw1
      call wtstr (0.15,0.14,head,1,0,0)
      write(head,888) nw2
      call wtstr (0.25,0.14,head,1,0,0)

c
      n2 = sn2 
      bbb = -ssd/sn2
      rrr = sqrt(ssd2/sn2) 
      call wtstr (0.10,0.12,'DATES ',1,0,0)
      call wtstr (0.30,0.12,head2,1,0,0)
 887  format(7x,i10,3x,i10)
c
      write(head2,883) n2
 883  format(1x,'Number of data points',i6,'000')
      call wtstr (0.35,0.59,head2,1,0,0)
c
      call setusv ('II',ic2)
      call wtstr (0.15,0.57,'Swath BIAS =',1,0,0)
      write(head,881) bbb
 881  format(1x,f6.2)
      call wtstr (0.35,0.57,head,1,0,0)
c
      call setusv ('II',ic1)
      call wtstr (0.33,0.57,' RMS = ',1,0,0)
      write(head,881) rrr
      call wtstr (0.50,0.57,head,1,0,0)
      call setusv ('II',ic5)
      call wtstr (0.20,0.64,' CELL NUMBER',1,0,0)
      call setusv ('II',ic5)
c     call set (0.1,0.80,y1,y2,0.0,80.,-0.5,4.5,1)
      call set (0.1,0.45,y1,y2,0.0,72.,-1.0,6.0,1)
c
      call labmod('(f4.0)','(f5.1)',4,5,0,0,0,0,0)
c      call periml(10,1,10,1)
      call periml(9,8,14,1)     
c
      x1(1) = 0.0
      x2(1) = 0.0 
      x1(2) = 72 
      x2(2) = 0.0   
      call plotdt(x1,x2,2,iflg2,icorp)
      x1(1) = 0.0
      x2(1) = 1.0
      x1(2) = 72  
      x2(2) = 1.0   
      call plotdt(x1,x2,2,iflg2,icorp)
      x1(1) = 0.0
      x2(1) = 2.0 
      x1(2) = 72 
      x2(2) = 2.0   
      call plotdt(x1,x2,2,iflg2,icorp)
      x1(1) = 0.0
      x2(1) = 3.0 
      x1(2) = 72
      x2(2) = 3.0   
c      call plotdt(x1,x2,2,iflg2,icorp)
      call plotdt(x1,x2,np,2,icorp)
      np = 0
      do 26 i=1,76
      if(xss(i).eq.0.0) go to 26
      np = np + 1
      x1(np) = xc(i)
      x2(np) = rs(i)
      write(6,631) np,x1(np),x2(np)
  26  continue
      call plotdt(x1,x2,np,1,icorp)
      call setusv ('II',ic5)
c
      call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
c 
c      call wtstr (0.40,0.99,'QSCAT vs MODEL ANL -- by CELL NUMBER -',
c     * 2,0,0)
      call wtstr (0.30,0.21,'DIRECTION DIFFERENCES (Degrees)',1,0,0)
      call setusv ('II',ic1)
      call setusv ('II',ic5)
      call wtstr (0.35,0.17,head,1,0,0)
      call setusv ('II',ic1)
      call wtstr (0.33,0.17,' RMS = ',1,0,0)
      write(head,882) rrr
      call wtstr (0.50,0.17,head,1,0,0)
      call setusv ('II',ic5)
      call wtstr (0.20,0.24,' CELL NUMBER',1,0,0)
       call setusv ('II',ic5)

c23456
       call set (0.1,0.45,y1,y2,0.0,72.0,-10.0,120.0,1)
c
      call labmod('(f4.0)','(f5.0)',4,5,0,0,0,0,0)
      call periml(9,8,13,1)
      call plotdt(x1,x2,2,iflg2,icorp)
c
      np = 0
      do 125 i=3,74
c      write(6,630) i,xss(i)
c 630  format(1h ,'nc',i3,'  xss',f6.0)
      if(xdd(i).eq.0.0) go to 125
      np = np + 1
      x1(np) = xc(i)
      x2(np) = bd(i)
      write(6,631) np,x1(np),x2(np)
 125  continue
      call plotdt(x1,x2,np,2,icorp)
      np = 0
      do 126 i=3,74  
      if(xdd(i).eq.0.0) go to 126
      np = np + 1  
      x1(np) = xc(i)
      x2(np) = rd(i)
      write(6,631) np,x1(np),x2(np)
 126  continue
      call plotdt(x1,x2,np,1,icorp)
c
      call frame 
      call CLSGKS
!-----------------------------------------------------------------------
      call setusv ('II',i12)                                  
      if (ibckb.eq.1) call setusv ('II',i2)                   
      call cpsetr ('VPB - VIEWPORT BOTTOM',.20)               
      call supmap (jproj,klat,rlon,rot,                       
     1        ymin,-xmax,ymax,-xmin,-2,10,1,idot,ier)         
      if (ier.ne.0) print *,'supmap error=',ier               
      CALL GETSET(fl,fr,fb,ft,ul,ur,ub,ut,1)                  
      CALL SET(fl,fr,fb,ft,-xmax,-xmin,ymin,ymax,1)           
c                                                             
      CALL LABMOD ('(f5.0)','(f5.0)',5,5,2,2,0,0,0)           
      CALL PERIML (1,1,1,1)    
!-------------------------------------------------------
