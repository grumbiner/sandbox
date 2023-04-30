c     program in /wd2/wd21/wd21wg/stats/scatplt.jcl 
c     ucl        /wd2/wd21/wd21rt/altbystt.jcl
c     data was prepared on NAS 9000 by 'nws.wd21.dce.file(jclrers)'  r.t.
c
c
c     written : June 19 1992     Rachel Teboulle
c
c     ==============================================
c      dimension buydat(3000),anldat(3000),SAT1(3000),SAT2(3000)
       dimension x(4,3000),x1(3000),x2(3000),valmax(4)
       dimension sumx(6),sumx2(6),sumy(6),sumy2(6),sumxy(6)
       dimension sumd(6),sumd2(6) 
       dimension avg1(6),avg2(6),rms(6),bias(6),sd1(6),sd2(6)
       dimension corr(6),a0(6),a1(6) 
      common/savcl/ ic1,ic2,ic3,ic4,ic5
      character*9 month(12)
      character head*50,buoyid*8
      character*4 ylab(6),xlab(6),lab1,lab2
      real win(2,2)
      data month/'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     1           'MAY      ','JUNE     ','JULY     ',
     1           'AUGUST   ','SEPTEMBER','OCTOBER  ','NOVEMBER ',
     1           'DECEMBER '/
      data ylab/'BUOY','BUOY','BUOY',' ANL',' ANL','SAT1'/
      data xlab/' ANL','SAT1','SAT2','SAT1','SAT2','SAT2'/
       data win/0.05,0.45,
     1         0.50,0.90/
c
      call OPNGKS
      call setusv ('IM',5)
      call stclrs (10000, 1500, 1500,10000,ic1)
      call stclrs (    0,10000,    0,10000,ic2)
      call stclrs ( 3000,    0,10000,10000,ic3)
      call stclrs (10000,    0,10000,10000,ic4)
      call stclrs ( 3000, 3000, 1000,10000,ic5)
c
      ifile = 21
 10   iwnw=1
      do 9 i=1,4
      valmax(i) = 0.
  9   continue
      n=0

      
c---------------------------------------------------------------
      read(ifile,1) id1,id2,lab1,lab2
  1   format(2i4,2a4)
      write(6,606) id1,id2,lab1,lab2
 606  format(1h ,'data matchups from',2i4,3x,2a6)
c
 11   continue   
      n = n + 1

C  DATA buoy, anl, sat1 sat2
      read(ifile,2) (x(i,n),i=1,4)
  2   format(4f6.1)
      write(6,611) n,(x(i,n),i=1,4)
 611  format(1h ,'data - buoy, anl, sat1, sat2',i4,4f6.1)
      if(x(1,n).gt.80.0) go to 22
      do 17 i=1,4
      If(valmax(i).lt.x(i,n)) valmax(i) = x(i,n)
  17  continue
      go to 11   
  22  continue 
      n = n-1
      xn = n
      rn = n 
      if(id1.ne.10.and.id2.eq.10) go to 10
c -------------------------------------------------------------c
c    
c
c     i999=1/2/3/4 for east coast/west coast/hawaii/the whole data
ccc      do 999 i999 = 1,4
      i999 = 1
c     =================
      call setusv ('II',ic1)
c
      call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
c
      call wtstr (0.58,0.99,'OCEAN SFC  WIND MATCH-UP STATISTICS',2,0,0)
c      ifile = ifile + 1
         call wtstr (0.58,0.96,'DATA PLATFORMS',1,0,0)
         call wtstr (0.52,0.93,lab1,1,0,0)
         call wtstr (0.58,0.93,'vs',1,0.0)
         call wtstr (0.64,0.93,lab2,1,0,0)
      call wtstr (0.58,0.90,'speed adjusted to 10m height',1,0,0)
c
ccc
c      do 101 iwnw=1,2

      do 8 k=1,6
      sumx(k)  = 0.0
      sumx2(k) = 0.0
      sumy(k)  = 0.0
      sumy2(k) = 0.0
      sumxy(k) = 0.0
      sumd(k)  = 0.0
      sumd2(k) = 0.0
  8   continue
c ---------------------------------------------------------------
      do 25 m=1,n
      k = 0
      do 25 i=1,3
      ip = i+ 1
      do 25 j=ip,4 
      k =k + 1 
c
      sumd(k)  = sumd(k)  +  x(i,m) - x(j,m)  
      sumd2(k) = sumd2(k) + (x(i,m) - x(j,m))**2 
      sumx(k)  = sumx(k)  +  x(i,m)
      sumx2(k) = sumx2(k) +  x(i,m)**2
      sumy(k)  = sumy(k)  +  x(j,m)
      sumy2(k) = sumy2(k) +  x(j,m)**2 
      sumxy(k) = sumxy(k) +  x(i,m)*x(j,m)
  25  continue
      if(n.le.2) go to 28
      do 30 k=1,6
      bias(k) = sumd(k)/xn
      rms(k)  = sqrt(sumd2(k)/xn)
      avg1(k) = sumx(k)/xn
      avg2(k) = sumy(k)/xn
      sd1(k)  = sqrt(sumx2(k)/xn - avg1(k)**2)
      sd2(k) = sqrt(sumy2(k)/xn - avg2(k)**2)       
      corr(k) = (xn*sumxy(k) - sumx(k)*sumy(k))/
     *sqrt((xn*sumx2(k)-avg1(k)**2)*(xn*sumy2(k) 
     *-avg2(k)**2))
      a0(k) = (sumy(k)*sumx2(k) - sumx(k)*sumxy(k))/
     *(xn*sumx2(k) - sumx(k)**2)
      a1(k) = (xn*sumxy(k) - sumx(k)*sumy(k))/
     *(xn*sumx2(k) - sumx(k)**2)
c      corr(k) = 0.0
  30  continue
  28  continue

        anot = 0.0
        a1 = 1.0
        conf90 = 0.0 
c
c .............................................................
      k=0
      do 60 i=1,3  
      ip = i + 1
      do 60 j=ip,4
      k = k+ 1 
c  .  .  .  .  .  .  .  .  .  .  .  .   .  .  .  .  .  .  .  .  .  
      write(6,607) k,n,lab1,lab2,xlab(k),ylab(k)
  607 format(1h ,2i3,'for platforms  ',2a6,'  matched with ',
     *a4,' vs',a4)
      if(k.gt.1) go to 60
      call setusv ('II',ic3)
      y1 = win(1,iwnw)
      y2 = win(2,iwnw)
c     print *,'y1,y2,n,rn=',y1,',',y2,',',n,',',rn
      call set (0.0,1.0,y1,y2,0.0,1.0,0.0,1.0,1)
        call wtstr (0.60,0.95,xlab(k),2,0,0)
        call wtstr (0.65,0.95,'vs',2,0,0)
        call wtstr (0.71,0.95,ylab(k),2,0,0)
      call setusv ('II',ic5)
      write (head,4) n
 4    format ('no.of data points=',i6)
      call wtstr (0.90,0.85,head,2,0,0)
      write (head,41) rms(k)
 41   format ('rms  =',f7.2)
      call wtstr (0.90,0.78,head,2,0,0)
      write (head,42) bias(k)
 42   format ('bias =',f7.2)
      call wtstr (0.90,0.71,head,2,0,0)
      write (head,43) corr(k)
 43   format ('cor.coef. =',f7.2)
      call wtstr (0.90,0.64,head,2,0,0)
      write(head,44) a0(k)
 44   format ('a0 =',f7.2) 
      call wtstr (0.90,0.57,head,2,0,0)
      write(head,45) a1(k)
 45   format('ai  =',f7.1)
      call wtstr (0.90,0.50,head,2,0,0)

c
      call wtstr(0.25,0.00,ylab(k),1,0,0)
         call pwrit (0.03,0.65,xlab(k),4,13,90,0)
c

      call set (0.1,0.45,y1+0.05,y2,0.0,rn,0.,rn,1)
c     draw the 45 deg.line.
      call dashdc ('$''$',10,12)
      call lined (0.0,0.0,rn,rn)
      call dashdc ('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$',10,12)
      ivalmx = valmax(i) + 1.0
      valmax(i) = ivalmx
      jvalmx = valmax(j) + 1.0
      valmax(j) = jvalmx
      ux1 = 0.
      uy1 = anot
      ux2 = valmax(i)
      uy2 = valmax(j)
      if(ux2.lt.uy2) uy2 = ux2
      if(uy2.lt.ux2) ux2 = uy2
c     draw the reg.line.
      print *,'ux1,ux2,uy1,uy2=',ux1,',',ux2,',',uy1,',',uy2
      call set (0.1,0.45,y1,y2,0.0,valmax(i),0.0,valmax(j),1)
      call labmod ('(f3.0)','(f3.0)',3,3,0,0,0,0,0)
      call periml (ivalmx,2,jvalmx,2)
      call line (ux1,uy1,ux2,uy2)
      do 61 kn=1,n
      x1(kn) = x(i,kn)
      x2(kn) = x(j,kn)
  61  continue  
c
      call plotdt (x1,x2,n,0)
c
 101  continue
  60  continue
      call frame
c ............................................................
 999  continue
      call CLSGKS
c
      STOP
      end
cc
c
c
c
      subroutine plotdt (x1,x2,n,iflag)
c     =========================================
c
      common/saved/ imonth,ndays
      common/savcl/ ic1,ic2,ic3,ic4,ic5
      dimension x1(3000),x2(3000)
c
      print *,'no. points passed to sub plotdt =',n
      if (iflag.eq.1) then
          call setusv ('II',ic3)
          call curved (x1,x2,n)
      else
          call setusv ('II',ic4)
          ich = ichar('*')
c         iflag=0/1 for markers only /connect with a line
          iflag = 0
          call points (x1,x2,n,ich,iflag)
      endif
      return
      end
c
      subroutine stclrs (iir,iig,iib,index,iout)
c     ==========================================
c
      call setusv ('IR',iir)
      call setusv ('IG',iig)
      call setusv ('IB',iib)
      call setusv ('IN',index)
      call getusv ('II',iout)
      return
      end
c
