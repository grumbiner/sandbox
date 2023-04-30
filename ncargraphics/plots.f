c -------------------------------------------------------------c
      subroutine plotxy(k,i,j,np,lab1,lab2,x,y,xn,
     *avx,avy,sdx,sdy,bias,rms,corr)
c
c for comparison within a single sat
c
      dimension x(1),y(1),win(2,2)
      dimension ic(5)
      character*5 lab1,lab2,nsat(4)
      character head*50,buoyid*8
      data win/0.05,0.45,
     *         0.50,0.90/
c
      data nsat/'  f10','  f13',' ers1','    '/
c      common/savcl/ic1,ic2,ic3,ic4,ic5
      iwnw = 1
      inp = 10  
      if(np.gt.inp) return 
c
       write(6,635) np,k,nsat(k)
 635  format(1h ,'+++ BEGIN plot #',i3,'  for',i3,a6,' +++')
      n = xn
      xmax = 30.0
      ymax = 30.0
      do 10 m=1,n
      if(x(m).lt.0.0.or.x(m).gt.xmax.or.y(m).lt.0.0.or.y(m).gt.ymax)
     *write(6,629) m,x(m),y(m),xmax,ymax
 629  format(1h ,'data out of range  for m=',i6,' x,y',2f6.0,'xmax,ymax',
     *2f6.0)
      if(x(m).gt.xmax) x(m) = xmax
      if(y(m).gt.ymax) y(m) = ymax
  10  continue
      write(6,636) n,xmax,ymax,i,j 
 636  format(1h ,'n ',i6,'  xmax,ymax',2f6.1,'  i,j',2i6)
      i999 = 1
      xmax = 30.0
      ymax = 30.0
c     =================
      if(np.gt.1) go to 75
      call OPNGKS
      call setusv ('IM',5)
      call stclrs(10,10,10,10,ic(1))
      call stclrs(10, 5, 5,10,ic(2))
      call stclrs(10, 0,10,10,ic(3))
      call stclrs( 0, 5,10,10,ic(4))
      call stclrs( 0,10, 0,10,ic(5))
  75  continue
      write(6,619) ic(1),ic(2),ic(3),ic(4),ic(5)
  619  format(1h ,'ic1,ic2,ic3,ic4,ic5',5i5)
      write(6,675) 
  675 format(1h ,'after 75')
      call setusv('IM',5)
c
      call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
c
      call wtstr (0.38,0.99,'OCEAN SFC  WIND MATCH-UP STATISTICS',2,0,0)
c      ifile = ifile + 1
       call setusv('II',ic(1))
      write(6,676)
 676  format(1h ,'after setusv')
         call wtstr (0.48,0.96,'DATA PLATFORMS',1,0,0)
         call wtstr (0.42,0.93,lab1,1,0,0)
         call wtstr (0.48,0.93,'vs',1,0.0)
         call wtstr (0.54,0.93,lab2,1,0,0)
      call wtstr (0.48,0.90,'Speed (M/S) Adjusted to 10m Height',1,0,0)
c
ccc
c  .  .  .  .  .  .  .  .  .  .  .  .   .  .  .  .  .  .  .  .  .
      write(6,607) k,nsat(k),i,j,lab1,lab2
  607 format(1h ,i3,' for platforms  ',a6,' i,j',2i4,'  matched with ',
     *a5,' vs',a5)
      y1 = win(1,iwnw)
      y2 = win(2,iwnw)
      write(6,638) y1,y2
  638 format(1h ,' y1,y2',2f6.1)
C      call set (0.0,1.0,y1,y2,0.0,1.0,0.0,1.0,1)
c        call wtstr (0.60,0.95,lab1,2,0,0)
c        call wtstr (0.65,0.95,'vs',2,0,0)
c        call wtstr (0.71,0.95,lab2,2,0,0)
      call setusv('II',ic(2))
      write(6,639)
 639  format(1h ,'start headers')
      write (head,4) n
 4    format ('no.of data points=',i6)
      call wtstr (0.50,0.85,head,2,0,0)
      write (head,41) rms
 41   format ('rms  =',f7.2)
      call wtstr (0.50,0.82,head,2,0,0)
      write (head,42) bias
 42   format ('bias =',f7.2)
      call wtstr (0.50,0.79,head,2,0,0)
      write (head,43) corr
 43   format ('corr. coef. =',f7.2)
      call wtstr (0.50,0.76,head,2,0,0)
c      write(head,44) a0(k)
c 44   format ('a0 =',f7.2)
c      call wtstr (0.90,0.57,head,2,0,0)
c      write(head,45) a1(k)
c 45   format('ai  =',f7.1)
c      call wtstr (0.90,0.50,head,2,0,0)
         write(6,647)
 647  format(1h ,'end headers')

c
      call wtstr(0.25,0.05,lab1,1,0,0)
         call pwrit (0.03,0.25,lab2,5,13,90,0)
c

      call set (0.1,0.6,0.1,0.6,0.0,xn,0.,xn,1)
c     draw the 45 deg.line.
      call dashdc ('$''$',15,15)
      call lined (0.0,0.0,xn,xn)
      call dashdc ('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$',15,15)
      ivalmx = xmax + 0.5
      xmax = ivalmx
      jvalmx = ymax + 0.5
      ymax = jvalmx
      ux1 = 0.
      uy1 = 0.
      ux2 = xmax
      uy2 = ymax
      if(ux2.lt.uy2) uy2 = ux2
      if(uy2.lt.ux2) ux2 = uy2
c     draw the reg.line.
c      print *,'ux1,ux2,uy1,uy2=',ux1,',',ux2,',',uy1,',',uy2
      call set (0.1,0.6,0.1,0.6,0.0,xmax,0.0,ymax,1)
      call labmod ('(f3.0)','(f3.0)',3,3,0,0,0,0,0)
      call periml (ivalmx,2,jvalmx,2)
      call line (ux1,uy1,ux2,uy2)
c
      n = xn
      call plotdt (x,y,n,0,ic,xmax,ymax)
      call frame
c
c 101  continue
c  60  continue
c ............................................................
 999  continue
 775   continue    
      if(np.eq.inp) call CLSGKS
c
      write(6,698) np
 698  format(1h ,'--- END PLOT # ',I4,' ----------------------------')
      return
      end
cc
c
c
c
      subroutine plotdt (x1,x2,n,iflag,ic,xmax,ymax)
c     =========================================
c
c      common/saved/ imonth,ndays
c      common/savcl/ ic1,ic2,ic3,ic4,ic5
      dimension ic(5)
      dimension x1(1),x2(1)
c
      print *,'no. points passed to sub plotdt =',n
      if (iflag.eq.1) then
          call setusv ('II',ic(3))
          call curved (x1,x2,n)
      else
          call setusv ('II',ic(4))
          ich = ichar('.')
c         iflag=0/1 for markers only /connect with a line
          iflag = 0
c          n = 50
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
