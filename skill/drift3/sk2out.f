      SUBROUTINE SK2OUT(x0, y0, dx, dy, skpt, npts, time)
c
C     Print out the forecasts for the skiles2 program virtual drift.
C     Bob Grumbine 4 April 1994.
C     Print of Alaskan points brought in to local routine.
C     Bob Grumbine 22 March 1996.
c     added code to write Alaska and global bulletins
c     Larry Burroughs 25 July 1997
c     Larry Burroughs 5 May 1998
c     Larry Burroughs 4 May 2001
C     27 January 2010 Robert Grumbine: coldp and cnewp merged in to sk2out.f file 
C     20 April 2012 Robert Grumbine: add call to kmlout 
c
      IMPLICIT none
c
      INTEGER npts, skpt(npts)
      REAL x0(npts), y0(npts), dx(npts), dy(npts)
      REAL dir(npts), dist(npts)
      INTEGER time
      CHARACTER*8 DATE
      CHARACTER*90 fname
c
      REAL arcdis, wdir, xp, yp, dxnm, dynm, dummy, kmtonm,rinc(5)
      PARAMETER (kmtonm = 1. /  1.852 )
      REAL xstl, ystl, akrad
      PARAMETER (xstl = -170.0)
      PARAMETER (ystl =  65.0 )
      PARAMETER (akrad = 2500.0)
      CHARACTER*1 line1(21),line2(9),line3(40),line4(26),line5(41),
     1            hdrfxp(23),hdrnfp(38),bln(4),indx(10),old(20),new(36),
     2            line5a(42)
      INTEGER maxnewpts, maxaknewpts
      PARAMETER (maxnewpts   = 10000)
      PARAMETER (maxaknewpts =  2000)
      CHARACTER*1 oldpts(207,20),newpts(maxnewpts,36),akoldp(160,20),
     1     km,jn,Q, aknewp(maxaknewpts,36),lf,cr,eom,blk,B,C,D,E,F,G,H,
     2     jI,jJ,jk,A, jL,O,P,R,S,X,W,glbmsg(1280,31),akmsg(1280,8),
     3     ldr(40), T,z,aps,copt(17),cnpt(33)
c
      INTEGER k,ii,ij,ik,il,idis,idir,igtot,iputg,iatot,iputa,i,j,
     1        iy,im,id,ih,idate,jy,jm,jd,jh,jdate,left,inc1,inc2,
     2        inc3,next,ix0(npts),iy0(npts),idat(8),jdat(8),jc 
c
      DATA line1/'F','Z','A','K','4','1',' ','K','W','N','O',' ',
     1           '0','0','0','0','0','0','<','<','@'/
      DATA line2/'I','D','M','A','K','W','<','<','@'/
      DATA line3/'P','A','R','T',' ','P','A','A',' ','A','L','A','S',
     1           'K','A',' ','S','E','A',' ','I','C','E',' ',
     2           'D','R','I','F','T',' ','V','E','C','T','O','R',
     3           'S','<','<','@'/
      DATA LINE4/'A','M','O','S','P','H','E','R','I','C',' ','D',
     1           'R','I','V','I','N','G',' ','O','N','L','Y','<',
     2           '<','@'/
      DATA LINE5/'0','0','H','R',' ','F','O','R','E','C','A','S',
     1           'T',' ','V','T',' ','0','0','/','0','0','/','0',
     2           '0',' ','0','0','0','0',' ','U','T','C','<','<',
     3           '@',' ','<','<','@'/
      DATA LINE5a/'0','0','0','H','R',' ','F','O','R','E','C','A','S',
     1           'T',' ','V','T',' ','0','0','/','0','0','/','0',
     2           '0',' ','0','0','0','0',' ','U','T','C','<','<',
     3           '@',' ','<','<','@'/
      DATA HDRFXP/'P','O','I','N','T',' ',' ','D','I','R',' ',' ',
     1            'D','I','S','T','(','N','M',')','<','<','@'/
      DATA HDRNFP/'P','O','I','N','T',' ','I','N','I','T','I','A',
     1            'L',' ','L','O','C','A','T','I','O','N',' ','D',
     2            'I','R',' ','D','I','S','T','(','N','M',')','<',
     3            '<','@'/
      DATA CR/'<'/,LF/'@'/,EOM/'%'/,BLK/' '/,B/'B'/,C/'C'/,D/'D'/,
     1     E/'E'/,F/'F'/,G/'G'/,H/'H'/,jI/'I'/,jJ/'J'/,A/'A'/,
     2     jL/'L'/,O/'O'/,P/'P'/,R/'R'/,X/'X'/,W/'W'/,jk/'K'/,
     3     S/'S'/,km/'M'/,jn/'N'/,Q/'Q'/,T/'T'/,z/'Z'/
      DATA indx/'1','2','3','4','5','6','7','8','9','0'/
c
      DO i = 1, npts
        WRITE (*,*) 'debug ', x0(i), y0(i), dx(i), dy(i)
      ENDDO

      IF (MOD(time,24) .NE. 0 ) RETURN
      CALL W3UTCDAT(idat)
      rinc(1)=0.
      rinc(2)=time
      rinc(3)=0.
      rinc(4)=0.
      rinc(5)=0.
      CALL W3MOVDAT(rinc,idat,jdat)
c
      read(91,6000)aps
 6000 format(a1)
      do i=1,31
         do j=1,1280
            glbmsg(j,i)=blk
         enddo
      enddo
      do i=1,8
         do j=1,1280
            akmsg(j,i)=blk
         enddo
      enddo
      do i=1,40
         ldr(1)=aps
         ldr(2)=indx(1)
         if(i.gt.2.and.i.lt.8)ldr(i)=indx(10)
         if(i.ge.8)ldr(i)=blk
      enddo
c     write(6,6010)ldr
c6010 format(40a1)
      bln(1)=blk
      bln(2)=cr
      bln(3)=cr
      bln(4)=lf
      do i=1,20
         do j=1,207
            if(i.lt.18)then
               if(j.gt.160)go to 8000
               akoldp(j,i)=blk
 8000          oldpts(j,i)=blk
            else
               if(j.gt.160)go to 8010
               akoldp(j,18)=cr
               akoldp(j,19)=cr
               akoldp(j,20)=lf
 8010          oldpts(j,18)=cr
               oldpts(j,19)=cr
               oldpts(j,20)=lf
            endif 
         enddo
      enddo
      do i=1,36
         do j=1,maxnewpts
            if(i.lt.34)then
               if(j.gt.maxaknewpts)go to 8020
               aknewp(j,i)=blk
 8020          newpts(j,i)=blk
            else
               if(j.gt.maxaknewpts)go to 8030
               aknewp(j,34)=cr
               aknewp(j,35)=cr
               aknewp(j,36)=lf
 8030          newpts(j,34)=cr
               newpts(j,35)=cr
               newpts(j,36)=lf
            endif 
         enddo
      enddo
c
      WRITE (60,9002) time
      WRITE (61,9002) time
      WRITE (62,9002) time
 9002 FORMAT (I3,'-Hour Forecast ice drift')
      WRITE (60,9003)
      WRITE (61,9003)
      WRITE (62,9003)
 9003 FORMAT ('Atmosphere only driving')
      WRITE (60,9004) DATE()
      WRITE (61,9004) DATE()
      WRITE (62,9004) DATE()
 9004 FORMAT ('          Day Zero = ',A8)
      WRITE (60,9005) 
      WRITE (61,9005) 
      WRITE (62,9005) 
c
      WRITE (60,9001) 
      WRITE (61,9011) 
      WRITE (62,9011) 
      ii=1
      ij=1
      ik=1
      il=1
      DO 1000 k = 1, npts
        xp = x0(k)+dx(k)
        yp = y0(k)+dy(k)
        dxnm = kmtonm * arcdis(x0(k), y0(k), xp, y0(k))
        dynm = kmtonm * arcdis(x0(k), y0(k), x0(k), yp)
        dxnm = SIGN(dxnm, x0(k)-xp)
        dynm = SIGN(dynm, y0(k)-yp)
        if(x0(k).gt.180.)then
           ix0(k)=10*(360.-x0(k))
        elseif(x0(k).lt.180.)then
           ix0(k)=10.*x0(k)
        elseif(x0(k).eq.180.)then
           ix0(k)=180
        else
           ix0(k)=0
        endif
        iy0(k)=10.*y0(k)
c
C       BG output file
c
        dir(k) = wdir  (-dxnm, -dynm, dummy)
        dist(k) =  kmtonm * arcdis(x0(k),y0(k),xp,yp)
        WRITE (60,9010) skpt(k), x0(k), y0(k),
     3  wdir  (-dxnm, -dynm, dummy),
     2  kmtonm * arcdis(x0(k),y0(k),xp,yp)
c
C       Operational output file
c
        IF(k .LE. 207) THEN 
           WRITE (61,9012) skpt(k), wdir(-dxnm, -dynm, dummy),
     1                     kmtonm * arcdis(x0(k),y0(k),xp,yp)
           idir=wdir(-dxnm,-dynm,dummy)+0.5
           idis=10.*(kmtonm*arcdis(x0(k),y0(k),xp,yp)+0.05)
           CALL coldp(skpt(k),idir,idis,copt)
           do i=1,17
              oldpts(ii,i)=copt(i)
           enddo
           ii=ii+1
           IF(arcdis(xstl, ystl, x0(k), y0(k) ) .LE. akrad) THEN
              WRITE (62,9012) skpt(k), wdir(-dxnm, -dynm, dummy),
     1        kmtonm * arcdis(x0(k),y0(k),xp,yp)
              idir=wdir(-dxnm,-dynm,dummy)+0.5
              idis=10.*(kmtonm*arcdis(x0(k),y0(k),xp,yp)+0.05)
              CALL coldp(skpt(k),idir,idis,copt)
              do i=1,17
                 akoldp(ij,i)=copt(i)
              enddo
              ij=ij+1
           ENDIF
c
        ELSE
           IF (k .EQ. 208) WRITE (61,9001)
           IF (k .EQ. 208) WRITE (62,9001)
           WRITE (61,9010) skpt(k), x0(k), y0(k),
     1     wdir  (-dxnm, -dynm, dummy),
     2     kmtonm * arcdis(x0(k),y0(k),xp,yp)
           idir=wdir(-dxnm,-dynm,dummy)+0.5
           idis=10.*(kmtonm*arcdis(x0(k),y0(k),xp,yp)+0.05)
           CALL cnewp(skpt(k),ix0(k),iy0(k),idir,idis,cnpt)
           do i=1,33
              newpts(ik,i)=cnpt(i)
           enddo
           ik=ik+1
           IF(arcdis(xstl, ystl, x0(k), y0(k) ) .LE. akrad) THEN
              WRITE (62,9010) skpt(k), x0(k), y0(k),
     1        wdir  (-dxnm, -dynm, dummy),
     2        kmtonm * arcdis(x0(k),y0(k),xp,yp)
              idir=wdir(-dxnm,-dynm,dummy)+0.5
              idis=10.*(kmtonm*arcdis(x0(k),y0(k),xp,yp)+0.05)
              CALL cnewp(skpt(k),ix0(k),iy0(k),idir,idis,cnpt)
              do i=1,33
                 aknewp(il,i)=cnpt(i)
              enddo
              il=il+1
c          else
c             il=999
           ENDIF
        ENDIF
 1000 CONTINUE
c
      WRITE (60,9005) 
      WRITE (61,9005) 
      WRITE (62,9005) 
      WRITE (60,9005) 
      WRITE (61,9005) 
      WRITE (62,9005) 
c
 9001 FORMAT ('Point ',' Initial location ',
     1    '   Dir  Dist(nm)')
 9011 FORMAT ('Point ','  Dir   Dist(nm)')
c
 9010 FORMAT (I4,3X,2F8.3,2X,2F6.1)
 9012 FORMAT (I4,3X,2F6.1)
 9005 FORMAT (' ')

C
C
C      Now have dir and dist filled, and other output written.
C      Write out the kml file:
       CALL kmlout(x0, y0, npts, dir, dist, time, idat, 70)
!      SUBROUTINE kmlout(lat, lon, npts, dir, dist, hours, date, unit)
c
c        Create tran files for OSO.  Each forecast time is a separate
c          part of each bulletin with its own header lines which are
c          the same as the first part with the exception of the label
c          and the verification time (VT).
c
c        Create global tran file
c
      read(90,7000)iy,im,id
 7000 format(i4,i2,i2)
      ih=00
      if(time.lt.100)then
         igtot=40+21+9+40+26+41+23+4+(ii-1)*20+4+38+4+(ik-1)*36+1
         iputg=igtot/1280
         left=mod(igtot,1280)
         if(left.gt.0)iputg=iputg+1
CD         write(06,7100)igtot,iputg
 7100    format(' igtot=',i5,' iputg=',i2/)
      else
         igtot=40+21+9+40+26+42+23+4+(ii-1)*20+4+38+4+(ik-1)*36+1
         iputg=igtot/1280
         left=mod(igtot,1280)
         if(left.gt.0)iputg=iputg+1
CD         write(06,7100)igtot,iputg
      endif
      next=0
      CALL W3AI19(ldr,40,glbmsg,igtot,next)
c     write(06,7101)ldr
c7101 format(' ',40a1)
      line1( 3)=x
      line1( 4)=x
      inc1=id/10
      inc2=id-10*inc1
      if(inc1.eq.0)inc1=10
      if(inc2.eq.0)inc2=10
      line1(13)=indx(inc1)
      line1(14)=indx(inc2)
      CALL W3AI19(line1,21,glbmsg,igtot,next)
c     write(06,7102)line1
c7102 format(' ',21a1)
      line2(4)=p
      line2(5)=jl
      line2(6)=r
      CALL W3AI19(line2, 9,glbmsg,igtot,next)
c     write(06,7103)line2
c7103 format(' ',9a1)
      line3(10)=G
      line3(11)=jL
      line3(12)=O
      line3(13)=B
      line3(14)=A
      line3(15)=jL
      if(time.eq. 24)line3(8)=A
      if(time.eq. 48)line3(8)=B
      if(time.eq. 72)line3(8)=C
      if(time.eq. 96)line3(8)=D
      if(time.eq.120)line3(8)=E
      if(time.eq.144)line3(8)=F
      if(time.eq.168)line3(8)=G
      if(time.eq.192)line3(8)=H
      if(time.eq.216)line3(8)=jI
      if(time.eq.240)line3(8)=jj
      if(time.eq.264)line3(8)=jk
      if(time.eq.288)line3(8)=jL
      if(time.eq.312)line3(8)=km
      if(time.eq.336)line3(8)=jn
      if(time.eq.360)line3(8)=O
      if(time.eq.384)line3(7)=z
      if(time.eq.384)line3(8)=P
      CALL W3AI19(line3,40,glbmsg,igtot,next)
c     write(06,7104)line3
c7104 format(' ',39a1)
      CALL W3AI19(line4,26,glbmsg,igtot,next)
c     write(06,7105)line4
c7105 format(' ',26a1)
      if(time.lt.100)then
         inc1=time/10
         inc2=time-10*inc1
         if(inc1.eq.0)inc1=10
         if(inc2.eq.0)inc2=10
         line5(1)=indx(inc1)
         line5(2)=indx(inc2)
         jc=jdat(1)/100
         jy=jdat(1)-100*jc
         jm=jdat(2)
         jd=jdat(3)
         jh=jdat(5)
         inc1=jm/10
         inc2=jm-10*inc1
         if(inc1.eq.0)inc1=10
         if(inc2.eq.0)inc2=10
         line5(18)=indx(inc1)
         line5(19)=indx(inc2)
         inc1=jd/10
         inc2=jd-10*inc1
         if(inc1.eq.0)inc1=10
         if(inc2.eq.0)inc2=10
         line5(21)=indx(inc1)
         line5(22)=indx(inc2)
         inc1=jy/10
         inc2=jy-10*inc1
         if(inc1.eq.0)inc1=10
         if(inc2.eq.0)inc2=10
         line5(24)=indx(inc1)
         line5(25)=indx(inc2)
         CALL W3AI19(line5,41,glbmsg,igtot,next)
c        write(06,7106)line5
c7106    format(' ',41a1)
      else
         inc1= time/100
         inc2=(time-100*inc1)/10
         inc3= time-100*inc1-10*inc2
         if(inc1.eq.0)then
            line5a(3)=blk
            if(inc2.eq.0)inc2=10
            if(inc3.eq.0)inc3=10
            line5a(1)=indx(inc2)
            line5a(2)=indx(inc3)
         else
            line5a(1)=indx(inc1)
            if(inc2.eq.0)inc2=10
            if(inc3.eq.0)inc3=10
            line5a(2)=indx(inc2)
            line5a(3)=indx(inc3)
         endif
         jc=jdat(1)/100
         jy=jdat(1)-100*jc
         jm=jdat(2)
         jd=jdat(3)
         jh=jdat(5)
         inc1=jm/10
         inc2=jm-10*inc1
         if(inc1.eq.0)inc1=10
         if(inc2.eq.0)inc2=10
         line5a(19)=indx(inc1)
         line5a(20)=indx(inc2)
         inc1=jd/10
         inc2=jd-10*inc1
         if(inc1.eq.0)inc1=10
         if(inc2.eq.0)inc2=10
         line5a(22)=indx(inc1)
         line5a(23)=indx(inc2)
         inc1=jy/10
         inc2=jy-10*inc1
         if(inc1.eq.0)inc1=10
         if(inc2.eq.0)inc2=10
         line5a(25)=indx(inc1)
         line5a(26)=indx(inc2)
         CALL W3AI19(line5a,42,glbmsg,igtot,next)
c        write(06,7118)line5a
c7118    format(' ',42a1)
      endif
      CALL W3AI19(hdrfxp,23,glbmsg,igtot,next)
c     write(06,7107)hdrfxp
c7107 format(' ',23a1)
      CALL W3AI19(bln,4,glbmsg,igtot,next)
c     write(06,7108)bln
c7108 format(' ',4a1)
      do i=1,ii-1
         do j=1,20
            old(j)=oldpts(i,j)
         enddo
         CALL W3AI19(old,20,glbmsg,igtot,next)
c        write(06,7109)old
c7109    format(' ',20a1)
      enddo
      CALL W3AI19(bln,4,glbmsg,igtot,next)
c     write(06,7108)bln
      CALL W3AI19(hdrnfp,38,glbmsg,igtot,next)
c     write(06,7111)hdrnfp
c7111 format(' ',38a1)
      CALL W3AI19(bln,4,glbmsg,igtot,next)
c     write(06,7108)bln
      do i=1,ik-1
         do j=1,36
            new(j)=newpts(i,j)
         enddo
         CALL W3AI19(new,36,glbmsg,igtot,next)
c        write(06,7113)new
c7113    format(' ',36a1)
      enddo
      CALL W3AI19(eom,1,glbmsg,igtot,next)
c     write(06,7114)eom
c7114 format(' ',a1//)
      do i=1,iputg
c        write(63)(glbmsg(j,i),j=1,1280)
         write(63,7300)(glbmsg(j,i),j=1,1280)
 7300    format(1280a1)
c        write(06,7115)(glbmsg(j,i),j=1,1280)
c7115    format(' ',80a1/)
CD         print *, (glbmsg(j,i),j=1,1280)
      enddo
c
c        Create Alaska tran file
c
CD      write(06,7116)
 7116 format(//)
      if(time.lt.100)then
c        if(il.eq.999)then
c           iatot=40+21+9+40+26+41+23+4+(ij-1)*20+1
c           iputa=iatot/1280
c           left=mod(iatot,1280)
c           if(left.gt.0)iputa=iputa+1
c        else
            iatot=40+21+9+40+26+41+23+4+(ij-1)*20+4+38+4+(il-1)*36+1
            iputa=iatot/1280
            left=mod(iatot,1280)
            if(left.gt.0)iputa=iputa+1
c        endif
CD         write(06,7117)iatot,iputa
 7117    format(' iatot=',i5,' iputa=',i2/)
      else
c        if(il.eq.999)then
c           iatot=40+21+9+40+26+42+23+4+(ij-1)*20+1
c           iputa=iatot/1280
c           left=mod(iatot,1280)
c           if(left.gt.0)iputa=iputa+1
c        else
            iatot=40+21+9+40+26+42+23+4+(ij-1)*20+4+38+4+(il-1)*36+1
            iputa=iatot/1280
            left=mod(iatot,1280)
            if(left.gt.0)iputa=iputa+1
c        endif
CD         write(06,7117)iatot,iputa
      endif
      next=0
      CALL W3AI19(ldr,40,akmsg,iatot,next)
c     write(06,7101)ldr
      line1( 3)=a
      line1( 4)=jk
      CALL W3AI19(line1,21,akmsg,iatot,next)
c     write(06,7102)line1
      line2(4)=a
      line2(5)=jk
      line2(6)=w
      CALL W3AI19(line2, 9,akmsg,iatot,next)
c     write(06,7103)line2
      line3(10)=A
      line3(11)=jL
      line3(12)=A
      line3(13)=S
      line3(14)=jK
      line3(15)=A
      CALL W3AI19(line3,40,akmsg,iatot,next)
c     write(06,7104)line3
      CALL W3AI19(line4,26,akmsg,iatot,next)
c     write(06,7105)line4
      if(time.lt.100)then
         CALL W3AI19(line5,41,akmsg,iatot,next)
c        write(06,7106)line5
      else
         CALL W3AI19(line5a,42,akmsg,iatot,next)
c        write(06,7118)line5a
      endif
      CALL W3AI19(hdrfxp,23,akmsg,iatot,next)
c     write(06,7107)hdrfxp
      CALL W3AI19(bln,4,akmsg,iatot,next)
c     write(06,7108)bln
      do i=1,ij-1
         do j=1,20
            old(j)=akoldp(i,j)
         enddo
         CALL W3AI19(old,20,akmsg,iatot,next)
c        write(06,7109)old
      enddo
c     if(il.eq.999)go to 7500
      CALL W3AI19(bln,4,akmsg,iatot,next)
c     write(06,7108)bln
      CALL W3AI19(hdrnfp,38,akmsg,iatot,next)
c     write(06,7111)hdrnfp
      CALL W3AI19(bln,4,akmsg,iatot,next)
c     write(06,7108)bln
      do i=1,il-1
         do j=1,36
            new(j)=aknewp(i,j)
         enddo
         CALL W3AI19(new,36,akmsg,iatot,next)
c        write(06,7113)new
      enddo
 7500 continue
      CALL W3AI19(eom,1,akmsg,iatot,next)
c     write(06,7114)eom
      do i=1,iputa
c        write(64)(akmsg(j,i),j=1,1280)
         write(64,7300)(akmsg(j,i),j=1,1280)
c        write(06,7115)(akmsg(j,i),j=1,1280)
CD         print *, (akmsg(j,i),j=1,1280)
      enddo
CD      write(06,7116)
      rewind 90
      rewind 91
      RETURN
      END
