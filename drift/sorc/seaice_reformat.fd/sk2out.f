      SUBROUTINE SK2OUT(x0, y0, dir, dist, skpt, npts, time)
!
!     Print out the forecasts for the skiles2 program virtual drift.
!     Bob Grumbine 4 April 1994.
!     Print of Alaskan points brought in to local routine.
!     Bob Grumbine 22 March 1996.
!     added code to write Alaska and global bulletins
!     Larry Burroughs 25 July 1997
!     Larry Burroughs 5 May 1998
!     Larry Burroughs 4 May 2001
!     27 January 2010 Robert Grumbine: coldp and cnewp merged in to sk2out.f file 
!     20 April 2012 Robert Grumbine: add call to kmlout 
!      2 June 2014  Robert Grumbine: change to passing in direction and
!      distance directly, rather than dx, dy.  Support of ensemble
!
      IMPLICIT none
!
      INTEGER npts, skpt(npts)
      REAL x0(npts), y0(npts), dx(npts), dy(npts)
      REAL dir(npts), dist(npts)
      INTEGER time
      CHARACTER(8) DATE
      CHARACTER(90) fname
!
      REAL arcdis, wdir, xp, yp, dxnm, dynm, dummy, kmtonm,rinc(5)
      PARAMETER (kmtonm = 1. /  1.852 )
      REAL xstl, ystl, akrad
      PARAMETER (xstl = -170.0)
      PARAMETER (ystl =  65.0 )
      PARAMETER (akrad = 2500.0)
      CHARACTER(1) line1(21),line2(9),line3(40),line4(26),line5(41),
     1            hdrfxp(23),hdrnfp(38),bln(4),indx(10),old(20),new(36),
     2            line5a(42)
      INTEGER maxnewpts, maxaknewpts
      PARAMETER (maxnewpts   = 100000)
      PARAMETER (maxaknewpts =  20000)
      CHARACTER(1) oldpts(207,20),newpts(maxnewpts,36),akoldp(160,20),
     1     km,jn,Q, aknewp(maxaknewpts,36),lf,cr,eom,blk,B,C,D,E,F,G,H,
     2     jI,jJ,jk,A, jL,O,P,R,S,X,W,glbmsg(1280,31),akmsg(1280,8),
     3     ldr(40), T,z,aps,copt(17),cnpt(33)
!
      INTEGER k,ii,ij,ik,il,idis,idir,igtot,iputg,iatot,iputa,i,j,
     1        iy,im,id,ih,idate,jy,jm,jd,jh,jdate,left,inc1,inc2,
     2        inc3,next,ix0(npts),iy0(npts),idat(8),jdat(8),jc
!
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
!
      IF (MOD(time,24) .NE. 0 ) RETURN
      CALL W3UTCDAT(idat)
      rinc(1)=0.
      rinc(2)=time
      rinc(3)=0.
      rinc(4)=0.
      rinc(5)=0.
      CALL W3MOVDAT(rinc,idat,jdat)
!
      read(91,6000)aps
 6000 format(a1)
      DO i=1,31
         DO j=1,1280
            glbmsg(j,i)=blk
         ENDDO
      ENDDO
      DO i=1,8
         DO j=1,1280
            akmsg(j,i)=blk
         ENDDO
      ENDDO
      DO i=1,40
         ldr(1)=aps
         ldr(2)=indx(1)
         IF (i.gt.2.and.i.lt.8)ldr(i)=indx(10)
         IF (i.ge.8)ldr(i)=blk
      ENDDO

      bln(1)=blk
      bln(2)=cr
      bln(3)=cr
      bln(4)=lf
      DO i=1,20
         DO j=1,207
            IF (i.lt.18) THEN
               IF (j.gt.160)go to 8000
               akoldp(j,i)=blk
 8000          oldpts(j,i)=blk
            ELSE
               IF (j.gt.160)go to 8010
               akoldp(j,18)=cr
               akoldp(j,19)=cr
               akoldp(j,20)=lf
 8010          oldpts(j,18)=cr
               oldpts(j,19)=cr
               oldpts(j,20)=lf
            ENDIF 
         ENDDO
      ENDDO
      DO i=1,36
         DO j=1,maxnewpts
            IF (i.lt.34) THEN
               IF (j.gt.maxaknewpts)go to 8020
               aknewp(j,i)=blk
 8020          newpts(j,i)=blk
            ELSE
               IF (j.gt.maxaknewpts)go to 8030
               aknewp(j,34)=cr
               aknewp(j,35)=cr
               aknewp(j,36)=lf
 8030          newpts(j,34)=cr
               newpts(j,35)=cr
               newpts(j,36)=lf
            ENDIF 
         ENDDO
      ENDDO
!
      WRITE (60,9002) time
      WRITE (61,9002) time
      WRITE (62,9002) time
 9002 FORMAT (I3,'-Hour Forecast ice drift')
      WRITE (60,9003)
      WRITE (61,9003)
      WRITE (62,9003)
 9003 FORMAT ('Atmosphere only driving')
      CALL date_and_time(DATE=DATE)
      WRITE (60,9004) DATE
      WRITE (61,9004) DATE
      WRITE (62,9004) DATE
 9004 FORMAT ('          Day Zero = ',A8)
      WRITE (60,9005) 
      WRITE (61,9005) 
      WRITE (62,9005) 
!
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
        IF (x0(k).gt.180.) THEN
           ix0(k)=10*(360.-x0(k))
        ELSEIF(x0(k).lt.180.) THEN
           ix0(k)=10.*x0(k)
        ELSEIF(x0(k).eq.180.) THEN
           ix0(k)=180
        ELSE
           ix0(k)=0
        ENDIF
        iy0(k)=10.*y0(k)
!
!       BG output file
!
        !old dir(k) = wdir  (-dxnm, -dynm, dummy)
        !old dist(k) =  kmtonm * arcdis(x0(k),y0(k),xp,yp)
        WRITE (60,9010) skpt(k), x0(k), y0(k),
     2  dir(k), dist(k)
! old 2  wdir  (-dxnm, -dynm, dummy),
! old 3  kmtonm * arcdis(x0(k),y0(k),xp,yp)
!
!       Operational output file
!
        IF(k .LE. 207) THEN 
!old WRITE (61,9012) skpt(k), wdir(-dxnm, -dynm, dummy),
!old 1                     kmtonm * arcdis(x0(k),y0(k),xp,yp)
           WRITE (61,9012) skpt(k), dir(k), dist(k)
           idir=dir(k) + 0.5 !old wdir(-dxnm,-dynm,dummy)+0.5
           idis=10.*dist(k)+0.5 !old *(kmtonm*arcdis(x0(k),y0(k),xp,yp)+0.05)

           CALL coldp(skpt(k),idir,idis,copt)
           DO i=1,17
              oldpts(ii,i)=copt(i)
           ENDDO
           ii=ii+1
           IF(arcdis(xstl, ystl, x0(k), y0(k) ) .LE. akrad) THEN
!old              WRITE (62,9012) skpt(k), wdir(-dxnm, -dynm, dummy),
!old     1        kmtonm * arcdis(x0(k),y0(k),xp,yp)
              WRITE (62,9012) skpt(k), dir(k), dist(k)
              idir=dir(k)+0.5 !old wdir(-dxnm,-dynm,dummy)+0.5
              idis=10.*dist(k)+0.5 !old 10.*(kmtonm*arcdis(x0(k),y0(k),xp,yp)+0.05)
              CALL coldp(skpt(k),idir,idis,copt)
              DO i=1,17
                 akoldp(ij,i)=copt(i)
              ENDDO
              ij=ij+1
           ENDIF
!
        ELSE
           IF (k .EQ. 208) WRITE (61,9001)
           IF (k .EQ. 208) WRITE (62,9001)
!old           WRITE (61,9010) skpt(k), x0(k), y0(k),
!old     1     wdir  (-dxnm, -dynm, dummy),
!old     2     kmtonm * arcdis(x0(k),y0(k),xp,yp)
           WRITE (61,9010) skpt(k), x0(k), y0(k),
     1     dir(k), dist(k)

           idir=dir(k)+0.5 !old wdir(-dxnm,-dynm,dummy)+0.5
           idis=10.*dist(k)+0.5 !old 10.*(kmtonm*arcdis(x0(k),y0(k),xp,yp)+0.05)

           CALL cnewp(skpt(k),ix0(k),iy0(k),idir,idis,cnpt)
           DO i=1,33
              newpts(ik,i)=cnpt(i)
           ENDDO
           ik=ik+1
           IF(arcdis(xstl, ystl, x0(k), y0(k) ) .LE. akrad) THEN
              WRITE (62,9010) skpt(k), x0(k), y0(k),
!old     1        wdir  (-dxnm, -dynm, dummy),
!old     2        kmtonm * arcdis(x0(k),y0(k),xp,yp)
     1        dir(k), dist(k)

              idir=dir(k)+0.5 !old wdir(-dxnm,-dynm,dummy)+0.5
              idis=10.*dist(k)+0.5 !old 10.*(kmtonm*arcdis(x0(k),y0(k),xp,yp)+0.05)
              CALL cnewp(skpt(k),ix0(k),iy0(k),idir,idis,cnpt)
              DO i=1,33
                 aknewp(il,i)=cnpt(i)
              ENDDO
              il=il+1
           ENDIF
        ENDIF
 1000 CONTINUE
!
      WRITE (60,9005) 
      WRITE (61,9005) 
      WRITE (62,9005) 
      WRITE (60,9005) 
      WRITE (61,9005) 
      WRITE (62,9005) 
!
 9001 FORMAT ('Point ',' Initial location ',
     1    '   Dir  Dist(nm)')
 9011 FORMAT ('Point ','  Dir   Dist(nm)')
!
 9010 FORMAT (I5,3X,2F8.3,2X,2F6.1)
 9012 FORMAT (I5,3X,2F6.1)
 9005 FORMAT (' ')

!
!      Now have dir and dist filled, and other output written.
!      Write out the kml file:
       CALL kmlout(x0, y0, npts, dir, dist, time, idat, 70)
!
!        Create tran files for OSO.  Each forecast time is a separate
!          part of each bulletin with its own header lines which are
!          the same as the first part with the exception of the label
!          and the verification time (VT).
!
!        Create global tran file
!
      read(90,7000)iy,im,id
 7000 format(i4,i2,i2)
      ih=00
      IF (time.lt.100) THEN
         igtot=40+21+9+40+26+41+23+4+(ii-1)*20+4+38+4+(ik-1)*36+1
         iputg=igtot/1280
         left=mod(igtot,1280)
         IF (left.gt.0)iputg=iputg+1
!D         write(06,7100)igtot,iputg
 7100    format(' igtot=',i5,' iputg=',i2/)
      ELSE
         igtot=40+21+9+40+26+42+23+4+(ii-1)*20+4+38+4+(ik-1)*36+1
         iputg=igtot/1280
         left=mod(igtot,1280)
         IF (left.gt.0)iputg=iputg+1
!D         write(06,7100)igtot,iputg
      ENDIF
      next=0
      CALL W3AI19(ldr,40,glbmsg,igtot,next)

      line1( 3)=x
      line1( 4)=x
      inc1=id/10
      inc2=id-10*inc1
      IF (inc1.eq.0)inc1=10
      IF (inc2.eq.0)inc2=10
      line1(13)=indx(inc1)
      line1(14)=indx(inc2)
      CALL W3AI19(line1,21,glbmsg,igtot,next)

      line2(4)=p
      line2(5)=jl
      line2(6)=r
      CALL W3AI19(line2, 9,glbmsg,igtot,next)

      line3(10)=G
      line3(11)=jL
      line3(12)=O
      line3(13)=B
      line3(14)=A
      line3(15)=jL
      IF (time.eq. 24)line3(8)=A
      IF (time.eq. 48)line3(8)=B
      IF (time.eq. 72)line3(8)=C
      IF (time.eq. 96)line3(8)=D
      IF (time.eq.120)line3(8)=E
      IF (time.eq.144)line3(8)=F
      IF (time.eq.168)line3(8)=G
      IF (time.eq.192)line3(8)=H
      IF (time.eq.216)line3(8)=jI
      IF (time.eq.240)line3(8)=jj
      IF (time.eq.264)line3(8)=jk
      IF (time.eq.288)line3(8)=jL
      IF (time.eq.312)line3(8)=km
      IF (time.eq.336)line3(8)=jn
      IF (time.eq.360)line3(8)=O
      IF (time.eq.384)line3(7)=z
      IF (time.eq.384)line3(8)=P
      CALL W3AI19(line3,40,glbmsg,igtot,next)

      CALL W3AI19(line4,26,glbmsg,igtot,next)

      IF (time.lt.100) THEN
         inc1=time/10
         inc2=time-10*inc1
         IF (inc1.eq.0)inc1=10
         IF (inc2.eq.0)inc2=10
         line5(1)=indx(inc1)
         line5(2)=indx(inc2)
         jc=jdat(1)/100
         jy=jdat(1)-100*jc
         jm=jdat(2)
         jd=jdat(3)
         jh=jdat(5)
         inc1=jm/10
         inc2=jm-10*inc1
         IF (inc1.eq.0)inc1=10
         IF (inc2.eq.0)inc2=10
         line5(18)=indx(inc1)
         line5(19)=indx(inc2)
         inc1=jd/10
         inc2=jd-10*inc1
         IF (inc1.eq.0)inc1=10
         IF (inc2.eq.0)inc2=10
         line5(21)=indx(inc1)
         line5(22)=indx(inc2)
         inc1=jy/10
         inc2=jy-10*inc1
         IF (inc1.eq.0)inc1=10
         IF (inc2.eq.0)inc2=10
         line5(24)=indx(inc1)
         line5(25)=indx(inc2)
         CALL W3AI19(line5,41,glbmsg,igtot,next)

      ELSE
         inc1= time/100
         inc2=(time-100*inc1)/10
         inc3= time-100*inc1-10*inc2
         IF (inc1.eq.0) THEN
            line5a(3)=blk
            IF (inc2.eq.0)inc2=10
            IF (inc3.eq.0)inc3=10
            line5a(1)=indx(inc2)
            line5a(2)=indx(inc3)
         ELSE
            line5a(1)=indx(inc1)
            IF (inc2.eq.0)inc2=10
            IF (inc3.eq.0)inc3=10
            line5a(2)=indx(inc2)
            line5a(3)=indx(inc3)
         ENDIF
         jc=jdat(1)/100
         jy=jdat(1)-100*jc
         jm=jdat(2)
         jd=jdat(3)
         jh=jdat(5)
         inc1=jm/10
         inc2=jm-10*inc1
         IF (inc1.eq.0)inc1=10
         IF (inc2.eq.0)inc2=10
         line5a(19)=indx(inc1)
         line5a(20)=indx(inc2)
         inc1=jd/10
         inc2=jd-10*inc1
         IF (inc1.eq.0)inc1=10
         IF (inc2.eq.0)inc2=10
         line5a(22)=indx(inc1)
         line5a(23)=indx(inc2)
         inc1=jy/10
         inc2=jy-10*inc1
         IF (inc1.eq.0)inc1=10
         IF (inc2.eq.0)inc2=10
         line5a(25)=indx(inc1)
         line5a(26)=indx(inc2)
         CALL W3AI19(line5a,42,glbmsg,igtot,next)

      ENDIF
      CALL W3AI19(hdrfxp,23,glbmsg,igtot,next)

      CALL W3AI19(bln,4,glbmsg,igtot,next)

      DO i=1,ii-1
         DO j=1,20
            old(j)=oldpts(i,j)
         ENDDO
         CALL W3AI19(old,20,glbmsg,igtot,next)
      ENDDO

      CALL W3AI19(bln,4,glbmsg,igtot,next)
      CALL W3AI19(hdrnfp,38,glbmsg,igtot,next)
      CALL W3AI19(bln,4,glbmsg,igtot,next)

      DO i=1,ik-1
         DO j=1,36
            new(j)=newpts(i,j)
         ENDDO
         CALL W3AI19(new,36,glbmsg,igtot,next)
      ENDDO
      CALL W3AI19(eom,1,glbmsg,igtot,next)

      DO i=1,iputg
         write(63,7300)(glbmsg(j,i),j=1,1280)
 7300    format(1280a1)
!D         print *, (glbmsg(j,i),j=1,1280)
      ENDDO
!
!        Create Alaska tran file
!
!D      write(06,7116)
 7116 format(//)
      IF (time.lt.100) THEN
            iatot=40+21+9+40+26+41+23+4+(ij-1)*20+4+38+4+(il-1)*36+1
            iputa=iatot/1280
            left=mod(iatot,1280)
            IF (left.gt.0)iputa=iputa+1
!D         write(06,7117)iatot,iputa
 7117    format(' iatot=',i5,' iputa=',i2/)
      ELSE
            iatot=40+21+9+40+26+42+23+4+(ij-1)*20+4+38+4+(il-1)*36+1
            iputa=iatot/1280
            left=mod(iatot,1280)
            IF (left.gt.0)iputa=iputa+1
!D         write(06,7117)iatot,iputa
      ENDIF
      next=0
      CALL W3AI19(ldr,40,akmsg,iatot,next)

      line1( 3)=a
      line1( 4)=jk
      CALL W3AI19(line1,21,akmsg,iatot,next)

      line2(4)=a
      line2(5)=jk
      line2(6)=w
      CALL W3AI19(line2, 9,akmsg,iatot,next)

      line3(10)=A
      line3(11)=jL
      line3(12)=A
      line3(13)=S
      line3(14)=jK
      line3(15)=A
      CALL W3AI19(line3,40,akmsg,iatot,next)
      CALL W3AI19(line4,26,akmsg,iatot,next)

      IF (time.lt.100) THEN
         CALL W3AI19(line5,41,akmsg,iatot,next)
      ELSE
         CALL W3AI19(line5a,42,akmsg,iatot,next)
      ENDIF

      CALL W3AI19(hdrfxp,23,akmsg,iatot,next)
      CALL W3AI19(bln,4,akmsg,iatot,next)

      DO i=1,ij-1
         DO j=1,20
            old(j)=akoldp(i,j)
         ENDDO
         CALL W3AI19(old,20,akmsg,iatot,next)
      ENDDO

      CALL W3AI19(bln,4,akmsg,iatot,next)
      CALL W3AI19(hdrnfp,38,akmsg,iatot,next)
      CALL W3AI19(bln,4,akmsg,iatot,next)
      DO i=1,il-1
         DO j=1,36
            new(j)=aknewp(i,j)
         ENDDO
         CALL W3AI19(new,36,akmsg,iatot,next)
      ENDDO
      CALL W3AI19(eom,1,akmsg,iatot,next)
      DO i=1,iputa
!        write(64)(akmsg(j,i),j=1,1280)
         write(64,7300)(akmsg(j,i),j=1,1280)
!D         print *, (akmsg(j,i),j=1,1280)
      ENDDO
!D      write(06,7116)
      rewind 90
      rewind 91
      RETURN
      END
!
      SUBROUTINE coldp(ipt,idir,idis,copt)
!
!         This subroutine converts point number and direction and
!           distance of ice drift from integer to character and
!           fills a multipoint character array with data for the
!           sea ice drift bulletins.
!
!         19970821 - created by Larry Burroughs
!
      IMPLICIT none
!
      INTEGER i,ipt,idir,idis,incl1,incl2,incl3,incl4
      CHARACTER(1) copt(17),indx(10),pt,blnk
!
      DATA indx/'1','2','3','4','5','6','7','8','9','0'/,pt/'.'/,
     1     blnk/' '/
!
!         Convert point number from integer to character, and
!           blank out leading zeros
!
      DO i=1,17
         copt(i)=blnk
      ENDDO
      incl1= ipt/1000
      incl2=(ipt-1000*incl1)/100
      incl3=(ipt-1000*incl1-100*incl2)/10
      incl4= ipt-1000*incl1-100*incl2-10*incl3
      IF (incl1.eq.0) THEN
         copt( 2)=blnk
      ELSE
         copt( 2)=indx(incl1)
      ENDIF
      IF (incl1.eq.0.and.incl2.eq.0) THEN
         copt( 3)=blnk
      ELSE
         IF (incl2.eq.0)incl2=10
         copt( 3)=indx(incl2)
      ENDIF
      IF (incl1.eq.0.and.incl2.eq.0.and.incl3.eq.0) THEN
         copt( 4)=blnk
      ELSE
         IF (incl3.eq.0)incl3=10
         copt( 4)=indx(incl3)
      ENDIF
      IF (incl4.eq.0)incl4=10
      copt( 5)=indx(incl4)
!
!         Convert direction of ice growth from integer to character,
!           and blank out leading zeros.
!
      incl1= idir/100
      incl2=(idir-100*incl1)/10
      incl3= idir-100*incl1-10*incl2
      IF (incl1.eq.0) THEN
         copt( 8)=blnk
      ELSE
         copt( 8)=indx(incl1)
      ENDIF
      IF (incl1.eq.0.and.incl2.eq.0) THEN
         copt( 9)=blnk
      ELSE
         IF (incl2.eq.0)incl2=10
         copt( 9)=indx(incl2)
      ENDIF
      IF (incl3.eq.0)incl3=10
      copt(10)=indx(incl3)
!
!         Convert distance of ice growth from integer to character,
!           and blank out leading zeros.
!
      incl1= idis/1000
      incl2=(idis-1000*incl1)/100
      incl3=(idis-1000*incl1-100*incl2)/10
      incl4= idis-1000*incl1-100*incl2-10*incl3
      IF (incl1.eq.0) THEN
         copt(12)=blnk
      ELSE
         copt(12)=indx(incl1)
      ENDIF
      IF (incl1.eq.0.and.incl2.eq.0) THEN
         copt(13)=blnk
      ELSE
         IF (incl2.eq.0)incl2=10
         copt(13)=indx(incl2)
      ENDIF
      IF (incl3.eq.0)incl3=10
      copt(14)=indx(incl3)
      copt(15)=pt
      IF (incl4.eq.0)incl4=10
      copt(16)=indx(incl4)
      RETURN
      END
!
      SUBROUTINE cnewp(ipt,ix,iy,idir,idis,cnpt)
!
!         This subroutine converts point number, initial latitude and
!           longitude of the ice point, and direction and distance
!           of ice drift from integer to character.  It also fills a
!           multipoint character array with data for the sea ice
!           drift bulletins.
!
!         19970821 - created by Larry Burroughs
!
      IMPLICIT none
!
      INTEGER i,ipt,ix,iy,idir,idis,in,incl1,incl2,incl3,incl4
      CHARACTER(1) cnpt(33),indx(10),pt,blnk,n,s,e,w
!
      DATA indx/'1','2','3','4','5','6','7','8','9','0'/,pt/'.'/,
     1     blnk/' '/,n/'N'/,s/'S'/,e/'E'/,w/'W'/
!
!         Convert point number from integer to character, and
!           blank out leading zeros
!
      DO i=1,33
         cnpt(i)=blnk
      ENDDO
      incl1= ipt/1000
      incl2=(ipt-1000*incl1)/100
      incl3=(ipt-1000*incl1-100*incl2)/10
      incl4= ipt-1000*incl1-100*incl2-10*incl3
      IF (incl1.eq.0) THEN
         cnpt( 2)=blnk
      ELSE
         cnpt( 2)=indx(incl1)
      ENDIF
      IF (incl1.eq.0.and.incl2.eq.0) THEN
         cnpt( 3)=blnk
      ELSE
         IF (incl2.eq.0)incl2=10
         cnpt( 3)=indx(incl2)
      ENDIF
      IF (incl1.eq.0.and.incl2.eq.0.and.incl3.eq.0) THEN
         cnpt( 4)=blnk
      ELSE
         IF (incl3.eq.0)incl3=10
         cnpt( 4)=indx(incl3)
      ENDIF
      IF (incl4.eq.0)incl4=10
      cnpt( 5)=indx(incl4)
!
!         Convert latitude from integer to character, and blank out
!           leading zeros
!
      incl1= abs(iy)/100
      incl2=(abs(iy)-100*incl1)/10
      incl3= abs(iy)-100*incl1-10*incl2
      cnpt( 9)=indx(incl1)
      IF (incl2.eq.0)incl2=10
      cnpt(10)=indx(incl2)
      IF (incl3.eq.0)incl3=10
      cnpt(11)=pt
      cnpt(12)=indx(incl3)
      IF (iy.lt.0) THEN
         cnpt(13)=s
      ELSEIF(iy.eq.0) THEN
         cnpt(13)=blnk
      ELSE
         cnpt(13)=n
      ENDIF
!
!         Convert longitude from integer to character, and blank
!           out leading zeros
!
      incl1= abs(ix)/1000
      incl2=(abs(ix)-1000*incl1)/100
      incl3=(abs(ix)-1000*incl1-100*incl2)/10
      incl4= abs(ix)-1000*incl1-100*incl2-10*incl3
      IF (incl1.eq.0) THEN
         cnpt(16)=blnk
      ELSE
         cnpt(16)=indx(incl1)
      ENDIF
      IF (incl1.eq.0.and.incl2.eq.0) THEN
         cnpt(17)=blnk
      ELSE
         IF (incl2.eq.0)incl2=10
         cnpt(17)=indx(incl2)
      ENDIF
      IF (incl3.eq.0)incl3=10
      cnpt(18)=indx(incl3)
      cnpt(19)=pt
      IF (incl4.eq.0)incl4=10
      cnpt(20)=indx(incl4)
      IF (iy.gt.-180.and.iy.lt.0) THEN
         cnpt(21)=e
      ELSEIF(iy.eq.0.or.abs(iy).eq.180) THEN
         cnpt(21)=blnk
      ELSE
         cnpt(21)=w
      ENDIF
!
!         Convert direction of ice growth from integer to character,
!           and blank out leading zeros.
!
      incl1= idir/100
      incl2=(idir-100*incl1)/10
      incl3= idir-100*incl1-10*incl2
      IF (incl1.eq.0) THEN
         cnpt(24)=blnk
      ELSE
         cnpt(24)=indx(incl1)
      ENDIF
      IF (incl1.eq.0.and.incl2.eq.0) THEN
         cnpt(25)=blnk
      ELSE
         IF (incl2.eq.0)incl2=10
         cnpt(25)=indx(incl2)
      ENDIF
      IF (incl3.eq.0)incl3=10
      cnpt(26)=indx(incl3)
!
!         Convert distance of ice growth from integer to character,
!           and blank out leading zeros.
!
      incl1= idis/1000
      incl2=(idis-1000*incl1)/100
      incl3=(idis-1000*incl1-100*incl2)/10
      incl4= idis-1000*incl1-100*incl2-10*incl3
      IF (incl1.eq.0) THEN
         cnpt(29)=blnk
      ELSE
         cnpt(29)=indx(incl1)
      ENDIF
      IF (incl1.eq.0.and.incl2.eq.0) THEN
         cnpt(30)=blnk
      ELSE
         IF (incl2.eq.0)incl2=10
         cnpt(30)=indx(incl2)
      ENDIF
      IF (incl3.eq.0)incl3=10
      cnpt(31)=indx(incl3)
      cnpt(32)=pt
      IF (incl4.eq.0)incl4=10
      cnpt(33)=indx(incl4)
      RETURN
      END
