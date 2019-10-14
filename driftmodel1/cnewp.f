c
      subroutine cnewp(ipt,ix,iy,idir,idis,cnpt)
c
c         This subroutine converts point number, initial latitude and
c           longitude of the ice point, and direction and distance
c           of ice drift from integer to character.  It also fills a
c           multipoint character array with data for the sea ice
c           drift bulletins.
c
c         970821 - created by Larry Burroughs
c
      implicit none
c
      integer i,ipt,ix,iy,idir,idis,in,incl1,incl2,incl3,incl4
      character*1 cnpt(33),indx(10),pt,blnk,n,s,e,w
c
      data indx/'1','2','3','4','5','6','7','8','9','0'/,pt/'.'/,
     1     blnk/' '/,n/'N'/,s/'S'/,e/'E'/,w/'W'/
c
c         Convert point number from integer to character, and
c           blank out leading zeros
c
      do i=1,33
         cnpt(i)=blnk
      enddo
      incl1= ipt/1000
      incl2=(ipt-1000*incl1)/100
      incl3=(ipt-1000*incl1-100*incl2)/10
      incl4= ipt-1000*incl1-100*incl2-10*incl3
      if(incl1.eq.0)then
         cnpt( 2)=blnk
      else
         cnpt( 2)=indx(incl1)
      endif
      if(incl1.eq.0.and.incl2.eq.0)then
         cnpt( 3)=blnk
      else
         if(incl2.eq.0)incl2=10
         cnpt( 3)=indx(incl2)
      endif
      if(incl1.eq.0.and.incl2.eq.0.and.incl3.eq.0)then
         cnpt( 4)=blnk
      else
         if(incl3.eq.0)incl3=10
         cnpt( 4)=indx(incl3)
      endif
      if(incl4.eq.0)incl4=10
      cnpt( 5)=indx(incl4)
c
c         Convert latitude from integer to character, and blank out
c           leading zeros
c
      incl1= abs(iy)/100
      incl2=(abs(iy)-100*incl1)/10
      incl3= abs(iy)-100*incl1-10*incl2
      cnpt( 9)=indx(incl1)
      if(incl2.eq.0)incl2=10
      cnpt(10)=indx(incl2)
      if(incl3.eq.0)incl3=10
      cnpt(11)=pt
      cnpt(12)=indx(incl3)
      if(iy.lt.0)then
         cnpt(13)=s
      elseif(iy.eq.0)then
         cnpt(13)=blnk
      else
         cnpt(13)=n
      endif
c
c         Convert longitude from integer to character, and blank
c           out leading zeros
c
      incl1= abs(ix)/1000
      incl2=(abs(ix)-1000*incl1)/100
      incl3=(abs(ix)-1000*incl1-100*incl2)/10
      incl4= abs(ix)-1000*incl1-100*incl2-10*incl3
      if(incl1.eq.0)then
         cnpt(16)=blnk
      else
         cnpt(16)=indx(incl1)
      endif
      if(incl1.eq.0.and.incl2.eq.0)then
         cnpt(17)=blnk
      else
         if(incl2.eq.0)incl2=10
         cnpt(17)=indx(incl2)
      endif
      if(incl3.eq.0)incl3=10
      cnpt(18)=indx(incl3)
      cnpt(19)=pt
      if(incl4.eq.0)incl4=10
      cnpt(20)=indx(incl4)
      if(iy.gt.-180.and.iy.lt.0)then
         cnpt(21)=e
      elseif(iy.eq.0.or.abs(iy).eq.180)then
         cnpt(21)=blnk
      else
         cnpt(21)=w
      endif
c
c         Convert direction of ice growth from integer to character,
c           and blank out leading zeros.
c
      incl1= idir/100
      incl2=(idir-100*incl1)/10
      incl3= idir-100*incl1-10*incl2
      if(incl1.eq.0)then
         cnpt(24)=blnk
      else
         cnpt(24)=indx(incl1)
      endif
      if(incl1.eq.0.and.incl2.eq.0)then
         cnpt(25)=blnk
      else
         if(incl2.eq.0)incl2=10
         cnpt(25)=indx(incl2)
      endif
      if(incl3.eq.0)incl3=10
      cnpt(26)=indx(incl3)
c
c         Convert distance of ice growth from integer to character,
c           and blank out leading zeros.
c
      incl1= idis/1000
      incl2=(idis-1000*incl1)/100
      incl3=(idis-1000*incl1-100*incl2)/10
      incl4= idis-1000*incl1-100*incl2-10*incl3
      if(incl1.eq.0)then
         cnpt(29)=blnk
      else
         cnpt(29)=indx(incl1)
      endif
      if(incl1.eq.0.and.incl2.eq.0)then
         cnpt(30)=blnk
      else
         if(incl2.eq.0)incl2=10
         cnpt(30)=indx(incl2)
      endif
      if(incl3.eq.0)incl3=10
      cnpt(31)=indx(incl3)
      cnpt(32)=pt
      if(incl4.eq.0)incl4=10
      cnpt(33)=indx(incl4)
      return
      end
