c
      subroutine coldp(ipt,idir,idis,copt)
c
c         This subroutine converts point number and direction and
c           distance of ice drift from integer to character and
c           fills a multipoint character array with data for the
c           sea ice drift bulletins.
c
c         970821 - created by Larry Burroughs
c
      implicit none
c
      integer i,ipt,idir,idis,incl1,incl2,incl3,incl4
      character*1 copt(17),indx(10),pt,blnk
c
      data indx/'1','2','3','4','5','6','7','8','9','0'/,pt/'.'/,
     1     blnk/' '/
c
c         Convert point number from integer to character, and
c           blank out leading zeros
c
      do i=1,17
         copt(i)=blnk
      enddo
      incl1= ipt/1000
      incl2=(ipt-1000*incl1)/100
      incl3=(ipt-1000*incl1-100*incl2)/10
      incl4= ipt-1000*incl1-100*incl2-10*incl3
      if(incl1.eq.0)then
         copt( 2)=blnk
      else
         copt( 2)=indx(incl1)
      endif
      if(incl1.eq.0.and.incl2.eq.0)then
         copt( 3)=blnk
      else
         if(incl2.eq.0)incl2=10
         copt( 3)=indx(incl2)
      endif
      if(incl1.eq.0.and.incl2.eq.0.and.incl3.eq.0)then
         copt( 4)=blnk
      else
         if(incl3.eq.0)incl3=10
         copt( 4)=indx(incl3)
      endif
      if(incl4.eq.0)incl4=10
      copt( 5)=indx(incl4)
c
c         Convert direction of ice growth from integer to character,
c           and blank out leading zeros.
c
      incl1= idir/100
      incl2=(idir-100*incl1)/10
      incl3= idir-100*incl1-10*incl2
      if(incl1.eq.0)then
         copt( 8)=blnk
      else
         copt( 8)=indx(incl1)
      endif
      if(incl1.eq.0.and.incl2.eq.0)then
         copt( 9)=blnk
      else
         if(incl2.eq.0)incl2=10
         copt( 9)=indx(incl2)
      endif
      if(incl3.eq.0)incl3=10
      copt(10)=indx(incl3)
c
c         Convert distance of ice growth from integer to character,
c           and blank out leading zeros.
c
      incl1= idis/1000
      incl2=(idis-1000*incl1)/100
      incl3=(idis-1000*incl1-100*incl2)/10
      incl4= idis-1000*incl1-100*incl2-10*incl3
      if(incl1.eq.0)then
         copt(12)=blnk
      else
         copt(12)=indx(incl1)
      endif
      if(incl1.eq.0.and.incl2.eq.0)then
         copt(13)=blnk
      else
         if(incl2.eq.0)incl2=10
         copt(13)=indx(incl2)
      endif
      if(incl3.eq.0)incl3=10
      copt(14)=indx(incl3)
      copt(15)=pt
      if(incl4.eq.0)incl4=10
      copt(16)=indx(incl4)
      return
      end
